
#pragma once

#include <algorithm>
#include <numeric>
#include <functional>
#include <iterator>
#include <array>
#include <vector>
#include <utility>
#include <tuple>
#include <memory>

#include <iostream>

namespace pure {

using namespace std;

/* 
 * Just  x -> Maybe (Just x) | with a definite value.
 * Nothing -> Maybe Nothing  | with definitely no value.
 * Mightbe pred x -> Maybe x | possible with a value.
 */
template< class T, class M = std::unique_ptr<T> > 
constexpr M Just( T t ) {
    return M( new T(move(t)) );
}

template< class X >
constexpr X* Just( X* x ) { return x; }

//template< class X, class M = std::unique_ptr<X> >
//constexpr M Just( const X& x ) {
//    return M( new X(x) );
//}

template< class T, class M = std::unique_ptr<T> > 
constexpr M Nothing() {
    return M( nullptr );
}

/* maybe b (a->b) (Maybe a) -> b */
template< class R, class F, class P >
constexpr R maybe( R&& nothingVal, F f, const P& m ) {
    return m ? f( *m ) : forward<R>( nothingVal );
}

/* 
 * Just f  * Just x  = Just (f x)
 * _       * _       = Nothing
 * Just x  | _       = Just x
 * Nothing | Just x  = Just x
 * Nothing | Nothing = Nothing
 */
template< class F, class P, 
          class Ret = decltype( declval<F>()(*declval<P>()) ) >
auto operator* ( const F& a, const P& b )
    -> unique_ptr< Ret >
{
    return a and b ? Just( (*a)(*b) ) : Nothing<Ret>();
}

template< class P > P operator| ( P&& a, P&& b ) {
    return a ? forward<P>(a) : forward<P>(b); 
}

/* Either a b : Left a | Right b */
template< class L, class R >
struct Either
{
    typedef L left_type;
    typedef R right_type;

    unique_ptr<left_type> left;
    unique_ptr<right_type> right;

    struct Left { 
        left_type value;
        constexpr Left( left_type value ) : value(move(value)) { }
    };

    struct Right { 
        right_type value;
        constexpr Right( right_type value ) : value(move(value)) { }
    };

    explicit constexpr Either( Left x )  
        : left(  new left_type (move(x.value)) ) { }
    explicit constexpr Either( Right x ) 
        : right( new right_type(move(x.value)) ) { }
};

/* 
 * Left<b>  a -> Either a b
 * Right<a> b -> Either a b
 * Since an Either cannot be constructed without two type arguments, the
 * 'other' type must be explicitly declared when called. It is, for
 * convenience, the first type argument in both.
 */
template< class R, class L, class E = Either<L,R> >
constexpr E Left( L x ) { return E( typename E::Left(move(x)) ); }

template< class L, class R, class E = Either<L,R> >
constexpr E Right( R x ) { return E( typename E::Right(move(x)) ); }

/* either (a->c) (b->c) (Either a b) -> c */
template< class F, class G, class L, class R >
constexpr auto either( F&& f, G&& g, const Either<L,R>& e )
    -> decltype( f(*e.right) )
{
    return e.right ? f(*e.right) : g(*e.left);
}

/*
 * Right f * Right x = Right (f x)
 * _       * _       = Left _
 */
template< class L, class F, class T, 
          class Ret = decltype( declval<F>()(declval<T>()) ) >
constexpr Either<L,Ret> operator* ( const Either<L,F>& a, 
                                    const Either<L,T>& b )
{
    return a.right and b.right ? Right<L>( (*a.right)(*b.right) )
        : a.left ? Left<Ret>( *a.left ) : Left<Ret>( *b.left );
}

/* Translate f g : h(x,xs...) = f( g(x), xs... ) */
template< class F, class G > struct Translate {
    F f;
    G g;

    template< class _F, class _G >
    Translate( _F&& f, _G&& g ) : f(forward<_F>(f)), g(forward<_G>(g)) { }

    template< class X, class ...XS >
    decltype( f(g(declval<X>()),declval<XS>()...) )
    operator() ( X&& x, XS&& ...xs ) {
        return f( g(forward<X>(x)), xs... );
    }
};

template< class F, class G >
Translate<F,G> translate( F&& f, G&& g ) {
    return Translate<F,G>( forward<F>(f), forward<G>(g) );
}

/* Flip f : g(x,y) = f(y,x) */
template< class F > struct Flip {
    F f;

    template< class _F >
    Flip( _F&& f ) : f(forward<_F>(f)) { }

    template< class X, class Y, class ...XS >
    decltype( f(declval<Y>(),declval<X>(),declval<XS>()...) )
    operator() ( X&& x, Y&& y, XS&& ...xs ) {
        return f( forward<Y>(y), forward<X>(x), forward<XS>(xs)... );
    }
};

template< class F > Flip<F> flip( F&& f ) {
    return Flip<F>( forward<F>(f) );
}

/* 
 * Partial application.
 * g(y) = f( x, y )
 * partial( f, x ) -> g(y)
 * partial( f, x, y ) -> g()
 */
template< class F, class ...Arg >
struct PartialApplication;

template< class F, class Arg >
struct PartialApplication< F, Arg >
{
    F f;
    Arg arg;

    template< class _F, class _Arg >
    constexpr PartialApplication( _F&& f, _Arg&& arg )
        : f(forward<_F>(f)), arg(forward<_Arg>(arg))
    {
    }

    /* 
     * The return type of F only gets deduced based on the number of arguments
     * supplied. PartialApplication otherwise has no idea whether f takes 1 or 10 args.
     */
    template< class ... Args >
    constexpr auto operator() ( Args&& ...args )
        -> decltype( f(arg,declval<Args>()...) )
    {
        return f( arg, forward<Args>(args)... );
    }
};

/* Recursive, variadic version. */
template< class F, class Arg1, class ...Args >
struct PartialApplication< F, Arg1, Args... > 
    : public PartialApplication< PartialApplication<F,Arg1>, Args... >
{
    template< class _F, class _Arg1, class ..._Args >
    constexpr PartialApplication( _F&& f, _Arg1&& arg1, _Args&& ...args )
        : PartialApplication< PartialApplication<F,Arg1>, Args... > (
            PartialApplication<F,Arg1>( forward<_F>(f), forward<_Arg1>(arg1) ),
            forward<_Args>(args)...
        )
    {
    }
};

template< class F, class ...A >
constexpr PartialApplication<F,A...> partial( F&& f, A&& ...a )
{
    return PartialApplication<F,A...>( forward<F>(f), forward<A>(a)... );
}

/* 
 * Composition. 
 * normal notation:  h = f( g() )
 * Haskell notation: h = f . g
 * compose(f,g) -> f . g
 * compose(f,g,h...) -> f . g . h ...
 */

template< class F, class ...G >
struct Composition;

template< class F, class G >
struct Composition<F,G>
{
    F f; G g;

    template< class _F, class _G >
    constexpr Composition( _F&& f, _G&& g ) 
        : f(forward<_F>(f)), g(forward<_G>(g)) 
    {
    }

    template< class ...Args >
    constexpr auto operator() ( Args&& ...args )
        -> decltype( f(g(declval<Args>()...)) )
    {
        return f( g(forward<Args>(args)...) );
    }
};

template< class F, class G, class ...H >
struct Composition<F,G,H...> : Composition<F,Composition<G,H...>>
{
    typedef Composition<G,H...> Comp;

    template< class _F, class _G, class ..._H >
    constexpr Composition( _F&& f, _G&& g, _H&& ...h )
        : Composition<_F,Composition<_G,_H...>> ( 
            forward<_F>(f), 
            Comp( forward<_G>(g), forward<_H>(h)... )
        )
    {
    }
};

template< class F, class ...G >
constexpr Composition<F,G...> compose( F&& f, G&& ...g )
{
    return Composition<F,G...>( forward<F>(f), forward<G>(g)... );
}

/*
 * In category theory, a functor is a mapping between categories.
 * Pure provides the minimal wrappings to lift X to a higher category.
 *
 * pure(x) -> F(x) where F(_) = x
 */
template< class X >
struct Pure
{
    X x;

    explicit constexpr Pure( X x ) : x( x ) { }
    constexpr Pure( Pure&& ) = default;

    template< class ...Args >
    constexpr X operator() ( Args ... ) { return x; }
};

template< class X >
constexpr Pure<X> pure( X&& x )
{
    return Pure<X>( forward<X>(x) );
}

/* 
 * Functor F:
 *      fmap f (F a) -> F (f a)
 *
 * fmap maps a function, f, to a functor, F such that for each f:X->Y, there
 * exists an F(f):F(X)->F(Y). 
 *
 * fmap f F(x) = F(f x) 
 *
 * In Haskell, each mappable type class must be specialized with a Functor
 * instance. Likewise, here, we must use template specialization for each type
 * we want mappable.
 */
template< class ...F > struct Functor;

/* fmap f g = compose( f, g ) */
template< class Function >
struct Functor<Function>
{
    template< class F, class G >
    static Composition<F,G> fmap( F&& f, G&& g ) {
        return compose( forward<F>(f), forward<G>(g) );
    }
};

/* fmap f Pair(x,y) = Pair( f x, f y ) */
template< class X, class Y >
struct Functor< pair<X,Y> >
{
    template< class F, class R = decltype(declval<F>()(declval<X>())) >
    static pair<R,R> fmap( const F& f, const pair<X,Y>& p ) {
        return make_pair( f(p.first), f(p.second) );
    }

    template< class F >
    static pair<X,Y> fmap( const F& f, pair<X,Y>&& p ) {
        p.first = f( p.first );
        p.second = f( p.second );
        return move( p );
    }
};

template< class T >
struct Functor< typename enable_if< is_pointer<T>::value, T >::value, 
                T >
{
    template< class F, class R = decltype( declval<F>()(*declval<T>()) ) >
    static unique_ptr<R> fmap( const F& f, T&& m ) {
        return maybe( Nothing<R>(), compose(Just<R>,f), forward<T>(m) );
    }
};

template< class L, class R >
struct Functor< Either<L,R> > 
{
    template< class F, class FR = decltype( declval<F>()(declval<R>()) ) >
    static Either<L,FR>  fmap( const F& f, const Either<L,R>& e ) {
        return e.right ?  Right<L>( f(*e.right) ) : Left<FR>( *e.left );
    }
};

/* map f {1,2,3} -> { f(1), f(2), f(3) } */
template< template<class...> class S, class X, class ...XS, class F,
          class R = decltype( declval<F>()(declval<X>()) ) >
S<R,XS...> map( F&& f, const S<X,XS...>& xs ) 
{
    S<R,XS...> r;
    transform( begin(xs), end(xs), back_inserter(r),
               forward<F>(f) );
    return r;
}

template< class X, size_t N, class F, class R = decltype( declval<F>()(declval<X>()) ) >
array< R, N > map( F&& f, const array< X, N >& xs ) {
    array< R, N > r;
    transform( begin(xs), end(xs), begin(r),
               forward<F>(f) );
    return r;
}

template< class C > struct IsSeqImpl {
    // Can only be supported on STL-like sequence types, not pointers.
    template< class _C > static true_type f(typename _C::iterator*);
    template< class _C > static false_type f(...);
    typedef decltype( f<C>(0) ) type;
};

template< class C > struct IsSeq : public IsSeqImpl<C>::type { };

/* Enable if is an STL-like sequence. */
template< class C, class R > struct ESeq : enable_if<IsSeq<C>::value,R> { };

/* Disable if is sequence. */
template< class C, class R > struct XSeq : enable_if<not IsSeq<C>::value,R> { };

/* fmap f {...} = map f {...} where {...} is any sequence. */
template< class F, class C >
constexpr auto fmap( F&& f, C&& c )
    -> typename ESeq <
        C, decltype( map(forward<F>(f),forward<C>(c)) ) 
    >::type
{
    return map( forward<F>(f), forward<C>(c) );
}

template< class F, class X >
unique_ptr< decltype( declval<F>()(declval<X>()) ) >
fmap( F&& f, X* p ) {
    return p ? Just(forward<F>(f)(*p)) : nullptr;
}

template< class F, class X >
unique_ptr< decltype( declval<F>()(declval<X>()) ) >
fmap( F&& f, unique_ptr<X> p ) {
    return p ? Just(forward<F>(f)(*p)) : nullptr;
}

template< class F, class G >
constexpr auto fmap( F&& f, G&& g )
    // Disallow on sequences.
    -> typename XSeq < 
        G, 
        decltype( Functor<G>::fmap(forward<F>(f),forward<G>(g)) ) 
    >::type
{
    return Functor<G>::fmap( forward<F>(f), forward<G>(g) );
}

/* foldl f x {1,2,3} -> f(f(f(x,1),2),3) */
template< typename Container, typename Value, typename F >
constexpr Value foldl( const F& f, Value val, const Container& cont )
{
    return accumulate( begin(cont), end(cont), move(val), f );
}

template< typename Value, typename F, typename Container >
constexpr Value foldl( F&& f, const Container& cont )
{
    return accumulate( next(begin(cont)), end(cont), 
                            cont.front(), f );
}

/* foldr f x {1,2,3} -> f(1,f(2,f(3,x))) */
template< typename F, typename Value, typename Container >
constexpr Value foldr( F&& f, Value val, const Container& cont )
{
    return accumulate( cont.rbegin(), cont.rend(), 
                       move(val), forward<F>(f) );
}

template< typename Value, typename F, typename Container >
constexpr Value foldr( F&& f, const Container& cont )
{
    return accumulate( next(cont.rbegin()), cont.rend(), 
                       cont.back(), forward<F>(f) );
}

/* 
 * append({1},{2,3},{4}) -> {1,2,3,4} 
 * Similar to [1]++[2,3]++[4]
 */
template< typename C1, typename C2 >
C1 append( C1 a, const C2& b ) {
    copy( begin(b), end(b), back_inserter(a) );
    return a;
}

template< typename Cx1, typename Cx2, typename Cx3, typename ... Cxs >
Cx1 append( Cx1 a, const Cx2& b, const Cx3& c, const Cxs& ... d )
{
    return append( append(move(a), b), c, d... );
}


template< class SS, class S = typename SS::value_type >
S concat( const SS& ss ) {
    return foldl( append<S,S>, S(), ss );
}

/* 
 * concatMap f s = concat (map f s)
 *      where f is a function: a -> [b]
 *            map f s produces a type: [[b]]
 * The operation "map then concat" may be inefficient compared to an inlined
 * loop, which would not require allocating a sequence of sequences to be
 * concatenated. This is an optimization that should be equivalent to the
 * inlined loop.
 */
template< class F, class S > 
auto concatMap( const F& f, const S& xs ) -> decltype( f(xs.front()) ) 
{
    typedef decltype( f(xs.front()) ) R;
    return foldr ( 
        flip( translate(append<R,R>,f) ), // \ acc x = append( acc, f(x) )
        R(), xs
    );
}

template< typename Container, typename F >
void for_each( const F& f, const Container& cont )
{
    for_each( begin(cont), end(cont), f );
}

template< class F, class I, class J >
void for_ij( const F& f, I i, const I& imax, J j, const J& jmax )
{
    for( ; j != jmax; j++ )
        for( ; i != imax; i++ )
            f( i, j );
}

template< class F, class I, class J >
void for_ij( const F& f, const I& imax, const J& jmax )
{
    for( J j = J(); j != jmax; j++ )
        for( I i = I(); i != imax; i++ )
            f( i, j );
}

/*
 * Monoid M :
 *      mempty -> M
 *      mappend M M -> M
 *      mconcat [M] -> M
 *
 *      mconcat = foldr mappend mempty
 */
template< class ...M > struct Monoid;

template< class M, class Mo = Monoid<M> >
decltype( Mo::mempty() )
mempty() { return Mo::mempty(); }

/* mempty [] */
template< class M > decltype( M() ) mempty() { return M(); }

template< class M1, class M2, class Mo = Monoid<M1> >
decltype( Mo::mappend(declval<M1>(),declval<M2>()) )
mappend( M1&& a, M2&& b ) {
    return Mo::mappend( forward<M1>(a), forward<M2>(b) );
}

/*
 * mappend const& 
 * The above && version will always be preferred, except in the case of a
 * const&. In other words, this version will NEVER be preferred otherwise.
 */
template< class M1, class M2, class Mo = Monoid<M1> >
decltype( Mo::mappend(declval<M1>(),declval<M2>()) )
mappend( const M1& a, M2&& b ) {
    return Mo::mappend( a, forward<M2>(b) );
}

/* mappend [] */
template< class XS, class YS >
typename ESeq< XS, XS >::type
mappend( XS xs, YS&& ys ) { 
    return append( move(xs), forward<YS>(ys) ); 
}

template< class S, class M = typename S::value_type >
decltype( Monoid<M>::mconcat(declval<S>()) ) mconcat( const S& s ) 
{
    return Monoid<M>::mconcat( s );
}

/* mconcat [] */
template< class SS >
typename ESeq< typename SS::value_type,
               decltype( concat(declval<SS>()) ) >::type
mconcat( SS&& ss ) {
    return concat( forward<SS>(ss) );
}

// When we call mappend from within Monoid<M>, lookup deduction will see its
// own implementation of mappend. This ensures a recursive mappend gets
// properly forwarded.
template< class XS, class YS > 
decltype( mappend(declval<XS>(),declval<YS>()) )
fwd_mappend( XS&& xs, YS&& ys ) {
    return mappend( forward<XS>(xs), forward<YS>(ys) );
}

template< class M > 
decltype( mempty<M>() ) fwd_mempty() { return mempty<M>(); }

/* Monoid (Maybe X) -- where X is a monoid. */
template< class X > struct Monoid< X* > {
    static X* mempty() { return nullptr; }

    static unique_ptr<X> mappend( X* x, X* y ) {
        return x and y ? Just( fwd_mappend(*x,*y) ) : x | y;
    }

    template< class S >
    static unique_ptr<X> mconcat( const S& s ) {
        typedef unique_ptr<X> (*F) ( X*, X* );
        return foldl( (F)mappend, mempty(), s );
    }
};

template< class X > struct Monoid< unique_ptr<X> > {
    typedef unique_ptr<X> P;

    static P mempty() { return P(nullptr); }

    static P mappend( const P& x, const P& y ) {
        return x and y ? Just( fwd_mappend(*x,*y) )
            : x ? Just(*x) : y ? Just(*y) : nullptr;
    }

    static P mappend( P&& x, P&& y ) {
        return x and y ? Just( fwd_mappend(*x,*y) )
            : move(x) | move(y);
    }

    template< class S >
    static P mconcat( const S& s ) {
        typedef P (*F) ( X*, X* );
        return foldl( (F)mappend, mempty(), s );
    }
};

/* Monoid (Pair X X) */
template< class X, class Y > struct Monoid< pair<X,Y> > {
    typedef pair<X,Y> P;

    static P mempty() { return P( fwd_mempty<X>(), fwd_mempty<Y>() ); }

    static P mappend( const P& a, const P& b ) {
        return P( fwd_mappend(a.first,b.first), 
                  fwd_mappend(a.second,b.second) );
    }

    template< class S > static P mconcat( const S& s ) {
        return foldr( mappend, mempty(), s );
    }
};

/*
 * Monad M :
 *   mdo (M a) (M b) -> M b        -- (M-do) Do from left to right.
 *   mbind (M a) (a -> M b) -> M b -- Apply to the right the value of the left.
 *   mreturn a -> M a              -- Lift a.
 *   mfail String -> M a           -- Produce a failure value.
 *
 *   a >>  b = mdo   a b
 *   m >>= f = mbind m f
 */
template< class ...M > struct Monad;

/* m >> k = k -- where m is a sequence. */
template< class A, class B >
B mdo( const A& a, const B& b ) {
    // In Haskell, this is defined as
    //      m >> k = foldr ((++) . (\ _ -> k)) [] m
    // In other words, 
    //      for each element in a, duplicate b.
    //      [] >> k = []
    B c;
    auto size = a.size();
    while( size-- )
        c = append( b, c );
    return c;
}

/* m >> k */
template< class A, class B >
decltype( Monad<A>::mdo(declval<A>(),declval<B>()) ) 
mdo( A&& a, B&& b ) {
    return Monad<A>::mdo( forward<A>(a), forward<B>(b) ); 
}

/* m >>= k -- where m is a sequence. */
template< class S, class F, class R = typename remove_reference < 
        decltype( declval<F>()(declval<S>().front()) ) 
    >::type >
R mbind( const S& xs, const F& f )
{ 
    // xs >>= f = foldr g [] xs 
    //     where g acc x = acc ++ f(x)
    //           ++ = append
    return concatMap( f, xs );
}


/* m >>= k */
template< class M, class F >
decltype( Monad<M>::mbind(declval<M>(),declval<F>()) ) 
mbind( M&& m, F&& f ) {
    return Monad<M>::mbind( forward<M>(m), forward<F>(f) ); 
}

/* return<S> x = [x] -- where S is a sequence. */
template< template<class> class S, class X, class SX = S<X> >
auto mreturn( X x ) -> decltype( SX{x} ) {
    return SX{ x };
}

/* return<M> x = M x */
template< class M, class X >
M mreturn( X x ) {
    return Monad< M >::mreturn( move(x) );
}

/* mreturn () = (\x -> return x) */
template< class M > 
decltype( Monad<M>::mreturn() ) mreturn() { 
    return Monad<M>::mreturn();
}

//template< template<class> class S, class X, class SX = S<X> >
//decltype( SX() ) mfail( const char* const ) {
//    return SX();
//}

template< class M >
M mfail( const char* const why ) {
    return Monad< M >::mfail( why );
}

template< class X, class Y >
decltype( mbind(declval<X>(),declval<Y>()) ) 
operator >>= ( X&& x, Y&& y ) {
    return mbind( forward<X>(x), forward<Y>(y) );
}

template< class X, class Y >
decltype( mdo(declval<X>(),declval<Y>()) ) 
operator >> ( X&& x, Y&& y ) {
    return mdo( forward<X>(x), forward<Y>(y) );
}

template< class X > struct Monad< X* > {
    static unique_ptr<X> mreturn( X x ) { return Just(move(x)); }

    typedef unique_ptr<X> (*Return)( X );
    static Return mreturn() { return Just<X>; }

    static X* mfail(const char*) { return nullptr; }

    template< class Y >
    static Y* mdo( X* x, Y* y ) {
        return x ? y : nullptr;
    }

    template< class Y, class F, 
              class R = decltype( declval<F>()(*declval<Y>()) ) >
    static typename enable_if< is_pointer<R>::value, R >::type
    mbind( Y* y, const F& f ) {
        return y ? f(*y) : nullptr;
    }
};

template< class X > struct Monad< unique_ptr<X> > {
    static unique_ptr<X> mreturn( X x ) { return Just(move(x)); }

    typedef unique_ptr<X> (*Return)( X );
    static Return mreturn() { return Just<X>; }

    static unique_ptr<X> mfail(const char*) { return nullptr; }

    template< class PZ >
    static PZ mdo( const unique_ptr<X>& x, PZ&& z ) {
        return x ? forward<PZ>(z) : nullptr;
    }

    template< class F,
              class R = decltype( declval<F>()(declval<X>()) ) >
    static R mbind( const unique_ptr<X>& x, const F& f ) {
        return x ? f(*x) : nullptr;
    }
};

/*
 * MonadPlus M :
 *      mzero -> M
 *      mplus M M -> M
 *
 *      mzero >>= f = mzero
 */
template< class ...F > struct MonadPlus;

template< class M >
decltype( M() ) mzero() { return M(); }

template< class M >
decltype( MonadPlus<M>::mzero() )
mzero() { return MonadPlus<M>::mzero(); }

template< class S >
typename ESeq< S, S >::type mplus( S a, const S& b ) {
    return append( move(a), b );
}

    template< class M1, class M2, class Mo = Monoid<M1> >
decltype( MonadPlus<M1>::mplus(declval<M1>(),declval<M2>()) )
mplus( M1&& a, M2&& b ) {
    return MonadPlus<M1>::mplus( forward<M1>(a), forward<M2>(b) );
}

template< class X > struct MonadPlus< X* > {
    static X* mzero() { return nullptr; }
    static X* mplus( X* a, X* b ) { return a | b; }
};

template< class X > struct MonadPlus< unique_ptr<X> > {
    typedef unique_ptr<X> P;

    static P mzero() { return P(nullptr); }
    static const P& mplus( const P& x, const P& y ) { return x | y; }
    static P mplus( P&& x, P&& y ) { return move(x) | move(y); }
};

/*
 * Rotation.
 * f(x...,y) = g(y,x...)
 * rot f = g
 * rrot g = f
 * rrot( rot f ) = f
 *
 * In stack-based (or concatenative languages) rotation applies to the top
 * three elements on the stack. Here, the function's arguments are treated as a
 * stack and rotated. The entire stack gets rotated, rather than the top three
 * elements.
 */
template< class F, class ...Args >
struct PartialLast; // Apply the last argument.

template< class F, class Last >
struct PartialLast< F, Last > : public PartialApplication< F, Last > 
{ 
    template< class _F, class _Last >
    constexpr PartialLast( _F&& f, _Last&& last )
        : PartialApplication<F,Last>( forward<_F>(f), forward<_Last>(last) )
    {
    }
};

// Remove one argument each recursion.
template< class F, class Arg1, class ...Args >
struct PartialLast< F, Arg1, Args... > : public PartialLast< F, Args... > 
{
    template< class _F, class _Arg1, class ..._Args >
    constexpr PartialLast( _F&& f, _Arg1&&, _Args&& ...args )
        : PartialLast<F,Args...>( forward<_F>(f), forward<_Args>(args)... )
    {
    }
};

template< class F, class ...Args >
struct PartialInitial; // Apply all but the last argument.

template< class F, class Arg1, class Arg2 >
struct PartialInitial< F, Arg1, Arg2 > : public PartialApplication< F, Arg1 >
{
    template< class _F, class _Arg1, class _Arg2 >
    constexpr PartialInitial( _F&& f, _Arg1&& arg1, _Arg2&& )
        : PartialApplication<F,Arg1>( forward<_F>(f), forward<_Arg1>(arg1) )
    {
    }
};

template< class F, class Arg1, class ...Args >
struct PartialInitial< F, Arg1, Args... > 
    : public PartialInitial< PartialApplication<F,Arg1>, Args... >
{
    template< class _F, class _Arg1, class ..._Args >
    PartialInitial( _F&& f, _Arg1&& arg1, _Args&& ...args )
        : PartialInitial<PartialApplication<F,Arg1>,Args...> (
            partial( forward<_F>(f), forward<_Arg1>(arg1) ),
            forward<_Args>(args)...
        )
    {
    }
};

template< class F >
struct Rot
{
    F f;

    template< class _F >
    constexpr Rot( _F&& f ) : f( forward<_F>(f) ) { }

    template< class ...Args >
    constexpr auto operator() ( Args&& ...args )
        -> decltype ( 
            declval< PartialInitial<PartialLast<F,Args...>,Args...> >()() 
        )
    {
        /* We can't just (to my knowledge) pull the initial and final args
         * apart, so first reverse-apply the last argument, then apply each
         * argument forward until the last. The result is a zero-arity function
         * to invoke.
         */
        return PartialInitial< PartialLast<F,Args...>, Args... > (
            PartialLast< F, Args... >( f, forward<Args>(args)... ),
            forward<Args>( args )...
        )();
    }
};

template< class F >
struct RRot // Reverse Rotate
{
    F f;

    template< class _F >
    constexpr RRot( _F&& f ) : f( forward<_F>(f) ) { }

    template< class Arg1, class ...Args >
    constexpr auto operator() ( Arg1&& arg1, Args&& ...args )
        -> decltype( f(declval<Args>()..., arg1) )
    {
        return f( forward<Args>(args)..., arg1 );
    }
};

template< class F >
constexpr Rot<F> rot( F&& f )
{
    return Rot<F>( forward<F>(f) );
}

template< class F >
constexpr RRot<F> rrot( F&& f )
{
    return RRot<F>( forward<F>(f) );
}

/* Rotate F by N times. */
template< unsigned int N, class F >
struct NRot;

template< class F >
struct NRot< 1, F > : public Rot<F>
{
    template< class _F >
    constexpr NRot( _F&& f ) : Rot<F>( forward<_F>(f) ) { }
};

template< unsigned int N, class F >
struct NRot : public NRot< N-1, Rot<F> >
{
    template< class _F >
    constexpr NRot( _F&& f ) : NRot< N-1, Rot<F> >( rot(forward<_F>(f)) ) { }
};

template< unsigned int N, class F >
struct RNRot; // Reverse nrot.

template< class F >
struct RNRot< 1, F > : public RRot<F>
{
    template< class _F >
    RNRot( _F&& f ) : RRot<F>( forward<_F>(f) ) { }
};

template< unsigned int N, class F >
struct RNRot : public RNRot< N-1, RRot<F> >
{
    template< class _F >
    RNRot( _F&& f ) : RNRot<N-1,RRot<F>>( rrot(forward<_F>(f)) ) { }
};

template< unsigned int N, class F >
constexpr NRot<N,F> nrot( F&& f )
{
    return NRot<N,F>( forward<F>(f) );
}

template< unsigned int N, class F >
constexpr RNRot<N,F> rnrot( F&& f )
{
    return RNRot<N,F>( forward<F>(f) );
}

/* squash[ f(x,x) ] = g(x) */
template< class F >
struct Squash
{
    F f;

    template< class _F >
    constexpr Squash( _F&& f ) : f( forward<_F>(f) ) { }

    template< class Arg1, class ...Args >
    constexpr auto operator() ( Arg1&& arg1, Args&& ...args )
        -> decltype( f(declval<Arg1>(),declval<Arg1>(),declval<Args>()...) )
    {
        return f( forward<Arg1>(arg1), forward<Arg1>(arg1), 
                  forward<Args>(args)... );
    }
};

template< class F >
constexpr Squash<F> squash( F&& f )
{
    return Squash<F>( forward<F>(f) );
}

/* 
 * f( x, y ) = f( l(z), r(z) ) = g(z)
 *  where x = l(z) and y = r(z)
 * join( f, l, r ) = g
 */
template< class F, class Left, class Right >
struct Join
{
    F f;
    Left l;
    Right r;

    template< class _F, class _L, class _R >
    constexpr Join( _F&& f, _L&& l, _R&& r )
        : f(forward<_F>(f)), l(forward<_L>(l)), r(forward<_R>(r))
    {
    }

    template< class A, class ...AS >
    constexpr auto operator() ( A&& a, AS&& ...as )
        -> decltype( f(l(declval<A>()),r(declval<A>()),declval<AS>()...) )
    {
        return f( l(forward<A>(a)), r(forward<A>(a)), forward<AS>(as)... );
    }
};

template< class F, class L, class R >
constexpr Join<F,L,R> join( F&& f, L&& l, R&& r )
{
    return Join<F,L,R>( forward<F>(f), forward<L>(l), forward<R>(r) );
}

template< typename Container >
constexpr bool ordered( const Container& c )
{
    return c.size() <= 1
        or mismatch ( 
            begin(c), end(c)-1, 
            begin(c)+1, 
            less_equal<int>() 
        ).second == end(c);
}

/* filter f C -> { x for x in C such that f(x) is true. } */
template< typename Container, typename F >
Container filter( const F& f, Container cont )
{
    typedef typename remove_reference<Container>::type::value_type T;
    cont.erase ( 
        remove_if ( 
            begin(cont), end(cont), 
            [&](const T& t){ return not f(t); } 
        ),
        end( cont )
    );
    return cont;
}

/* find pred xs -> Maybe x */
template< class F, class Sequence,
          class Val = decltype(&declval<Sequence>().front()) >
Val find( F&& f, Sequence&& s )
{
    const auto e = end( s ), b = begin( s );
    const auto it = find_if( b, e, forward<F>(f) );

    return it != e ? &(*it) : nullptr; 
}

/* cfind x C -> C::iterator */
template< typename Container, typename T >
constexpr auto cfind( const T& value, Container&& cont )
    -> decltype( begin(cont) )
{
    return find( begin(cont), end(cont), value );
}

template< typename Container, typename F >
constexpr auto cfind_if( const F& f, Container&& cont )
    -> decltype( begin(cont) )
{
    return find_if( begin(cont), end(cont), f );
}

/* all f C -> true when f(x) is true for all x in C; otherwise false. */
template< typename Container, typename F >
constexpr bool all( const F& f, const Container& cont )
{
    return all_of( begin(cont), end(cont), f );
}

/* zip_with f A B -> { f(a,b) for a in A and b in B } */
template< typename Container, typename F >
Container zip_with( F&& f, const Container& a, Container b )
{
    transform( begin(a), end(a), begin(b), 
                    begin(b), forward<F>(f) );
    return b;
}

/*
 * cleave x f g h -> { f(x), g(x), h(x) }
 * Inspired by Factor, the stack-based language.
 */
template< typename X, typename F, typename ... Fs >
constexpr array<X,sizeof...(Fs)+1> cleave( X x, const F& f, const Fs& ... fs ) 
{
    return {{ f(x), fs(x)... }};
}

/* cleave_with f x y z =  { f(x), f(y), f(z) } */
template< class F, class A, class ...B >
constexpr auto cleave_with( F&& f, A&& a, B&& ...b )
    -> array< decltype( declval<F>()(declval<A>()) ), sizeof...(B)+1 > 
{
    return {{ f(forward<A>(a)), f(forward<B>(b))... }};
}

template< class T, unsigned int N, class F >
array<T,N> generate( F f )
{
    array<T,N> cont;
    generate( begin(cont), end(cont), f );
    return cont;
}

template< class T, class F >
vector<T> generate( F f, unsigned int n )
{
    vector<T> c; c.reserve(n);
    while( n-- )
        c.push_back( f() );
    return c;
}

template< class Cmp, class Container >
constexpr auto max( const Cmp& cmp, Container&& cont )
    -> decltype( begin(cont) )
{
    return max_element( begin(cont), end(cont), cmp );
}

template< class Container >
constexpr auto max( Container&& cont )
    -> decltype( begin(cont) )
{
    return max_element( begin(cont), end(cont) );
}

template< class Cmp, class Container >
constexpr auto min( const Cmp& cmp, Container&& cont )
    -> decltype( begin(cont) )
{
    return min_element( begin(cont), end(cont), cmp );
}

template< class Container >
constexpr auto min( Container&& cont )
    -> decltype( begin(cont) )
{
    return min_element( begin(cont), end(cont) );
}

} // namespace pure

