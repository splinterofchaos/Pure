
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
 * Sequence "[ ]"
 * Any type that defines begin() and end().
 */

namespace cata {

/* 
 * CATEGORIES:
 * In order to easily implement algorithms for different categories of types,
 * we define categories and a type, Cat<T>, which defines Cat<T>::type as
 * either a category or T. This way, we can implement algorithms in terms of
 * either tags (categories) or specific types.
 *
 * This is similar to the tag dispatch pattern used in the STL (see:
 * random_access_iterator_tag, forward_iterator_tag, etc...) except that the
 * type is not erased.
 *
 * The following traits classes merely act as a shortcut to using decltype.
 */

/*
 * []
 * Any type that:
 *      Has a defined begin(s) and end(s).
 */
template< class S > struct sequence { using type = S; };

template< class Seq > struct sequence_traits {
    using sequence   = Seq;
    using iterator   = decltype( begin(declval<Seq>()) );
    using reference  = decltype( *declval<iterator>() );
    using value_type = typename decay<reference>::type;
};

/*
 * Maybe 
 * Any type that: 
 *      Can be dereferenced like so: *Maybe(). (fromJust).
 *      Can be converted to a boolean. (isJust/isNothing)
 *      But is not a function
 */
template< class M > struct maybe { using type = M; };

// If we want to return a pointer that defines ownership, we want a smart
// pointer. Consider any non-raw pointer good 'nuff.
template< class Ptr > struct SmartPtr { using type = Ptr; };
template< class X > struct SmartPtr<X*> { using type = unique_ptr<X>; };

template< class Ptr > struct maybe_traits {
    using pointer    = Ptr;
    using smart_ptr  = typename SmartPtr<pointer>::type;
    using reference  = decltype( *declval<pointer>() );
    using value_type = typename decay<reference>::type;
};

/*
 * cat performs the actual type deduction via its argument. If no other
 * definition of cat is valid, it returns a variable of the same type as its
 * argument. It is only for use in decltype expressions.
 */
template< class X >
X cat( ... );

template< class S >
auto cat( const S& s ) -> decltype( begin(s), end(s), sequence<S>() );

template< class M >
auto cat( const M& m ) -> decltype( *m, (bool)m, maybe<M>() );

template< class T > struct Cat {
    using type = decltype( cat<T>(declval<T>()) );
};

// Prevent function pointers from being deduced as maybe types.
template< class R, class ...X > struct Cat< R(&)(X...) > {
    typedef R(&type)(X...);
};

} // namespace cata

template< class ... > struct Category;

template< class X, class C = Category<X> >
auto cid( X&& x ) -> decltype( C::id( declval<X>() ) ) {
    return C::id( forward<X>(x) );
}

struct Id {
    template< class X >
    constexpr X operator() ( X&& x ) { return forward<X>(x); }
};

struct CId {
    template< class X >
    constexpr X operator() ( X&& x ) { return cid( forward<X>(x) ); }
};

/* 
 * Let comp be a generalization of compose. 
 */
template< class F, class ...G, class C = Category<F> > 
constexpr decltype( Category<F>::comp( declval<F>(), declval<G>()... ) )
comp( F&& f, G&& ...g ) {
    return C::comp( forward<F>(f), forward<G>(g)... );
}


/* 
 * FUNCTION TRANSFORMERS
 * The fallowing are types that contain one or more functions and act like a
 * function. 
 */

/* Translate f g : h(x,xs...) = f( g(x), xs... ) */
template< class F, class G > struct Translate {
    F f;
    G g;

    template< class _F, class _G >
    Translate( _F&& f, _G&& g ) : f(forward<_F>(f)), g(forward<_G>(g)) { }

    template< class X, class ...XS >
    decltype( f(g(declval<X>()),declval<XS>()...) )
    operator() ( X&& x, XS&& ...xs ) {
        return f( g(forward<X>(x)), forward<XS>(xs)... );
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
 * Given f(x,y...) and g(z)
 * The composition of f and g, f . g, equals h(z,y...)
 *      h(z,y...) = f( g(z), y... )
 *
 */

template< class F, class ...G >
struct Composition;

template< class F, class G >
struct Composition<F,G>
{
    F f; G g;

    template< class _F, class _G >
    constexpr Composition( _F&& f, _G&& g ) 
        : f(forward<_F>(f)), g(forward<_G>(g)) { }

    template< class X, class ...Y >
    constexpr decltype( f(g(declval<X>()), declval<Y>()...) )
    operator() ( X&& x, Y&& ...y ) {
        return f( g( forward<X>(x) ), forward<Y>(y)... );
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
constexpr Composition<F,G...> compose( F f, G ...g ) {
    return Composition<F,G...>( move(f), move(g)... );
}

/* A const composition for when g is a constant function. */
template< class F, class ...G >
struct CComposition;

template< class F, class G >
struct CComposition<F,G>
{
    F f; G g;

    template< class _F, class _G >
    constexpr CComposition( _F&& f, _G&& g ) 
        : f(forward<_F>(f)), g(forward<_G>(g)) { }

    template< class ...Y >
    constexpr decltype( f(g(), declval<Y>()...) )
    operator() ( Y&& ...y ) {
        return f( g(), forward<Y>(y)... );
    }
};

template< class F, class G, class ...H >
struct CComposition<F,G,H...> : CComposition<Composition<F,G>,H...>
{
    typedef Composition<F,G> Then;
    typedef CComposition<Then,H...> CComp;

    template< class _F, class _G, class ..._H >
    constexpr CComposition( _F&& f, _G&& g, _H&& ...h )
        : CComp ( 
            Then( forward<_F>(f), forward<_G>(g) ),
            forward<_H>(h)...
        )
    {
    }
};

template< class F, class ...G, class C = CComposition<F,G...> >
constexpr C ccompose( F f, G ...g ) {
    return C( move(f), move(g)... );
}

/* N-ary composition assumes a unary f and N-ary g. */
template< class F, class ...G >
struct NCompoposition;

template< class F, class G >
struct NCompoposition<F,G>
{
    F f; G g;

    template< class _F, class _G >
    constexpr NCompoposition( _F&& f, _G&& g ) 
        : f(forward<_F>(f)), g(forward<_G>(g)) { }

    template< class ...X >
    constexpr decltype( f(g(declval<X>()...)) )
    operator() ( X&& ...x ) {
        return f( g( forward<X>(x)... ) );
    }
};

template< class F, class G, class ...H >
struct NCompoposition<F,G,H...> : NCompoposition<Composition<F,G>,H...>
{
    typedef Composition<F,G> Then;
    typedef NCompoposition<Then,H...> NComp;

    template< class _F, class _G, class ..._H >
    constexpr NCompoposition( _F&& f, _G&& g, _H&& ...h )
        : NComp ( 
            Then( forward<_F>(f), forward<_G>(g) ),
            forward<_H>(h)...
        )
    {
    }
};

template< class F, class ...G, class C = NCompoposition<F,G...> >
constexpr C ncompose( F f, G ...g ) {
    return C( move(f), move(g)... );
}

/*
 * Binary composition 
 *      (compose2 http://www.sgi.com/tech/stl/binary_compose.html)
 * Given g(x)=y,  h(x)=z, and f(y,z), let bf(x) = f( g(x), h(x) )
 * This implementation diverges from compose2 in that it allows g and h to be
 * n-ary.
 */
template< class F, class G, class H >
struct BCompoposition
{
    F f; G g; H h;

    template< class _F, class _G, class _H >
    constexpr BCompoposition( _F&& f, _G&& g, _H&& h ) 
        : f(forward<_F>(f)), g(forward<_G>(g)), h(forward<_H>(h)) { }

    template< class ...X >
    constexpr decltype( f(g(declval<X>()...), h(declval<X>()...)) )
    operator() ( X&& ...x ) {
        return f( g( forward<X>(x)... ), h( forward<X>(x)... ) );
    }
};

template< class F, class G, class H, class C = BCompoposition<F,G,H> >
constexpr C bcompose( F f, G g, H h ) {
    return C( move(f), move(g), move(h) );
}

/* Default category: function. */
template< class F > struct Category<F> {
    static constexpr Id id = Id();

    template< class _F, class ..._G > static
    constexpr decltype( compose(declval<_F>(),declval<_G>()...) )
    comp( _F&& f, _G&& ...g ) {
        return compose( forward<_F>(f), forward<_G>(g)... );
    }
};

template< class ... > struct Arrow;

/* Haskell defines <<< as an alias for (.). */

/*
 * fcomp (forward compose): the reverse of comp.
 * Haskell's >>>.
 */
template< class F, class G >
constexpr auto fcomp( F&& f, G&& g ) 
    -> decltype( comp(declval<G>(), declval<F>()) ) 
{
    return comp( forward<G>(g), forward<F>(f) ); 
}

template< class F, class G, class H, class ...I>
constexpr decltype( comp( fcomp(declval<G>(),declval<H>(),declval<I>()...), 
                          declval<F>() ) )
fcomp( F&& f, G&& g, H&& h, I&& ...i ) {
    return comp( fcomp( forward<G>(g), forward<H>(h), forward<I>(i)... ),
                 forward<F>(f) );
}

template< class A, class F, class Arr = Arrow<A> >
decltype( Arr::arr( declval<F>() ) )
arr( F&& f ) { return Arr::arr( forward<F>(f) ); }

/* (f *** g) (x,y) = (f x, g y) */
template< class F, class G, class A = Arrow<F> >
decltype( A::split(declval<F>(), declval<G>()) )
split( F&& f, G&& g ) {
    return A::split( forward<F>(f), forward<G>(g) );
}

/* (f &&& g) x = (f x, g x) */
template< class F, class G, class A = Arrow<F> >
decltype( A::fanout(declval<F>(),declval<G>()) )
fanout( F&& f, G&& g ) {
    return A::fanout( forward<F>(f), forward<G>(g) );
}

/* (first f) (x,y) = (f x, y) */
template< class F, class A = Arrow<F> >
decltype( A::first(declval<F>()) )
first( F&& f ) {
    return A::first( forward<F>(f) );
}

/* (second f) (x,y) = (x, f y) */
template< class F, class A = Arrow<F> >
decltype( A::second(declval<F>()) )
second( F&& f ) {
    return A::second( forward<F>(f) );
}

template< class Func > struct Arrow<Func> {
    static constexpr Id arr = Id();

    /* 
     * FSplit f g = fs.
     * fs (x,y)  = (f x, g y)
     */
    template< class F, class G > struct FSplit {
        F f; G g;

        template< class _F, class _G >
        constexpr FSplit( _F&& f, _G&& g ) 
            : f(forward<_F>(f)), g(forward<_G>(g)) { }

        template< class P/*air*/ >
        auto operator() ( const P& p ) const
            -> decltype( make_pair(f(get<0>(declval<P>())),
                                   g(get<1>(declval<P>()))) )
        {
            return make_pair( f(get<0>(p)), g(get<1>(p)) );
        }
    };

    template< class F, class G, class S = FSplit<F,G> >
    static S split ( F f, G g ) {
        return S( move(f), move(g) );
    }

    template< class F, class S = FSplit<F,Id> > static 
    S first( F f ) { return S( move(f), arr ); }

    template< class F, class S = FSplit<Id,F> > static 
    S second( F f ) { return S( arr, move(f) ); }

    /*
     * Fan f g = fan
     * fan x = (f x, g x)
     */
    template< class F, class G > struct Fan {
        F f; G g;

        template< class _F, class _G >
        constexpr Fan( _F&& f, _G&& g ) : f(forward<_F>(f)), g(forward<_G>(g)) { }

        template< class X >
        decltype( make_pair( f(declval<X>()), g(declval<X>()) ) )
        operator() ( X&& x ) const {
            return make_pair( f(forward<X>(x)), g(forward<X>(x)) );
        }
    };

    template< class F, class G, class Fan = Fan<F,G> >
    static Fan fanout( F f, G g ) {
        return Fan( move(f), move(g) );
    }
};

/* map f {1,2,3} -> { f(1), f(2), f(3) } */
template< template<class...> class S, class X, class ...XS, class F,
          class R = typename result_of<F(X)>::type >
S<R,XS...> map( F&& f, const S<X,XS...>& xs ) 
{
    S<R,XS...> r;
    transform( begin(xs), end(xs), back_inserter(r),
               forward<F>(f) );
    return r;
}

template< class X, size_t N, class F, 
          class R = class result_of<F(X)>::type >
array< R, N > map( F&& f, const array< X, N >& xs ) {
    array< R, N > r;
    transform( begin(xs), end(xs), begin(r), forward<F>(f) );
    return r;
}

/* foldl f x {1,2,3} -> f(f(f(x,1),2),3) */
template< class F, class X, class S >
constexpr X foldl( F&& f, X&& val, const S& cont )
{
    return accumulate( begin(cont), end(cont), forward<X>(val), 
                       forward<F>(f) );
}

template< class X, class F, class S >
constexpr X foldl( F&& f, const S& cont )
{
    return accumulate( next(begin(cont)), end(cont), 
                            cont.front(), forward<F>(f) );
}

/* foldr f x {1,2,3} -> f(1,f(2,f(3,x))) */
template< class F, class X, class S >
constexpr X foldr( F&& f, X&& val, const S& cont )
{
    return accumulate( cont.rbegin(), cont.rend(), 
                       forward<X>(val), forward<F>(f) );
}

template< class X, class F, class S >
constexpr X foldr( F&& f, const S& cont )
{
    return accumulate( next(cont.rbegin()), cont.rend(), 
                       cont.back(), forward<F>(f) );
}

/* 
 * append({1},{2,3},{4}) -> {1,2,3,4} 
 * Similar to [1]++[2,3]++[4]
 */
template< typename A, typename B >
A append( A a, const B& b ) {
    copy( begin(b), end(b), back_inserter(a) );
    return move(a);
}

template< typename A, typename B, typename C, typename ...D >
A append( A&& a, const B& b, const C& c, const D& ... d )
{
    return append( append(forward<A>(a), b), c, d... );
}


template< class SS, class S = typename SS::value_type >
S concat( const SS& ss ) {
    return foldl( append<S,S>, S(), forward<SS>(ss) );
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
template< class F, class S,
          class R = decltype( declval<F>()( declval<S>().front() ) ) > 
R concatMap( F&& f, S&& xs ) {
    return foldr ( 
        // \ acc x = append( acc, f(x) )
        flip( translate(append<R,R>,forward<F>(f)) ), 
        R(), forward<S>(xs)
    );
}

template< typename Container, typename F >
void for_each( F&& f, const Container& cont ) {
    for_each( begin(cont), end(cont), forward<F>(f) );
}

template< class F, class I, class J >
void for_ij( const F& f, I i, const I& imax, J j, const J& jmax ) {
    for( ; j != jmax; j++ )
        for( ; i != imax; i++ )
            f( i, j );
}

template< class F, class I, class J >
void for_ij( const F& f, const I& imax, const J& jmax ) {
    for( J j = J(); j != jmax; j++ )
        for( I i = I(); i != imax; i++ )
            f( i, j );
}


/* 
 * Just  x -> Maybe (Just x) | with a definite value.
 * Nothing -> Maybe Nothing  | with definitely no value.
 */
template< class T, class M = std::unique_ptr<T> > 
constexpr M Just( T t ) {
    return M( new T(move(t)) );
}

template< class X >
constexpr X* Just( X* x ) { return x; }

template< class T, class M = std::unique_ptr<T> > 
constexpr M Nothing() {
    return nullptr;
}

/* maybe b (a->b) (Maybe a) -> b */
template< class R, class F, class P >
constexpr R maybe( R&& nothingVal, F&& f, P&& m ) {
    return m ? forward<F>(f)( *forward<P>(m) )
             : forward<R>( nothingVal );
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
auto operator* ( F&& a, P&& b )
    -> unique_ptr< Ret >
{
    return a and b ? Just( (*forward<F>(a))(*forward<P>(b)) ) 
                   : nullptr;
}

template< class P > P operator|| ( P&& a, P&& b ) {
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

    // TODO: Make these type constructors, not types.
    struct Left { 
        left_type value;
        constexpr Left( left_type value ) : value(move(value)) { }
    };

    struct Right { 
        right_type value;
        constexpr Right( right_type value ) : value(move(value)) { }
    };

    template< class X >
    explicit constexpr Either( X&& x )  
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
    -> decltype( declval<F>()(*e.right) )
{
    return e.right ? forward<F>(f)(*e.right) : forward<G>(g)(*e.left);
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
struct Functor<Function> {
    template< class F, class G >
    static auto fmap( F&& f, G&& g ) 
        -> decltype( comp(declval<F>(),declval<G>()) )
    {
        return comp( forward<F>(f), forward<G>(g) );
    }
};

/* fmap f Pair(x,y) = Pair( f x, f y ) */
template< class X, class Y >
struct Functor< pair<X,Y> > {
    template< class F, class R = decltype(declval<F>()(declval<X>())) >
    static pair<R,R> fmap( F&& f, const pair<X,Y>& p ) {
        return make_pair( f(p.first), f(p.second) );
    }

    template< class F >
    static pair<X,Y> fmap( F&& f, pair<X,Y>&& p ) {
        F _f = forward<F>(f);
        p.first  = _f( p.first );
        p.second = _f( p.second );
        return move( p );
    }
};

template< class M >
struct Functor< cata::maybe<M> > {
    /*
     * f <$> Just x = Just (f x)
     * f <$> Nothing = Nothing
     */
    template< class F, class _M,
              class R = decltype( declval<F>()(*declval<_M>()) ) >
    static unique_ptr<R> fmap( F&& f, _M&& m ) {
        return m ? Just( forward<F>(f)(*m) ) : nullptr;
    }
};

template< class Seq >
struct Functor< cata::sequence<Seq> > {
    /* f <$> [x0,x1,...] = [f x0, f x1, ...] */
    template< class F, class S >
    static decltype( map(declval<F>(),declval<S>()) )
    fmap( F&& f, S&& s ) {
        return map( forward<F>(f), forward<S>(s) );
    }
};

template< class L, class R >
struct Functor< Either<L,R> > {
    template< class F, class FR = decltype( declval<F>()(declval<R>()) ) >
    static Either<L,FR>  fmap( F&& f, const Either<L,R>& e ) {
        return e.right ? Right<L>( forward<F>(f)(*e.right) ) 
                       : Left<FR>( *e.left );
    }
};

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

template< class F, class G, 
          class Fn = Functor< typename cata::Cat<G>::type > >
constexpr auto fmap( F&& f, G&& g )
    -> decltype( Fn::fmap(forward<F>(f),forward<G>(g)) ) 
{
    return Fn::fmap( forward<F>(f), forward<G>(g) );
}

template< class F, class M >
constexpr decltype( fmap(declval<F>(), declval<M>()) )
operator^ ( F&& f, M&& m ) {
    return fmap( forward<F>(f), forward<M>(m) );
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

template< class M, class Mo = Monoid<typename cata::Cat<M>::type> >
decltype( Mo::mempty() ) mempty() { return Mo::mempty(); }

template< class M1, class M2, 
          class Mo = Monoid<typename cata::Cat<M1>::type> >
decltype( Mo::mappend(declval<M1>(),declval<M2>()) )
mappend( M1&& a, M2&& b ) {
    return Mo::mappend( forward<M1>(a), forward<M2>(b) );
}

/*
 * mappend const& 
 * The above && version will always be preferred, except in the case of a
 * const&. In other words, this version will NEVER be preferred otherwise.
 */
template< class M1, class M2, 
          class Mo = Monoid<typename cata::Cat<M1>::type> >
decltype( Mo::mappend(declval<M1>(),declval<M2>()) )
mappend( const M1& a, M2&& b ) {
    return Mo::mappend( a, forward<M2>(b) );
}

template< class S, class V = typename cata::sequence_traits<S>::value_type,
          class M = Monoid<typename cata::Cat<V>::type> >
decltype( M::mconcat(declval<S>()) ) mconcat( S&& s ) 
{
    return M::mconcat( forward<S>(s) );
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

template< class Seq > struct Monoid< cata::sequence<Seq> > {
    static Seq mempty() { return Seq{}; }

    template< class XS, class YS >
    static decltype( append(declval<XS>(),declval<YS>()) )
    mappend( XS&& xs, YS&& ys ) {
        return append( forward<XS>(xs), forward<YS>(ys) );
    }

    template< class SS >
    static decltype( concat(declval<SS>()) )
    mconcat( SS&& ss ) {
        return concat( forward<SS>(ss) ); 
    }
};

/* Monoid (Maybe X) -- where X is a monoid. */
template< class M > struct Monoid< cata::maybe<M> > {
    static M mempty() { return nullptr; }

    /*
     * Just x <> Just y = Just (x <> y)
     *      a <> b      = a | b
     */
    static M mappend( const M& x, const M& y ) {
        return x and y ? Just( fwd_mappend(*x,*y) )
            : x ? Just(*x) : y ? Just(*y) : nullptr;
    }

    static M mappend( M&& x, M&& y ) {
        return x and y ? Just( fwd_mappend(*forward<M>(x), *forward<M>(y)) )
            : forward<M>(x) || forward<M>(y);
    }

    /* mconcat [Maybe x] -> Maybe x -- where mappend x x is defined. */
    template< class S > static M mconcat( S&& s ) {
        typedef M (*F) ( const M&, const M& );
        return foldl( (F)mappend, mempty(), forward<S>(s) );
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

/* m >> k */
template< class A, class B,
          class Mo = Monad<typename cata::Cat<A>::type> >
decltype( Mo::mdo(declval<A>(),declval<B>()) ) 
mdo( A&& a, B&& b ) {
    return Mo::mdo( forward<A>(a), forward<B>(b) ); 
}

/* m >>= k */
template< class M, class F,
          class Mo = Monad<typename cata::Cat<M>::type> >
decltype( Mo::mbind(declval<M>(),declval<F>()) ) 
mbind( M&& m, F&& f ) {
    return Mo::mbind( forward<M>(m), forward<F>(f) ); 
}

/* return<M> x = M x */
template< class M, class X > M mreturn( X&& x ) {
    return Monad< typename cata::Cat<M>::type >::mreturn( forward<X>(x) );
}

/* mreturn () = (\x -> return x) */
template< class M, class Mo = Monad<typename cata::Cat<M>::type> > 
decltype( Mo::mreturn() ) mreturn() { 
    return Mo::mreturn();
}

template< class M >
M mfail( const char* const why ) {
    return Monad< typename cata::Cat<M>::type >::mfail( why );
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

template< class Seq >
struct Monad< cata::sequence<Seq> > {
    template< class X >
    static Seq mreturn( X&& x ) { 
        return Seq{ forward<X>(x) }; 
    }

    static Seq mfail(const char*) { return Seq(); }

    template< class S, class YS >
    static YS mdo( const S& a, const YS& b ) {
        // In Haskell, this is defined as
        //      m >> k = foldr ((++) . (\ _ -> k)) [] m
        // In other words, 
        //      for each element in a, duplicate b.
        //      [] >> k = []
        YS c;
        auto size = a.size();
        while( size-- )
            c = append( c, b );
        return c;
    }

    /* m >>= k -- where m is a cata::sequence. */
    template< class S, class F >
    static decltype( concatMap(declval<F>(),declval<S>()) )
    mbind( S&& xs, F&& f ) { 
        // xs >>= f = foldr g [] xs 
        //     where g acc x = acc ++ f(x)
        //           ++ = append
        return concatMap( f, xs );
    }
};

template< class P > struct IsPointerImpl { 
    using reference = decltype( *declval<P>() );
    using bool_type = decltype( (bool)declval<P>() );
};

template< class M > 
struct Monad< cata::maybe<M> > {
    using traits = cata::maybe_traits<M>;
    using value_type = typename traits::value_type;
    using smart_ptr  = typename traits::smart_ptr;

    template< class X >
    static smart_ptr mreturn( X&& x ) { 
        return smart_ptr( new X(forward<X>(x)) ); 
    }

    using Return = smart_ptr (*) ( value_type );
    static Return mreturn() { return Just; }

    static smart_ptr mfail(const char*) { return nullptr; }

    template< class PZ >
    static PZ mdo( const M& x, PZ&& z ) {
        return x ? forward<PZ>(z) : nullptr;
    }

    template< class F,
              class R = decltype( declval<F>()(declval<value_type>()) ) >
    static R mbind( M&& x, F&& f ) {
        return x ? forward<F>(f)( *forward<M>(x) ) : nullptr;
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

template< class M, class Mo = MonadPlus<typename cata::Cat<M>::type> >
decltype( Mo::mzero() ) mzero() { return Mo::mzero(); }

template< class M1, class M2, 
          class Mo = MonadPlus<typename cata::Cat<M1>::type> >
decltype( Mo::mplus(declval<M1>(),declval<M2>()) )
mplus( M1&& a, M2&& b ) {
    return Mo::mplus( forward<M1>(a), forward<M2>(b) );
}

template< class Seq >
struct MonadPlus< cata::sequence<Seq> > {
    static Seq mzero() { return Seq(); }

    template< class SX, class SY >
    static decltype( append(declval<SX>(),declval<SY>()) )
    mplus( SX&& sx, SY&& sy ) {
        return append( forward<SX>(sx), forward<SY>(sy) );
    }
};

template< class M > 
struct MonadPlus< cata::maybe<M> > {
    static M mzero() { return nullptr; }

    template< class A, class B >
    static decltype(declval<A>() || declval<B>())
    mplus( A&& a, B&& b ) { return forward<A>(a) || forward<B>(b); }
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
        -> decltype( f(declval<Args>()..., declval<Arg1>()) )
    {
        return f( forward<Args>(args)..., forward<Arg1>(arg1) );
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
 * f( x, y, a... ) = f( l(z), r(z), a... ) = g(z,a...)
 *  where x = l(z) and y = r(z)
 * join( f, l, r ) = g
 *
 * Similar to bcompose, but expects a unary l and r and an nary f.
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
Container filter( F&& f, Container cont )
{
    cont.erase ( 
        remove_if( begin(cont), end(cont), 
                   compose( [](bool b){return !b;}, forward<F>(f) ) ),
        end( cont )
    );
    return cont;
}

/* find pred xs -> Maybe x */
template< class F, class S,
          class Val = decltype( &( * begin(declval<S>()) ) ) >
Val find( F&& f, const S& s )
{
    const auto& e = end(s);
    const auto it = find_if( begin(s), e, forward<F>(f) );
    return it != e ? &(*it) : nullptr; 
}

/* cfind x C -> C::iterator */
template< typename S, typename T >
constexpr auto cfind( T&& value, const S& cont )
    -> decltype( begin(cont) )
{
    return find( begin(cont), end(cont), forward<T>(value) );
}

template< typename S, typename F >
constexpr auto cfind_if( F&& f, const S& cont )
    -> decltype( begin(cont) )
{
    return find_if( begin(cont), end(cont), forward<F>(f) );
}

/* all f C -> true when f(x) is true for all x in C; otherwise false. */
template< typename Container, typename F >
constexpr bool all( F&& f, const Container& cont )
{
    return all_of( begin(cont), end(cont), forward<F>(f) );
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
template< typename X, typename F, typename ... Fs,
          typename R = decltype( declval<F>()(declval<X>()) ) >
constexpr array<R,sizeof...(Fs)+1> cleave( X&& x, F&& f, Fs&& ... fs ) 
{
    return {{ forward<F> (f )( forward<X>(x) ), 
              forward<Fs>(fs)( forward<X>(x) )... }};
}

/* cleave_with f x y z =  { f(x), f(y), f(z) } */
template< class F, class A, class ...B >
constexpr auto cleave_with( F&& f, A&& a, B&& ...b )
    -> array< decltype( declval<F>()(declval<A>()) ), sizeof...(B)+1 > 
{
    return {{ f(forward<A>(a)), f(forward<B>(b))... }};
}

template< class T, unsigned int N, class F >
array<T,N> generate( F&& f ) {
    array<T,N> cont;
    generate( begin(cont), end(cont), forward<F>(f) );
    return cont;
}

template< class T, class F >
vector<T> generate( F&& f, unsigned int n ) {
    vector<T> c; 
    c.reserve(n);
    while( n-- )
        c.push_back( forward<F>(f)() );
    return c;
}

template< class Cmp, class S, class R = decltype( begin(declval<Cmp>()) ) >
constexpr R max( Cmp&& cmp, const S& cont ) {
    return max_element( begin(cont), end(cont), forward<Cmp>(cmp) );
}

template< class S >
constexpr auto max( const S& cont ) -> decltype( begin(cont) ) {
    return max_element( begin(cont), end(cont) );
}

template< class Cmp, class S >
constexpr auto min( Cmp&& cmp, const S& cont ) -> decltype( begin(cont) ) {
    return min_element( begin(cont), end(cont), forward<Cmp>(cmp) );
}

template< class Container >
constexpr auto min( const Container& cont )
    -> decltype( begin(cont) )
{
    return min_element( begin(cont), end(cont) );
}

} // namespace pure

