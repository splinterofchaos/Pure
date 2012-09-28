
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
#include <limits>

#include <iostream>

#include "Common.h"
#include "List.h"
#include "Functional.h"

namespace pure {

using namespace std;

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

struct other {};

/*
 * Maybe 
 * Any type that: 
 *      Can be dereferenced like so: *Maybe(). (fromJust).
 *      Can be converted to a boolean. (isJust/isNothing)
 *      But is not a function
 */
struct maybe {};

// If we want to return a pointer that defines ownership, we want a smart
// pointer. Consider any non-raw pointer good 'nuff.
template< class Ptr > struct SmartPtr { using type = Ptr; };
template< class X > struct SmartPtr<X*> { using type = unique_ptr<X>; };

template< class Ptr > struct maybe_traits {
    using pointer    = Ptr;
    using smart_ptr  = typename SmartPtr<pointer>::type;
    using reference  = decltype( *declval<pointer>() );
    using value_type = Decay<reference>;
};

/*
 * cat performs the actual type deduction via its argument. If no other
 * definition of cat is valid, it returns a variable of the same type as its
 * argument. It is only for use in decltype expressions.
 */
template< class X >
X cat( ... );

template< class S >
auto cat( const S& s ) -> decltype( begin(s), end(s), sequence() );

template< class M >
auto cat( const M& m ) -> decltype( *m, (bool)m, maybe() );

template< class T > struct Cat {
    using type = decltype( cat<Decay<T>>(declval<T>()) );
};

// Prevent function pointers from being deduced as maybe types.
template< class R, class ...X > struct Cat< R(&)(X...) > {
    typedef R(&type)(X...);
};

} // namespace cata

template< class X >
using Cat = typename cata::Cat<X>::type;

template< class ... > struct Category;

template< class X, class C = Category<X> >
auto cid( X&& x ) -> decltype( C::id( declval<X>() ) ) {
    return C::id( forward<X>(x) );
}

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
decltype( A::fan(declval<F>(),declval<G>()) )
fan( F&& f, G&& g ) {
    return A::fan( forward<F>(f), forward<G>(g) );
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

    // Same decltype expression used many times. Save typing.
    template< class F, class G >
    using Split = decltype( pairCompose(declval<F>(), declval<G>()) );

    /*
     * Note: It would seem that an easier way to define this class might be to
     * have it define only split, and define first, second, and fan in terms
     * of that. This goes against Haskell's version, but I don't see a reason,
     * currently, why it would not be just as generic and more convenient.
     */

    template< class F, class G > static 
    constexpr Split<F,G> split ( F&& f, G&& g ) 
    {
        return pairCompose( forward<F>(f), forward<G>(g) );
    }

    template< class F > static 
    constexpr Split<F,Id> first( F&& f ) 
    { 
        return pairCompose( forward<F>(f), Id() );
    }

    template< class F > static 
    constexpr Split<Id,F> second( F&& f ) 
    {
        return pairCompose( Id(), forward<F>(f) );
    }

    struct Splitter {
        template< class X, class P = pair<X,X> > 
            constexpr P operator() ( const X& x ) {
                return P( x, x );
            }
    };

    template< class F, class G > static 
    constexpr auto fan( F&& f, G&& g ) 
        -> decltype( compose(declval< Split<F,G> >(), Splitter()) )
    {
        return compose( pairCompose( forward<F>(f), forward<G>(g) ), 
                        Splitter() );
    }
};

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

struct ReturnJust {
    template< class X >
    unique_ptr<Decay<X>> operator() ( X&& x ) {
        return Just( forward<X>(x) );
    }
};

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
          class Ret = decltype( Just( (*declval<F>())(*declval<P>()) ) ) >
Ret operator* ( F&& a, P&& b ) {
    return a and b ? Just( (*forward<F>(a))(*forward<P>(b)) ) 
                   : nullptr;
}

template< class P > 
constexpr P operator|| ( P&& a, P&& b ) {
    return a ? forward<P>(a) : forward<P>(b); 
}

template< class P, class R = decltype( Just(*declval<P>()) ) > 
constexpr R operator|| ( const P& a, const P& b ) {
    return a ? Just(*a) : b ? Just(*b) : nullptr;
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

template< class F, class G, class ...H,
          class Fn = Functor< Cat<G> > >
constexpr auto fmap( F&& f, G&& g, H&& ...h )
    -> decltype( Fn::fmap(declval<F>(),declval<G>(),declval<H>()...) ) 
{
    return Fn::fmap( forward<F>(f), forward<G>(g), forward<H>(h)... );
}

template< class F, class X,
          class Fn = Functor< cata::sequence > >
constexpr auto fmap( F&& f, const std::initializer_list<X>& l )
    -> decltype( Fn::fmap(declval<F>(),l) ) 
{
    return Fn::fmap( forward<F>(f), l );
}

struct FMap {
    template< class F, class ...X >
    constexpr auto operator() ( F&& f, X&& ...x ) 
        -> decltype( fmap(declval<F>(),declval<X>()...) )
    {
        return fmap( forward<F>(f), forward<X>(x)... );
    }
};

template< class F > 
constexpr Closure<FMap,F> fmap( F&& f ) {
    return closure( FMap(), forward<F>(f) );
}

/* fmap f g = compose( f, g ) */
template< class Function >
struct Functor<Function> {
    template< class F, class G >
    static auto fmap( F&& f, G&& g ) 
        -> decltype( compose(declval<F>(),declval<G>()) )
    {
        return compose( forward<F>(f), forward<G>(g) );
    }
};

/* fmap f Pair(x,y) = Pair( f x, f y ) */
template< class X, class Y >
struct Functor< pair<X,Y> > {
    template< unsigned N, class P >
    using Nth = decltype( get<N>( declval<P>() ) );

    template< class F, class ...P >
    using PX = decltype( declval<F>()( declval<Nth<0,P>>()... ) );
    template< class F, class ...P >
    using PY = decltype( declval<F>()( declval<Nth<1,P>>()... ) );

    template< class F, class ...P >
    static constexpr auto fmap( F&& f, P&& ...p ) 
        -> decltype (
            make_pair( declval<PX<F,P...>>(), declval<PY<F,P...>>() )
        )
    {
        return make_pair( forward<F>(f)(forward<P>(p).first...),
                  forward<F>(f)(forward<P>(p).second...) );
    }
};

template<>
struct Functor< cata::maybe > {
    template< class M > 
    static constexpr bool each( const M& m ) {
        return (bool)m;
    }

    template< class M1, class ...Ms >
    static constexpr bool each( const M1& m1, const Ms& ...ms ) {
        return (bool)m1 && each( ms... );
    }

    /*
     * f <$> Just x = Just (f x)
     * f <$> Nothing = Nothing
     */
    template< class F, class ...M >
    static constexpr auto fmap( F&& f, M&& ...m ) 
        -> decltype( Just(declval<F>()( *declval<M>()... )) )
    {
        return each( forward<M>(m)... ) ? 
            Just( forward<F>(f)( *forward<M>(m)... ) ) : nullptr;
    }
};

template<>
struct Functor< cata::sequence > {
    /* f <$> [x0,x1,...] = [f x0, f x1, ...] */
    template< class F, class ...S >
    static constexpr decltype( list::map(declval<F>(),declval<S>()...) )
    fmap( F&& f, S&& ...s ) {
        return list::map( forward<F>(f), forward<S>(s)... );
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

/* f <$> m */
template< class F, class M >
constexpr auto operator^ ( F&& f, M&& m ) 
    -> decltype( fmap(declval<F>(), declval<M>()) )
{
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
template< class > struct Monoid;

template< class M, class Mo = Monoid<Cat<M>> >
decltype( Mo::mempty() ) mempty() { return Mo::mempty(); }

template< class M1, class M2, 
          class Mo = Monoid<Cat<M1>> >
auto mappend( M1&& a, M2&& b ) 
    -> decltype( Mo::mappend(declval<M1>(),declval<M2>()) )
{
    return Mo::mappend( forward<M1>(a), forward<M2>(b) );
}

/*
 * mappend const& 
 * The above && version will always be preferred, except in the case of a
 * const&. In other words, this version will NEVER be preferred otherwise.
 */

template< class S, class V = list::SeqVal<S>, class M = Monoid<Cat<V>> >
auto mconcat( S&& s ) -> decltype( M::mconcat(declval<S>()) ) {
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

template<> struct Monoid< cata::sequence > {
    template< class S >
    static S mempty() { return S{}; }

    template< class XS, class YS >
    static XS mappend( XS xs, YS&& ys ) {
        return list::append( move(xs), forward<YS>(ys) );
    }

    template< class SS >
    static auto mconcat( SS&& ss ) -> decltype( concat(declval<SS>()) )
    {
        return concat( forward<SS>(ss) ); 
    }
};

/* Monoid (Maybe X) -- where X is a monoid. */
template<> struct Monoid< cata::maybe > {
    template< class M >
    static constexpr M mempty() { return nullptr; }

    template< class M >
    static M dup( const M& m ) {
        return maybe( mempty<M>(), ReturnJust(), m );
    }

    template< class X >
    using ERVal = ERVal<X,Decay<X>>;

    template< class M >
    constexpr static ERVal<M> dup( M&& m ) {
        return m;
    }

    /*
     * Just x <> Just y = Just (x <> y)
     *      a <> b      = a | b
     */
    template< class M >
    static constexpr Decay<M> mappend( M&& x, M&& y ) {
        return x and y ? Just( fwd_mappend(*forward<M>(x), *forward<M>(y)) )
            : dup(forward<M>(x) || forward<M>(y));
    }

    /* mconcat [Just x, Just y, Nothing] = Just (x <> y <> Nothing)*/
    template< class S, class M = list::SeqRef<S>, class R = Decay<M> > 
    static R mconcat( S&& s ) {
        using F = R (*) ( const M&, const M& );
        return list::foldl( (F)mappend, mempty<R>(), forward<S>(s) );
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
        return list::foldr( mappend, mempty(), s );
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
          class Mo = Monad<Cat<A>> >
decltype( Mo::mdo(declval<A>(),declval<B>()) ) 
mdo( A&& a, B&& b ) {
    return Mo::mdo( forward<A>(a), forward<B>(b) ); 
}

/* m >>= k */
template< class M, class F,
          class Mo = Monad<Cat<M>> >
auto mbind( M&& m, F&& f ) 
    -> decltype( Mo::mbind(declval<M>(),declval<F>()) ) 
{
    return Mo::mbind( forward<M>(m), forward<F>(f) ); 
}

/* return<M> x = M x */
template< class M, class X > 
M mreturn( X&& x ) {
    return Monad< Cat<M> >::template mreturn<M>( forward<X>(x) );
}

/* mreturn () = (\x -> return x) */
template< class M, class Mo = Monad<Cat<M>> > 
constexpr auto mreturn() -> decltype( Mo::mreturn() ) { 
    return Mo::mreturn();
}

template< class M > struct Return {
    template< class X >
    constexpr auto operator() ( X&& x ) 
        -> decltype( mreturn<M>(declval<X>()) ) 
    {
        return mreturn<M>( forward<X>(x) );
    }
};

template< class M >
M mfail( const char* const why ) {
    return Monad< Cat<M> >::template mfail<M>( why );
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

template<> struct Monad< cata::sequence > {
    template< class S, class X >
    constexpr static S mreturn( X&& x ) { 
        return S{ forward<X>(x) }; 
    }

    template< class S >
    constexpr static S mfail(const char*) { return S(); }

    template< class S, class YS >
    static YS mdo( const S& a, const YS& b ) {
        // In Haskell, this is defined as
        //      m >> k = foldr ((++) . (\ _ -> k)) [] m
        // In other words, 
        //      for each element in a, duplicate b.
        //      [] >> k = []
        YS c;
        auto size = list::length( a );
        while( size-- )
            c = list::append( move(c), b );
        return c;
    }

    /* m >>= k -- where m is a sequence. */
    template< class S, class F >
    static decltype( list::concatMap(declval<F>(),declval<S>()) )
    mbind( S&& xs, F&& f ) { 
        // xs >>= f = foldr g [] xs 
        //     where g acc x = acc ++ f(x)
        //           ++ = append
        return list::concatMap( f, xs );
    }
};

template< class P > struct IsPointerImpl { 
    using reference = decltype( *declval<P>() );
    using bool_type = decltype( (bool)declval<P>() );
};

template<> struct Monad< cata::maybe > {
    template< class M > using traits = cata::maybe_traits<M>;
    template< class M > using value_type = typename traits<M>::value_type;
    template< class M > using smart_ptr  = typename traits<M>::smart_ptr;

    template< class M, class X, class P = smart_ptr<M> >
    static P mreturn( X&& x ) { 
        return P( new X(forward<X>(x)) ); 
    }

    template< class M >
    using Return = smart_ptr<M> (*) ( value_type<M> );

    static constexpr ReturnJust mreturn() { return ReturnJust(); }

    template< class M >
    static constexpr smart_ptr<M> mfail(const char*) { return nullptr; }

    template< class M, class PZ >
    static constexpr PZ mdo( const M& x, PZ&& z ) {
        return x ? forward<PZ>(z) : nullptr;
    }

    template< class M, class F >
    using Result = Result< F, value_type<M> >;

    template< class M, class F >
    static constexpr Result<M,F> mbind( M&& x, F&& f ) {
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

template< class M, class Mo = MonadPlus<Cat<M>> >
decltype( Mo::mzero() ) mzero() { return Mo::mzero(); }

template< class M1, class M2, 
          class Mo = MonadPlus<Cat<M1>> >
auto mplus( M1&& a, M2&& b ) 
    -> decltype( Mo::mplus(declval<M1>(),declval<M2>()) )
{
    return Mo::mplus( forward<M1>(a), forward<M2>(b) );
}

template<> struct MonadPlus< cata::sequence > {
    template< class S >
    static S mzero() { return S(); }

    template< class SX, class SY >
    static auto mplus( SX&& sx, SY&& sy ) 
        -> decltype( list::append(declval<SX>(),declval<SY>()) )
    {
        return list::append( forward<SX>(sx), forward<SY>(sy) );
    }
};

template<> struct MonadPlus< cata::maybe > {
    template< class M >
    static M mzero() { return nullptr; }

    template< class A, class B >
    using Result = decltype( declval<A>() || declval<B>() );

    template< class A, class B >
    static constexpr Result<A,B> mplus( A&& a, B&& b ) { 
        return forward<A>(a) || forward<B>(b); 
    }
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
struct PartLast; // Apply the last argument.

template< class F, class Last >
struct PartLast< F, Last > : public Part< F, Last > 
{ 
    template< class _F, class _Last >
    constexpr PartLast( _F&& f, _Last&& last )
        : Part<F,Last>( forward<_F>(f), forward<_Last>(last) )
    {
    }
};

// Remove one argument each recursion.
template< class F, class Arg1, class ...Args >
struct PartLast< F, Arg1, Args... > : public PartLast< F, Args... > 
{
    template< class _F, class _Arg1, class ..._Args >
    constexpr PartLast( _F&& f, _Arg1&&, _Args&& ...args )
        : PartLast<F,Args...>( forward<_F>(f), forward<_Args>(args)... )
    {
    }
};

template< class F, class ...Args >
struct PartInit; // Apply all but the last argument.

template< class F, class Arg1, class Arg2 >
struct PartInit< F, Arg1, Arg2 > : public Part< F, Arg1 >
{
    template< class _F, class _Arg1, class _Arg2 >
    constexpr PartInit( _F&& f, _Arg1&& arg1, _Arg2&& )
        : Part<F,Arg1>( forward<_F>(f), forward<_Arg1>(arg1) )
    {
    }
};

template< class F, class Arg1, class ...Args >
struct PartInit< F, Arg1, Args... > 
: public PartInit< Part<F,Arg1>, Args... >
{
    template< class _F, class _Arg1, class ..._Args >
    PartInit( _F&& f, _Arg1&& arg1, _Args&& ...args )
        : PartInit<Part<F,Arg1>,Args...> (
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
            declval< PartInit<PartLast<F,Args...>,Args...> >()() 
        )
    {
        /* We can't just (to my knowledge) pull the initial and final args
         * apart, so first reverse-apply the last argument, then apply each
         * argument forward until the last. The result is a zero-arity function
         * to invoke.
         */
        return PartInit< PartLast<F,Args...>, Args... > (
            PartLast< F, Args... >( f, forward<Args>(args)... ),
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
constexpr NRot<N,F> nrot( F&& f ) {
    return NRot<N,F>( forward<F>(f) );
}

template< unsigned int N, class F >
constexpr RNRot<N,F> rnrot( F&& f ) {
    return RNRot<N,F>( forward<F>(f) );
}

/* squash[ f(x,x) ] = g(x) */
template< class F >
struct Squash
{
    F f;

    template< class _F >
    constexpr Squash( _F&& f ) : f( forward<_F>(f) ) { }

    template< class X, class ...Y >
    constexpr auto operator() ( X&& x, Y&& ...y )
        -> decltype( f(declval<X>(),declval<X>(),declval<Y>()...) )
    {
        return f( forward<X>(x), forward<X>(x), 
                  forward<Y>(y)... );
    }
};

    template< class F >
constexpr Squash<F> squash( F f )
{
    return Squash<F>( move(f) );
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

    template< class X >
    using L = decltype( l(declval<X>()) );
    template< class X >
    using R = decltype( r(declval<X>()) );

    template< class A, class ...AS >
    constexpr Result< F, L<A>, R<A>, AS... > 
    operator() ( A&& a, AS&& ...as ) {
        return f( l( forward<A>(a) ), 
                  r( forward<A>(a) ), 
                  forward<AS>(as)... );
    }
};

    template< class F, class L, class R >
constexpr Join<F,L,R> join( F&& f, L&& l, R&& r ) {
    return Join<F,L,R>( forward<F>(f), forward<L>(l), forward<R>(r) );
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
constexpr auto min( const Container& cont ) -> decltype( begin(cont) ) {
    return min_element( begin(cont), end(cont) );
}

} // namespace pure

