
#pragma once

#include "Data.h"
#include "List.h"
#include "Category.h"

namespace pure {

namespace monad {

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
struct Functor< std::pair<X,Y> > {
    template< unsigned N, class P >
    using Nth = decltype( get<N>( declval<P>() ) );

    template< class F, class ...P >
    using PX = decltype( declval<F>()( declval<Nth<0,P>>()... ) );
    template< class F, class ...P >
    using PY = decltype( declval<F>()( declval<Nth<1,P>>()... ) );

    template< class F, class ...P >
    static constexpr auto fmap( F&& f, P&& ...p ) 
        -> decltype (
            std::make_pair( declval<PX<F,P...>>(), declval<PY<F,P...>>() )
        )
    {
        return std::make_pair( forward<F>(f)(forward<P>(p).first...),
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
        -> decltype( data::Just(declval<F>()( *declval<M>()... )) )
    {
        return each( forward<M>(m)... ) ? 
            data::Just( forward<F>(f)( *forward<M>(m)... ) ) : nullptr;
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
struct Functor< data::Either<L,R> > {
    template< class F, class FR = decltype( declval<F>()(declval<R>()) ) >
    static data::Either<L,FR>  fmap( F&& f, const data::Either<L,R>& e ) {
        return e.right ? data::Right<L>( forward<F>(f)(*e.right) ) 
                       : data::Left<FR>( *e.left );
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

template< template<class...>class M, class X > 
M<X> mreturn( X&& x ) {
    return Monad< Cat<M<X>> >::template mreturn<M<X>>( forward<X>(x) );
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

    static constexpr data::ReturnJust mreturn() { return data::ReturnJust(); }

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

} // namespace monad

} // namespace pure
