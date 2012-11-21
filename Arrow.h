
#pragma once

#include "Pure.h"

namespace pure {

namespace arrow {

using namespace category;

template< class ... > struct Arrow;

template< class A, class F, class Arr = Arrow< Cat<A> > >
constexpr auto arr( F&& f ) ->  decltype( Arr::arr( declval<F>() ) )
{
    return Arr::arr( forward<F>(f) );
}

template< template<class...> class A, class F  >
constexpr auto arr( F f ) -> decltype( arr<A<F>>( declval<F>() ) )
{
    // TODO: Untested.
    return arr<A<F>>( move(f) );
}

template< class A > struct Arr {
    template< class F >
    constexpr auto operator () ( F&& f ) -> decltype( arr(declval<F>()) )
    {
        return arr( forward<F>(f) );
    }
};

/* (f *** g) (x,y) = (f x, g y) */
constexpr struct Split : Chainable<Split> {
    using Chainable<Split>::operator();

    template< class F, class G, class A = Arrow<Cat<F>> >
    constexpr auto operator () ( F&& f, G&& g )
        -> decltype( A::split(declval<F>(), declval<G>()) )
    {
        return A::split( forward<F>(f), forward<G>(g) );
    }
} split{};

/* (f &&& g) x = (f x, g x) */
constexpr struct Fan : Chainable<Fan> {
    using Chainable<Fan>::operator();

    template< class F, class G, class A = Arrow<Cat<F>> >
    constexpr auto operator () ( F&& f, G&& g )
        -> decltype( A::fan(declval<F>(),declval<G>()) )
    {
        return A::fan( forward<F>(f), forward<G>(g) );
    }
} fan{};

template< class F, class G >
auto operator * ( F&& f, G&& g ) 
    -> decltype( split(declval<F>(),declval<G>()) )
{
    return split( forward<F>(f), forward<G>(g) );
}

template< class F, class G >
auto operator && ( F&& f, G&& g ) 
    -> decltype( fan(declval<F>(),declval<G>()) )
{
    return fan( forward<F>(f), forward<G>(g) );
}

/* (first f) (x,y) = (f x, y) */
constexpr struct First {
    template< class F, class A = Arrow<Cat<F>> >
    constexpr auto operator () ( F&& f ) 
        -> decltype( A::first(declval<F>()) ) 
    {
        return A::first( forward<F>(f) );
    }
} first{};

/* (second f) (x,y) = (x, f y) */
constexpr struct Second {
    template< class F, class A = Arrow<Cat<F>> >
    constexpr auto operator () ( F&& f ) -> decltype( A::second(declval<F>()) ) {
        return A::second( forward<F>(f) );
    }
} second{};

constexpr struct Duplicate {
    template< class X, class P = std::pair<X,X> > 
    constexpr P operator() ( const X& x ) {
        return P( x, x );
    }
} duplicate{};

template< class Func > struct Arrow<Func> {
    static constexpr Func arr( Func f ) { return f; }

    /*
     * Note: It would seem that an easier way to define this class might be to
     * have it define only split, and define first, second, and fan in terms
     * of that. This goes against Haskell's version, but I don't see a reason,
     * currently, why it would not be just as generic and more convenient.
     */

    /* split(f,g)(x,y) = { f(x), g(y) } */
    static constexpr auto split = pairCompose;

    /*
     * first(f)(x,y)  = { f(x), y }
     * second(f)(x,y) = { x, f(y) }
     */
    static constexpr auto first  = arrow::split.with( id );
    static constexpr auto second = arrow::split( id );

    /* fan(f,g)(x) = { f(x), g(x) } */
    static constexpr auto fan = fanCompose;
};

template< unsigned int N > struct Nth {
    template< class X >
    constexpr auto operator () ( X&& x )
        -> decltype( std::get<N>(declval<X>()) )
    {
        return std::get<N>( forward<X>(x) );
    }
};

template< class Binary > struct Uncurrier {
    Binary b;

    template< class P >
    constexpr auto operator () ( P&& p )
        -> decltype( b( get<0>(declval<P>()), get<1>(declval<P>()) ) )
    {
        return b( get<0>(forward<P>(p)), get<1>(forward<P>(p)) );
    }
};

constexpr struct Uncurry {
    template< class B >
    constexpr Uncurrier<B> operator () ( B b ) {
        return { move(b) };
    }
} _uncurry{};

constexpr auto uncurry = rcloset( bcompose, Nth<1>(), Nth<0>() );

//template< template<class...>class, class ... > struct Kleisli;

template< template<class...> class M, class F = Id >
struct Kleisli : Forwarder<F> {
    template< class ...G >
    constexpr Kleisli( G&& ...g ) : Forwarder<F>(std::forward<G>(g)...) { }
};

template< template<class...> class M, class F,
          class K = Kleisli<M,F> >
constexpr K kleisli( F f ) {
    return K( std::move(f) );
}

template< template<class...> class M, class F, class G >
struct KleisliComposition {
    F f;
    G g;

    template< class _F, class _G >
    constexpr KleisliComposition( _F&& f, _G&& g )
        : f(std::forward<_F>(f)), g(std::forward<_G>(g))
    {
    }

    template< class ...X >
    using Monad1 = decltype( g( std::declval<X>()... ) );

    template< class ...X >
    constexpr auto operator () ( X&& ...x )
        -> decltype( std::declval<Monad1<X...> >() >>= f )
    {
         return g( std::forward<X>(x)... ) >>= f;
    }
};

template< template<class...> class M >
struct KleisliCompose : Chainable<KleisliCompose<M>> {
    using Chainable<KleisliCompose<M>>::operator();

    template< class F, class G,
              class Komp = KleisliComposition<M,F,G>,
              class K = Kleisli<M,Komp> >
    constexpr K operator () ( F f, G g ) {
        return K( Komp(std::move(f), std::move(g)) );
    }
};

template< template<class...> class M, class ...F >
constexpr auto kleisliCompose( F&& ...f )
    -> decltype( KleisliCompose<M>()(std::declval<F>()...) )
{
    return KleisliCompose<M>()( std::forward<F>(f)... );
}

template< template<class...> class M, class X, class Y >
constexpr auto mreturnPair( X x, Y y )
    -> decltype( mreturn<M>( declval<std::pair<X,Y>>() ) )
{
    return mreturn<M> (
            std::pair<X,Y>{ move(x), move(y) }
    );
}

template< template<class...> class M >
struct MReturnPair : Binary<MReturnPair<M>> {
    using Binary<MReturnPair<M>>::operator();

    template< class X, class Y >
    constexpr auto operator () ( X&& x, Y&& y )
        -> decltype( mreturnPair<M>( declval<X>(),declval<Y>() ) )
    {
        return mreturnPair<M>( forward<X>(x), forward<Y>(y) );
    }
};

template< template<class...> class M, class F >
struct KleisliFirst {
    F f;

    template< class _F >
    constexpr KleisliFirst( _F&& f )
        : f(std::forward<_F>(f))
    {
    }

    template< class X, class Y >
    constexpr auto operator () ( const std::pair<X,Y>& p )
        -> decltype( f( p.first ) >>= MReturnPair<M>().with( p.second ) )
    {
         return f( p.first ) >>= MReturnPair<M>().with( p.second );
    }
};

template< template<class...> class M, class F >
constexpr auto kleisliFirst( F f ) -> KleisliFirst<M,F> {
    return KleisliFirst<M,F>( move(f) );
}

template< template<class...> class M, class F >
struct KleisliSecond {
    F f;

    template< class _F >
    constexpr KleisliSecond( _F&& f )
        : f(std::forward<_F>(f))
    {
    }

    template< class X, class Y >
    constexpr auto operator () ( const std::pair<X,Y>& p )
        -> decltype( f(p.second) >>= MReturnPair<M>()(p.first) )
    {
         return f( p.second ) >>= MReturnPair<M>()( p.first );
    }
};

template< template<class...> class M, class F >
constexpr auto kleisliSecond( F f ) -> KleisliSecond<M,F> {
    return KleisliSecond<M,F>( move(f) );
}

} // namespace arrow

namespace category {

template< template<class...> class M, class F >
struct Category< arrow::Kleisli<M,F> > {
    template< class G >
    using K = arrow::Kleisli<M,G>;

    template< class G >
    using Ret = decltype( comp(monad::MReturn<M>(),std::declval<G>()) );

    template< class G >
    using KRet = K< Ret<G> >;

    template< class G >
    static constexpr KRet<F> id( G&& g ) {
        return KRet<F>( comp(monad::MReturn<M>(), std::forward<G>(g)) );
    }

    template< template<class...> class _M, class _F, class _G >
    using Komp = arrow::KleisliComposition<_M,_F,_G>;

    template< template<class...> class _M, class _F, class _G,
              class Kl = K< Komp<_M,_F,_G> > >
    static constexpr auto comp( arrow::Kleisli<_M,_F> f,
                                arrow::Kleisli<_M,_G> g )
        -> K< Komp<_M,_F,_G> >
    {
        return arrow::kleisliCompose<M>( std::move(f.f), std::move(g.f) );
    }
};

} // namespace category

namespace arrow {

template< template<class...> class M, class F >
struct Arrow< Kleisli<M,F> > {
    template< class G >
    using Comp = decltype (
            comp( monad::MReturn<M>(), std::declval<G>() )
    );

    template< class G >
    using K = Kleisli<M,G>;

    template< class G >
    static constexpr auto arr( G g ) -> Kleisli< M, Comp<G> > {
        return kleisli<M>( comp(monad::MReturn<M>(), std::move(g)) );
    }

    template< class G >
    static constexpr auto first( G g ) -> K< KleisliFirst<M,G> > {
        return kleisliFirst<M>( move(g) );
    }

    template< class G >
    static constexpr auto second( G g) -> K< KleisliSecond<M,G> > {
        return kleisliSecond<M>( move(g) );
    }

    template< class _F, class _G >
    static constexpr auto split( _F&& f, _G&& g )
        -> decltype( first(declval<_F>()) > second(declval<_G>()) )
    {
        return first(forward<_F>(f)) > second(forward<_G>(g));
    }

    template< class _F, class _G >
    static constexpr auto fan( _F&& f, _G&& g )
        -> decltype( arr(duplicate) > declval<_F>() * declval<_G>() )
    {
        return arr(duplicate) > forward<_F>(f) * forward<_G>(g);
    }
};

}  // namespace arrow

}
