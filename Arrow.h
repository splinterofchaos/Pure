
#pragma once

#include "Pure.h"

namespace pure {

namespace arrow {

using namespace category;

template< class ... > struct Arrow;

struct Arr {
    template< class A, class F, class Arr = Arrow<A> >
    constexpr auto operator () ( F&& f )
        ->  decltype( Arr::arr( declval<F>() ) ) 
    { 
        return Arr::arr( forward<F>(f) ); 
    }
} arr{};

/* (f *** g) (x,y) = (f x, g y) */
constexpr struct Split {
    template< class F, class G, class A = Arrow<F> >
    constexpr auto operator () ( F&& f, G&& g )
        -> decltype( A::split(declval<F>(), declval<G>()) )
    {
        return A::split( forward<F>(f), forward<G>(g) );
    }
} split{};

/* (f &&& g) x = (f x, g x) */
constexpr struct Fan {
    template< class F, class G, class A = Arrow<F> >
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
    template< class F, class A = Arrow<F> >
    constexpr auto operator () ( F&& f ) 
        -> decltype( A::first(declval<F>()) ) 
    {
        return A::first( forward<F>(f) );
    }
} first{};

/* (second f) (x,y) = (x, f y) */
constexpr struct Second {
    template< class F, class A = Arrow<F> >
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

    // Same decltype expression used many times. Save typing.
    template< class F, class G >
    using Split = decltype( pairCompose(declval<F>(), declval<G>()) );

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
    static constexpr auto first  = rcloset( split, Id() );
    static constexpr auto second = closet ( split, Id() );

    /* fan(f,g)(x) = { f(x), g(x) } */
    static constexpr auto fan = fanCompose;
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
} uncurry{};

template< template<class...> class M, class F > struct Kleisli {
    F f = F();

    constexpr Kleisli( F f ) : f(std::move(f)) { }

    template< class X >
    constexpr auto operator () ( X&& x )
        -> decltype( f(std::declval<X>()) )
    {
        return f( std::forward<X>(x) );
    }
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

template< template<class...> class M > struct KleisliCompose {
    template< class F, class G,
              class Komp = KleisliComposition<M,F,G>,
              class K = Kleisli<M,Komp> >
    constexpr K operator () ( F f, G g ) {
        return K( Komp(std::move(f), std::move(g)) );
    }
};

template< template<class...> class M, class F, class G >
auto kleisliCompose( F&& f, G&& g )
    -> decltype( KleisliCompose<M>()(std::declval<F>(),std::declval<G>()) )
{
    return KleisliCompose<M>()( std::forward<F>(f), std::forward<G>(g) );
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

}
