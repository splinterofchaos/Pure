
#include "Pure.h"

#pragma once

namespace pure {

namespace arr {

template< class ... > struct Arrow;

/* f > g = g . f (Haskell's >>>.) */
template< class F, class G >
auto operator > ( F&& f, G&& g ) 
    -> decltype( compose(declval<G>(),declval<F>()) )
{
    return compose( forward<G>(g), forward<F>(f) );
}

/* f < g = f . g (Haskell's <<<.) */
template< class F, class G >
auto operator < ( F&& f, G&& g ) 
    -> decltype( compose(declval<F>(),declval<G>()) )
{
    return compose( forward<F>(f), forward<G>(g) );
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

    template< class F >
    using type = decltype( First()( declval<F>() ) );
} first{};

/* (second f) (x,y) = (x, f y) */
template< class F, class A = Arrow<F> >
auto second( F&& f ) -> decltype( A::second(declval<F>()) ) {
    return A::second( forward<F>(f) );
}

constexpr struct Splitter {
    template< class X, class P = std::pair<X,X> > 
        constexpr P operator() ( const X& x ) {
            return P( x, x );
        }
} splitter{};

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

    template< class F, class G > static 
    constexpr auto fan( F&& f, G&& g ) 
        -> decltype( compose(declval< Split<F,G> >(), Splitter()) )
    {
        return compose( pairCompose( forward<F>(f), forward<G>(g) ), 
                        Splitter() );
    }
};

template< class Binary > struct Unsplit {
    Binary b;

    template< class P >
    constexpr auto operator () ( P&& p )
        -> decltype( b( get<0>(declval<P>()), get<1>(declval<P>()) ) )
    {
        return b( get<0>(forward<P>(p)), get<1>(forward<P>(p)) );
    }
};

constexpr struct ReturnUnsplit {
    template< class B > 
    constexpr Unsplit<B> operator () ( B b ) {
        return { move(b) };
    }
} unsplit{};

}

}