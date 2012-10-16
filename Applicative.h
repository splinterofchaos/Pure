
#pragma once

#include "Pure.h"

namespace pure {

namespace ap {

template< class ... > struct Applicative;
template< class ... > struct Alternative;

template< class X, class A = Applicative<Cat<X>> >
constexpr auto pure( X&& x ) -> decltype( A::pure(std::declval<X>()) ) {
    return A::pure( std::forward<X>(x) );
}

template< class X, class ...Y, class A = Applicative<Cat<X>> >
constexpr auto ap( X&& x, Y&& ...y ) 
    -> decltype( A::ap(std::declval<X>(),std::declval<Y>()...) )
{
    return A::ap( std::forward<X>(x), std::forward<Y>(y)... );
}

template< class X, class Y >
constexpr auto operator * ( X&& x, Y&& y ) 
    -> decltype( ap(std::declval<X>(),std::declval<Y>()) )
{
    return ap( std::forward<X>(x), std::forward<Y>(y) );
}

template< class X, class A = Alternative< Cat<X> > >
constexpr auto empty() -> decltype( A::empty() ) {
    return A::empty();
}

template< class X, class Y, class A = Alternative< Cat<X> > >
constexpr auto alt( X&& x, Y&& y ) 
    -> decltype( A::alt(std::declval<X>(),std::declval<Y>()) ) 
{
    return A::alt( std::forward<X>(x), std::forward<Y>(y) );
}

template< class X, class Y >
constexpr auto operator || ( X&& x, Y&& y ) 
    -> decltype( alt(std::declval<X>(),std::declval<Y>()) )
{
    return alt( std::forward<X>(x), std::forward<Y>(y) );
}

template<> struct Applicative< cata::maybe > {
    static constexpr auto pure = Just;

    template< class A, class B >
    static constexpr auto ap( A&& a, B&& b ) 
        -> decltype( Just((*std::declval<A>())(*std::declval<B>())) )
    {
        return a and b 
            ? Just( (*std::forward<A>(a))( *std::forward<B>(b) ) )
            : nullptr;
    }
};

template<> struct Alternative< cata::maybe > {
    static constexpr std::nullptr_t empty() { return nullptr; }

    template< class P > 
    static constexpr P alt ( P&& a, P&& b ) {
        return a ? std::forward<P>(a) : std::forward<P>(b); 
    }

    template< class P, class R = decltype( Just(*declval<P>()) ) > 
    static constexpr R alt ( const P& a, const P& b ) {
        return a ? Just(*a) : b ? Just(*b) : nullptr;
    }
};

}

}
