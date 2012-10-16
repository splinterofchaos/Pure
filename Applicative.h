
#pragma once

#include "Pure.h"

namespace pure {

namespace ap {

template< class ... > struct Applicative;
template< class ... > struct Alternative;

template< template<class...>class M, class X, class A = Applicative<Cat<M<X>>> >
constexpr auto pure( X&& x ) 
    -> decltype( A::template pure<M>(std::declval<X>()) ) 
{
    return A::template pure<M>( std::forward<X>(x) );
}

template< class X, class ...Y, class A = Applicative<Cat<X>> >
constexpr auto ap( X&& x, Y&& ...y ) 
    -> decltype( A::ap(std::declval<X>(),std::declval<Y>()...) )
{
    return A::ap( std::forward<X>(x), std::forward<Y>(y)... );
}

template< class X, class A = Alternative< Cat<X> > >
constexpr auto empty() -> decltype( A::template empty<X>() ) {
    return A::template empty<X>();
}

template< class X, class Y, class A = Alternative< Cat<X> > >
constexpr auto alt( X&& x, Y&& y ) 
    -> decltype( A::alt(std::declval<X>(),std::declval<Y>()) ) 
{
    return A::alt( std::forward<X>(x), std::forward<Y>(y) );
}

template< class X, class Y >
constexpr auto operator * ( X&& x, Y&& y ) 
    -> decltype( ap(std::declval<X>(),std::declval<Y>()) )
{
    return ap( std::forward<X>(x), std::forward<Y>(y) );
}

template< class X, class Y >
constexpr auto operator || ( X&& x, Y&& y ) 
    -> decltype( alt(std::declval<X>(),std::declval<Y>()) )
{
    return alt( std::forward<X>(x), std::forward<Y>(y) );
}

template<> struct Applicative< cata::maybe > {
    template< template<class...>class Ptr, class X >
    static Ptr<Decay<X>> pure( X&& x ) {
        return Just( std::forward<X>(x) );
    }

    template< class A, class B >
    static constexpr auto ap( A&& a, B&& b ) 
        -> decltype( Just((*std::declval<A>())(*std::declval<B>())) )
    {
        return a and b 
            ? Just( (*std::forward<A>(a))( *std::forward<B>(b) ) )
            : nullptr;
    }
};

constexpr struct Call {
    template< class F, class X >
    constexpr auto operator () ( F&& f, X&& x ) -> Result<F,X> {
        return std::forward<F>(f)( std::forward<X>(x) );
    }
} call{};
    

template<> struct Applicative< cata::sequence > {
    template< template<class...>class S, class X >
    static constexpr S<Decay<X>> pure( X&& x ) {
        return S<Decay<X>>{ std::forward<X>(x) };
    }

    static constexpr auto ap = fmap(call);
};

template<> struct Alternative< cata::maybe > {
    template< class Ptr >
    static constexpr Ptr empty() { return nullptr; }

    template< class P > 
    static constexpr P alt ( P&& a, P&& b ) {
        return a ? std::forward<P>(a) : std::forward<P>(b); 
    }

    template< class P, class R = decltype( Just(*declval<P>()) ) > 
    static constexpr R alt ( const P& a, const P& b ) {
        return a ? Just(*a) : b ? Just(*b) : nullptr;
    }
};

template<> struct Alternative< cata::sequence > {
    template< class S >
    static constexpr S empty() { return {}; }

    template< class X, class Y >
    static auto alt( X&& x, Y&& y )
        -> decltype( list::append(std::declval<X>(),std::declval<Y>()) )
    {
        return list::append( std::forward<X>(x), std::forward<Y>(y) );
    }
};

}

}
