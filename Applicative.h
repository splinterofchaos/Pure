
#pragma once

#include "Arrow.h"
#include "Monoid.h"

#include <array>

namespace pure {

namespace ap {

constexpr struct SPure {
    template< class X, class ...Y, class A = std::array<Decay<X>,1+sizeof...(Y)> >
    constexpr A operator () ( X&& x, Y&& ...y ) {
        return {{
            std::forward<X>(x),
            std::forward<Y>(y)...
        }};
    }
} spure{};

template< class ... > struct Applicative;
template< class ... > struct Alternative;

template< template<class...>class M, class X, class A = Applicative<Cat<M<X>>> >
constexpr auto pure( X&& x ) 
    -> decltype( A::template pure<M>(std::declval<X>()) ) 
{
    return A::template pure<M>( std::forward<X>(x) );
}

// apure -- To disambiguate between the function and namespace.
template< template<class...>class M, class X, class A = Applicative<Cat<M<X>>> >
constexpr auto apure( X&& x ) 
    -> decltype( A::template pure<M>(std::declval<X>()) ) 
{
    return A::template pure<M>( std::forward<X>(x) );
}

template< template<class...> class M > struct Pure {
    template< class X >
    auto operator () ( X&& x ) const -> decltype(apure<M>(declval<X>()))
    {
        return apure<M>( forward<X>(x) );
    }
};

struct Ap : Binary<Ap> {
    using Binary<Ap>::operator();

    template< class X, class ...Y, class A = Applicative<Cat<X>> >
    constexpr auto operator () ( X&& x, Y&& ...y )
        -> decltype( A::ap(std::declval<X>(),std::declval<Y>()...) )
    {
        return A::ap( std::forward<X>(x), std::forward<Y>(y)... );
    }
} ap{};

template< class X, class A = Alternative< Cat<X> > >
constexpr auto empty() -> decltype( A::template empty<X>() ) {
    return A::template empty<X>();
}

template< class X > struct Empty {
    constexpr auto operator () () -> decltype( empty<X>() ) {
        return empty<X>();
    }
};

constexpr struct Alt : Chainable<Alt> {
    using Chainable<Alt>::operator();

    template< class X, class Y, class A = Alternative< Cat<X> > >
    constexpr auto operator () ( X&& x, Y&& y )
        -> decltype( A::alt(std::declval<X>(),std::declval<Y>()) )
    {
        return A::alt( std::forward<X>(x), std::forward<Y>(y) );
    }
} alt{};

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

template< class X, class Y >
constexpr auto operator || ( X&& x, std::initializer_list<Y> l ) 
    -> decltype( alt(std::declval<X>(),std::move(l)) )
{
    return alt( std::forward<X>(x), std::move(l) );
}

template<> struct Applicative< category::maybe_type > {
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

constexpr struct Call : Binary<Call> {
    using Binary<Call>::operator();

    template< class F, class ...X >
    constexpr auto operator () ( F&& f, X&& ...x ) -> Result<F,X...> {
        return std::forward<F>(f)( std::forward<X>(x)... );
    }
} call{};
    

template<> struct Applicative< category::sequence_type > {
    template< template<class...>class S, class X >
    static constexpr S<Decay<X>> pure( X&& x ) {
        return S<Decay<X>>{ std::forward<X>(x) };
    }

    static Part<FMap,Call> ap;
};

Part<FMap,Call> Applicative< category::sequence_type >::ap = fmap(call);

template< class _X, class _Y > struct Applicative< std::pair<_X,_Y> > {
    template< class P, class X > 
    static constexpr P pure( X&& x ) {
        return { {}, std::forward<X>(x) };
    }

    template< class U, class V, class F, class X >
    static constexpr auto ap( std::pair<U,F> a, const std::pair<V,X>& b )
        -> std::pair<U,Result<F,X>>
    {
        using monoid::operator+;
        return { move(std::get<0>(a)) + std::get<0>(b),
                 std::get<1>(a)( std::get<1>(b) ) };
    }
};


template<> struct Alternative< category::maybe_type > {
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

template<> struct Alternative< category::sequence_type > {
    template< class S >
    static constexpr S empty() { return {}; }

    static constexpr auto alt = list::append;
};

}

}
