
#pragma once

#include <utility>

#include "Functional.h"

namespace pure {

namespace tpl {

template< size_t ... > struct LastIndex;

template< size_t X > struct LastIndex<X> {
    static constexpr size_t I = X;
};

template< size_t X, size_t ...Y > struct LastIndex<X,Y...> {
    static constexpr size_t I = LastIndex<Y...>::I;
};

template< size_t ...i > struct IndexList {
    static constexpr size_t I = LastIndex<i...>::I;
    using Next = IndexList< i..., I+1 >;
};

template< size_t n > struct IListBuilder {
    using type = typename IListBuilder< n-1 >::type::Next;
};

template<> struct IListBuilder<0> {
    using type = IndexList<0>;
};

template< size_t N > using BuildList = typename IListBuilder<N-1>::type;

constexpr auto pair  = MakeBinaryT<std::pair>();
constexpr auto tuple = MakeT<std::tuple>();

template< size_t I, class X >
X xOfI( const X& x ) {
    return x;
}

template< class X, size_t ...I >
constexpr auto _repeat( const X& x, IndexList<I...> )
    -> decltype( tuple( xOfI<I>(x)... ) )
{
    return tuple( xOfI<I>(x)... );
}

template< size_t I, class X >
constexpr auto repeat( const X& x )
    -> decltype( _repeat(x,BuildList<I>()) )
{
    return _repeat( x, BuildList<I>() );
}

constexpr struct tupleZip {
    template< class F, class ...X, size_t ...I,
              class R = std::tuple< Result<F,X,X>... > >
    R operator () ( F f, const std::tuple<X...>& xs,
                         const std::tuple<X...>& ys,
                         IndexList<I...> ) const
    {
        return R( f( std::get<I>(xs), std::get<I>(ys) )... );
    }

    template< class F, class ...X,
              class R = std::tuple< decltype(declval<F>()(declval<X>(),declval<X>()))... > >
    R operator () ( F f, const std::tuple<X...>& xs,
                         const std::tuple<X...>& ys) const
    {
        const auto N = sizeof ...(X);
        return (*this)( std::move(f), xs, ys, typename IListBuilder<N-1>::type() );
    }
} tupleZip{};

/*
 * fork(xs,pred0,pred1,...,predn)
 *
 * fork is similar to list::span, but flipped and variadic.
 *
 * Returns a tuple, {leftovers,xs0,xs1,...,xsn}, where xsN is every x from xs
 * such that predN(x) is true. leftovers contains every x for which no
 * predicate is true.
 */
constexpr struct Fork : Binary<Fork> {
    using Binary<Fork>::operator();

    template< class XS, class ...P >
    using Forked = decltype (
        tuple_cat( tuple(declval<XS>()), // leftovers
                   repeat<sizeof...(P)>(XS()) /* {xs0,xs1,...} */ )
    );

    // First, convert ps... to a tuple.
    template< class XS, class ...P >
    Forked<XS,P...> operator () ( const XS& xs, P&& ...ps ) const {
        return doFork( xs, std::forward_as_tuple(ps...) );
    }

    // Build the results.
    template< class XS, class ...P >
    static Forked<XS,P...> doFork( const XS& xs, const std::tuple<P...>& ps ) {
        Forked<XS,P...> r;
        for( const auto& x : xs )
            doSendWhen<0>( x, r, ps );
        return r;
    }

    // Check each predicate and send it to the correct resultant.
    // If nothing matched, send x to the leftovers.
    template< size_t N, class ...P, class X, class ...XS >
    static auto doSendWhen( X&& x, std::tuple<XS...>& dsts,
                            const std::tuple<P...>& )
        -> typename std::enable_if<(N==sizeof...(P)), void >::type
    {
        // Nothing matched.
        std::get<0>(dsts).emplace_back( forward<X>(x) );
    }

    template< size_t N, class ...P, class X, class ...XS >
    static auto doSendWhen( X&& x, std::tuple<XS...>& dsts,
                      const std::tuple<P...>& preds )
        -> typename std::enable_if<(N<sizeof...(P)), void >::type
    {
        if( std::get<N>(preds)(x) ) {
            std::get<N+1>(dsts).emplace_back( forward<X>(x) );
        } else {
            doSendWhen<N+1>( forward<X>(x), dsts, preds );
        }
    }
} fork{};

constexpr struct Map {
    template< class F, class ...X, size_t ...I >
    constexpr auto doMap( F f, const std::tuple<X...>& ts,
                          IndexList<I...> )
        -> decltype( tuple( f(std::get<I>(ts))... ) )
    {
        return tuple( f(std::get<I>(ts))... );
    }

    template< class F, class ...X,
              class IList = BuildList<sizeof...(X)-1> >
    constexpr auto operator () ( F f, const std::tuple<X...>& ts  )
        -> decltype( doMap(std::move(f),ts,IList()) )
    {
        return doMap( std::move(f), ts, IList() );
    }
} map{};

constexpr struct Chainl {
    template< class Binary, class X >
    constexpr X operator () ( const Binary&, X&& x ) {
        return forward<X>(x);
    }

    template< class Binary, class X, class Y, class ...Z >
    constexpr auto operator () ( Binary&& b, X&& x, Y&& y, Z&& ...z)
        -> decltype(
            (*this)( b, b(declval<X>(),declval<Y>()), declval<Z>()... )
        )
    {
        return (*this)(
            b,
            b( forward<X>(x), forward<Y>(y) ),
            forward<Z>(z)...
        );
    }
} chainl{};

constexpr struct Chainr {
    template< class Binary, class X >
    constexpr X operator () ( const Binary&, X&& x ) {
        return forward<X>(x);
    }

    template< class Binary, class X, class Y, class ...Z >
    constexpr auto operator () ( const Binary& b, X&& x, Y&& y, Z&& ...z)
        -> decltype(
                b( (*this)(b,declval<Y>(),declval<Z>()...), declval<X>() )
        )
    {
        return b (
            (*this)( b, forward<Y>(y), forward<Z>(z)... ),
            forward<X>(x)
        );
    }
} chainr{};

constexpr struct Foldl {
    template< class Binary, class T, size_t ...I >
    constexpr auto doFold( Binary b, T&& ts, IndexList<I...> )
        -> decltype( chainl(b,std::get<I>(declval<T>())...) )
    {
        return chainl( b, std::get<I>(forward<T>(ts))... );
    }

    template< class Binary, class T, size_t N = std::tuple_size<T>::value,
              class IList = BuildList<N> >
    constexpr auto operator () ( Binary&& b, T&& ts  )
        -> decltype( doFold(declval<Binary>(),declval<T>(),IList()) )
    {
        return doFold( forward<Binary>(b), forward<T>(ts), IList() );
    }
} foldl{};

constexpr struct Foldr {
    template< class Binary, class T, size_t ...I >
    constexpr auto doFold( Binary b, T&& ts, IndexList<I...> )
        -> decltype( chainr(b,std::get<I>(declval<T>())...) )
    {
        return chainr( b, std::get<I>(forward<T>(ts))... );
    }

    template< class Binary, class T, size_t N = std::tuple_size<T>::value,
              class IList = BuildList<N> >
    constexpr auto operator () ( Binary&& b, T&& ts  )
        -> decltype( doFold(declval<Binary>(),declval<T>(),IList()) )
    {
        return doFold( forward<Binary>(b), forward<T>(ts), IList() );
    }
} foldr{};

} // namespace tpl

} // namespace pure
