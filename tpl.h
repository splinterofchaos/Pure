
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

constexpr auto pair  = MakeBinaryT<std::pair>();
constexpr auto tuple = MakeT<std::tuple>();

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

template< size_t N, class ...P, class X, class ...XS >
auto _doSendWhen( X&& x, std::tuple<XS...>& dsts, const std::tuple<P...>& )
    -> typename std::enable_if<(N==sizeof...(P)), void >::type
{
    std::get<0>(dsts).emplace_back( forward<X>(x) );
}

template< size_t N, class ...P, class X, class ...XS >
auto _doSendWhen( X&& x, std::tuple<XS...>& dsts,
                  const std::tuple<P...>& preds )
    -> typename std::enable_if<(N<sizeof...(P)), void >::type
{
    if( std::get<N>(preds)(x) ) {
        std::get<N+1>(dsts).emplace_back( forward<X>(x) );
    } else {
        _doSendWhen<N+1>( forward<X>(x), dsts, preds );
    }
}

template< class R, class X >
R dontCare(X);

template< class XS, class ...P,
          class R = decltype (
              tuple_cat( tuple(declval<XS>()),
                         tuple(dontCare<XS>(declval<P>())...) )
          ) >
R _fork( const XS& xs, const std::tuple<P...>& ps ) {
    R r;
    for( const auto& x : xs )
        _doSendWhen<0>( std::move(x), r, ps );
    return r;
}

constexpr struct Fork : Binary<Fork> {
    using Binary<Fork>::operator();

    template< class XS, class ...P,
              class R = decltype (
                  tuple_cat( tuple(declval<XS>()),
                             tuple(dontCare<XS>(declval<P>())...) )
              ) >
    R operator () ( const XS& xs, P&& ...ps ) const {
        return _fork( xs, std::forward_as_tuple(ps...) );
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
              class IList = typename IListBuilder<sizeof...(X)-1>::type >
    constexpr auto operator () ( F f, const std::tuple<X...>& ts  )
        -> decltype( doMap(std::move(f),ts,IList()) )
    {
        return doMap( std::move(f), ts, IList() );
    }
} map{};

} // namespace tpl

} // namespace pure
