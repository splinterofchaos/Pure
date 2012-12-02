
#pragma once

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

} // namespace tpl

} // namespace pure
