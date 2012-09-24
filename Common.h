
#include <utility>
#include <functional>

#pragma once

namespace pure {

using std::declval;
using std::forward;
using std::move;

template< class X > using Decay = typename std::decay<X>::type;

template< class ...X > 
using CommonType = Decay<typename std::common_type<X...>::type>;

template< class X, class R >
using ERVal = typename std::enable_if <
    std::is_rvalue_reference<X>::value, R
>::type;
template< class X, class R >
using XRVal = typename std::enable_if <
    not std::is_rvalue_reference< X >::value, R
>::type;

template< class A, class B, class R >
using ESame = typename std::enable_if< std::is_same<A,B>::value, R >::type;
template< class A, class B, class R >
using XSame = typename std::enable_if< !std::is_same<A,B>::value, R >::type;

/* The result of a function applied against X. */
template< class F, class ...X >
using Result = Decay<decltype( declval<F>()( declval<X>()... ) )>;

struct Id {
    template< class X >
    constexpr X operator() ( X&& x ) { return forward<X>(x); }
};

}
