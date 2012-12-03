
#pragma once

#include <utility>

#include "Functional.h"

namespace pure {

template< unsigned int N > struct Get {
    template< class X >
    constexpr auto operator () ( X&& x )
        -> decltype( std::get<N>(declval<X>()) )
    {
        return std::get<N>( std::forward<X>(x) );
    }
};

using std::get;

namespace tpl {

constexpr auto pair  = MakeBinaryT<std::pair>();
constexpr auto tuple = MakeT<std::tuple>();

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

template< size_t N >
using BuildList = typename IListBuilder<N-1>::type;

template< class T >
using TupleIndicies = BuildList< std::tuple_size<Decay<T>>::value >;

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

template< class T >
constexpr size_t size() {
    return std::tuple_size<Decay<T>>::value;
}

template< class T >
constexpr size_t size( const T& ) {
    return size<T>();
}

template< size_t i, class T >
using Elem = decltype( get<i>(declval<T>()) );

constexpr auto head = Get<0>();

constexpr struct last {
    template< class T, size_t S = size<T>() >
    constexpr Elem<S-1,T> operator () ( T&& t ) {
        return get<S-1>( forward<T>(t) );
    }
} last{};

template< size_t N, size_t ...I, class T >
constexpr auto takeFrom( IndexList<I...>, T&& t )
    -> decltype( tuple( get<I+N>(forward<T>(t))... ) )
{
    return tuple( get<I+N>(forward<T>(t))... );
}

template< size_t N, class T, class IS = BuildList<N> >
constexpr auto take( T&& t )
    -> decltype( takeFrom<0>( IS(), forward<T>(t) ) )
{
    return takeFrom<0>( IS(), forward<T>(t) );
}

template< size_t N, class T, class IS = BuildList<size<T>()-N> >
constexpr auto drop( T&& t )
    -> decltype( takeFrom<N>( IS(), forward<T>(t) ) )
{
    return takeFrom<N>( IS(), forward<T>(t) );
}

constexpr struct tail {
    template< class T >
    constexpr auto operator () ( T&& t )
        -> decltype( drop<1>( forward<T>(t) ) )
    {
        return drop<1>( forward<T>(t) );
    }
} tail{};

constexpr struct init {
    template< class T, size_t N = size<T>() - 1 >
    constexpr auto operator () ( T&& t )
        -> decltype( take<N>( forward<T>(t) ) )
    {
        return take<N>( forward<T>(t) );
    }
} init{};

constexpr struct append {
    template< class ...T >
    constexpr auto operator () ( T&& ...t )
        -> decltype( std::tuple_cat( forward<T>(t)... ) )
    {
        return std::tuple_cat( forward<T>(t)... );
    }
} append{};

constexpr struct call : Binary<call> {
    using Binary<call>::operator();

    template< size_t ...I, class F, class T >
    static constexpr auto impl( IndexList<I...>, const F& f, T&& t )
        -> decltype( f( get<I>(forward<T>(t))... ) )
    {
        return f( get<I>(forward<T>(t))... );
    }

    template< class F, class T, class IS = TupleIndicies<T> >
    constexpr auto operator () ( const F& f, T&& t )
        -> decltype( impl(IS(),f,forward<T>(t)) )
    {
        return impl( IS(), f, forward<T>(t) );
    }
} call{};


template< size_t N, class F, class T >
constexpr auto nth( const F& f, T&& t )
    -> decltype( std::tuple_cat (
        take<N>( forward<T>(t) ),
        tuple( f(get<N>(forward<T>(t))) ),
        drop<N+1>(forward<T>(t))
    ) )
{
    return std::tuple_cat (
        take<N>( forward<T>(t) ),
        tuple( f(get<N>(forward<T>(t))) ),
        drop<N+1>(forward<T>(t))
    );
}

template< size_t i, class TF, class ...TX >
constexpr auto apRow( const TF& tf, TX&& ...tx )
    -> decltype( std::get<i>(tf)( get<i>(forward<TX>(tx))... ) )
{
    return std::get<i>(tf)( get<i>(forward<TX>(tx))... );
}

constexpr struct ap {
    template< size_t ...I, class TF, class ...T >
    static constexpr auto impl( IndexList<I...>, const TF& tf, T&& ...t )
        -> decltype( tuple( apRow<I>(tf,forward<T>(t)...)... ) )
    {
        return tuple( apRow<I>(tf,forward<T>(t)...)... );
    }

    template< class TF, class ...T, class I = TupleIndicies<TF> >
    constexpr auto operator () ( const TF& tf, T&& ...t )
        -> decltype( impl(I(),tf,forward<T>(t)...) )
    {
        return impl( I(), tf, forward<T>(t)... );
    }
} ap{};

template< size_t i, class F, class ...T >
constexpr auto zipRowWith( const F& f, T&& ...t )
    -> decltype( f( get<i>(forward<T>(t))... ) )
{
    return f( get<i>(forward<T>(t))... );
}

constexpr struct zipWith {
    template< size_t ...I, class F, class ...T >
    static constexpr auto _zip (IndexList<I...>, const F& f, T&& ...t )
        -> decltype( tuple(zipRowWith<I>(f,declval<T>()...)...) )
    {
        return tuple( zipRowWith<I>( f, forward<T>(t)... )... );
    }

    template< class F, class T, class ...U,
              class IS = BuildList<std::tuple_size<T>::value> >
    constexpr auto operator () ( const F& f, T&& t, U&& ...u )
        -> decltype( _zip(IS(),f,declval<T>(),declval<U>()...) )
    {
        return _zip( IS(), f, forward<T>(t), forward<U>(u)... );
    }
} zipWith{};

template< size_t i, class ...T >
constexpr auto zipRow( T&& ...t )
    -> decltype( tuple( get<i>(forward<T>(t))... ) )
{
    return tuple( get<i>(forward<T>(t))... );
}

constexpr auto zip = closure( zipWith, tuple );

/*
 * fork(xs,pred0,pred1,...,predn)
 *
 * fork is similar to list::span, but flipped and variadic.
 *
 * Returns a tuple, {leftovers,xs0,xs1,...,xsn}, where xsN is every x from xs
 * such that predN(x) is true. leftovers contains every x for which no
 * predicate is true.
 */
constexpr struct Fork_ : Binary<Fork_> {
    using Binary<Fork_>::operator();

//    template< class XS, class ...P >
//    using Forked = decltype (
//        tuple_cat( tuple(declval<XS>()), // leftovers
//                   repeat<sizeof...(P)>(XS()) /* {xs0,xs1,...} */ )
//    );

    template< class XS, class ...P >
    using Forked = decltype( repeat<sizeof...(P)>(XS()) );

    // First, convert ps... to a tuple.
    template< class XS, class ...P >
    Forked<XS,P...> operator () ( XS& xs, P&& ...ps ) const {
        return doFork( xs, std::forward_as_tuple(ps...) );
    }

    // Build the results.
    template< class XS, class ...P >
    static Forked<XS,P...> doFork( XS& xs, const std::tuple<P...>& ps ) {
        Forked<XS,P...> r;
        auto it = std::begin(xs);
        for( ; it != std::end(xs); )
            if( doSendWhen<0>( *it, r, ps ) )
                it = xs.erase(it);
            else
                it++;
        return r;
    }

    template< bool when >
    using EnableWhen = typename std::enable_if< when, bool >::type;

    // Check each predicate and send it to the correct resultant.
    // If nothing matched, send x to the leftovers.
    template< size_t N, class X, class Dsts, class Preds >
    static auto doSendWhen( const X&, Dsts&, const Preds& )
        -> EnableWhen< N >= std::tuple_size<Dsts>::value >
    {
        return false; // No match.
    }

    template< size_t N, class X, class Dsts, class Preds >
    static auto doSendWhen( X& x, Dsts& dsts, const Preds& preds )
        -> EnableWhen< (N < std::tuple_size<Dsts>::value) >
    {
        if( std::get<N>(preds)(x) ) {
            std::get<N>(dsts).push_back( std::move(x) );
            return true;
        } else {
            doSendWhen<N+1>( x, dsts, preds );
        }
    }
} fork_{};

constexpr struct fork {
    template< class XS, class ...P >
    using Forked = decltype( repeat<sizeof...(P) + 1>(XS()) );

    // First, convert ps... to a tuple.
    template< class XS, class ...P >
    Forked<XS,P...> operator () ( XS xs, P&& ...ps ) const {
        auto forks = fork_( xs, forward<P>(ps)... );
        return std::tuple_cat( tuple(move(xs)), move(forks) );
    }
} fork{};

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
    template< class Binary, class T >
    constexpr auto operator () ( Binary&& b, T&& ts  )
        -> decltype( call( closure(chainl,forward<Binary>(b)), forward<T>(ts) ) )
    {
        return call( closure(chainl,forward<Binary>(b)), forward<T>(ts) );
    }
} foldl{};

constexpr struct Foldr {
    template< class Binary, class T >
    constexpr auto operator () ( Binary&& b, T&& ts  )
        -> decltype( call( closure(chainr,forward<Binary>(b)), forward<T>(ts) ) )
    {
        return call( closure(chainr,forward<Binary>(b)), forward<T>(ts) );
    }
} foldr{};

constexpr auto concat = closure( foldl, append );

constexpr struct fan {
    template< class X, class ...F >
    constexpr auto operator () ( const X& x, const F& ...f )
        -> decltype( tuple( f(x)... ) )
    {
        return tuple( f(x)... );
    }
} fan{};

constexpr struct split {
    template< class T, class ...F >
    constexpr auto operator () ( T&& t, F&& ...f )
        -> decltype( ap( std::forward_as_tuple(forward<F>(f)...), forward<T>(t) ) )
    {
        return ap( std::forward_as_tuple(forward<F>(f)...), forward<T>(t) );
    }
} split{};

constexpr struct toArray {
    template< class R, size_t ...I, class T >
    static constexpr R impl( IndexList<I...>, T&& t )
    {
        return {{ get<I>(forward<T>(t))... }};
    }

    template< class T, class X = Decay<Elem<0,T>>, size_t S = size<T>(),
              class IS = TupleIndicies<T>,
              class A = std::array<X,S> >
    constexpr A operator () ( T&& t ) {
        return impl<A>( IS(), forward<T>(t) );
    }
} toArray{};

constexpr auto toTuple = closure( call, tuple );

} // namespace tpl

} // namespace pure
