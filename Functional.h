
#include "Common.h"

#pragma once

namespace pure {

/* 
 * FUNCTION TRANSFORMERS
 * The fallowing are types that contain one or more functions and act like a
 * function. 
 */

/* Flip f : g(x,y) = f(y,x) */
template< class F > struct Flip {
    F f;

    template< class _F >
    constexpr Flip( _F&& f ) : f(forward<_F>(f)) { }

    template< class X, class Y, class ...Z >
    constexpr decltype( f(declval<Y>(),declval<X>(),declval<Z>()...) )
    operator() ( X&& x, Y&& y, Z&& ...z ) {
        return f( forward<Y>(y), forward<X>(x), forward<Z>(z)... );
    }
};

template< class F >
constexpr Flip<F> flip( F&& f ) {
    return Flip<F>( forward<F>(f) );
}

/* 
 * Partial application.
 * g(y) = f( x, y )
 * h()  = f( x, y )
 * partial( f, x )    = g
 * partial( f, x, y ) = h
 */
template< class F, class X >
struct Part {
    F f;
    X x;

    template< class _F, class _X >
    constexpr Part( _F&& f, _X&& x )
        : f(forward<_F>(f)), x(forward<_X>(x))
    {
    }

    /* 
     * The return type of F only gets deduced based on the number of xuments
     * supplied. Part otherwise has no idea whether f takes 1 or 10 xs.
     */
    template< class ... Xs >
    constexpr auto operator() ( Xs&& ...xs )
        -> decltype( f(x,declval<Xs>()...) )
    {
        return f( x, forward<Xs>(xs)... );
    }
};

/*
 * Reverse partial application. 
 * g(z) = f( x, y, z )
 * rpartial( f, y, z ) -> g(x)
 */
template< class F, class X > struct RPart {
    F f;
    X x;

    template< class _F, class _X >
    constexpr RPart( _F&& f, _X&& x ) 
        : f( forward<_F>(f) ), x( forward<_X>(x) ) { }

    template< class ...Y >
    constexpr decltype( f(declval<Y>()..., x) )
    operator() ( Y&& ...y ) {
        return f( forward<Y>(y)..., x );
    }
};

template< class D > struct Binary {
    template< class X >
    constexpr Part<D,X> operator () ( X x ) {
        return Part<D,X>( D(), move(x) );
    }

    template< class X >
    constexpr RPart<D,X> with( X x ) {
        return RPart<D,X>( D(), move(x) );
    }
};

template< class D > struct Chainable : Binary<D> {
    using Binary<D>::operator();

    template< class X, class Y >
    using R = typename std::result_of< D(X,Y) >::type;

    // Three arguments: unroll.
    template< class X, class Y, class Z >
    constexpr auto operator () ( X&& x, Y&& y, Z&& z )
        -> R< R<X,Y>, Z >
    {
        return D()(
            D()( std::forward<X>(x), std::forward<Y>(y) ),
            std::forward<Z>(z)
        );
    }

    template< class X, class Y, class ...Z >
    using Unroll = typename std::result_of <
        Chainable<D>( Result<X,Y>, Z... )
    >::type;

    // Any more? recurse.
    template< class X, class Y, class Z, class H, class ...J >
    constexpr auto operator () ( X&& x, Y&& y, Z&& z, H&& h, J&& ...j )
        -> Unroll<X,Y,Z,H,J...>
    {
        // Notice how (*this) always gets applied at LEAST three arguments.
        return (*this)(
            D()( std::forward<X>(x), std::forward<Y>(y) ),
            std::forward<Z>(z), std::forward<H>(h), std::forward<J>(j)...
        );
    }
};

/*
 * Some languages implement partial application through closures, which hold
 * references to the function's arguments. But they also often use reference
 * counting. We must consider the scope of the variables we want to apply. If
 * we apply references and then return the applied function, its references
 * will dangle.
 *
 * See:
 * upward funarg problem http://en.wikipedia.org/wiki/Upward_funarg_problem
 */

/*
 * closure http://en.wikipedia.org/wiki/Closure_%28computer_science%29
 * Here, closure forwards the arguments, which may be references or rvalues--it
 * does not matter. A regular closure works for passing functions down.
 */
constexpr struct ReturnClosure : Chainable<ReturnClosure> {
    using Chainable<ReturnClosure>::operator();

    template< class F, class X >
    constexpr Part<F,X> operator () ( F&& f, X&& x ) {
        return Part<F,X>( forward<F>(f), forward<X>(x) );
    }
} closure{};

/*
 * Thinking as closures as open (having references to variables outside of
 * itself), let's refer to a closet as closed. It contains a function and its
 * arguments (or environment).
 */
constexpr struct ReturnCloset : Chainable<ReturnCloset> {
    using Chainable<ReturnCloset>::operator();

    template< class F, class X >
    constexpr Part<F,X> operator () ( F f, X x ) {
        return Part<F,X>( move(f), move(x) );
    }
} closet{};

constexpr struct ReturnRClosure : Chainable<ReturnRClosure> {
    using Chainable<ReturnRClosure>::operator();

    template< class F, class X, class P = RPart<F,X> >
    constexpr P operator () ( F&& f, X&& x ) {
        return P( forward<F>(f), forward<X>(x) );
    }
} rclosure{};

constexpr struct ReturnRCloset : Chainable<ReturnRCloset> {
    using Chainable<ReturnRCloset>::operator();

    template< class F, class X, class P = RPart<F,X> >
    constexpr P operator () ( F f, X x ) {
        return P( move(f), move(x) );
    }
} rcloset{};

template< class F, class ...X >
using Closure = decltype( closure(declval<F>(), declval<X>()...) );
template< class F, class ...X >
using RClosure = decltype( rclosure(declval<F>(), declval<X>()...) );
template< class F, class ...X >
using Closet = decltype( closet(declval<F>(), declval<X>()...) );
template< class F, class ...X >
using RCloset = decltype( rcloset(declval<F>(), declval<X>()...) );

/* 
 * Given f(x,y...) and fx(y...)
 *  enclose f = fx
 */
template< class F > struct Enclosure {
    F f;

    template< class _F >
    constexpr Enclosure( _F&& f ) : f(forward<_F>(f)) { }

    template< class ...X >
    using Result = decltype( closure(f,declval<X>()...) );

    template< class ...X > 
    constexpr Result<X...> operator() ( X&& ...x ) {
        return closure( f, forward<X>(x)... );
    }
};

template< class F, class E = Enclosure<F> >
constexpr E enclosure( F&& f ) {
    return E( forward<F>(f) );
}

/* 
 * Composition. 
 * Given f(x,y...) and g(z)
 * The composition of f and g, f . g, equals h(z,y...)
 *      h(z,y...) = f( g(z), y... )
 *
 */
template< class F, class G > struct Composition {
    F f; G g;

    constexpr Composition( F f, G g) 
        : f(move(f)), g(move(g)) { }

    template< class X, class ...Y >
    constexpr decltype( f(g(declval<X>()), declval<Y>()...) )
    operator () ( X&& x, Y&& ...y ) {
        return f( g( forward<X>(x) ), forward<Y>(y)... );
    }
};

constexpr struct Compose : Chainable<Compose> {
    using Chainable<Compose>::operator();

    template< class F, class G, class C = Composition<F,G> >
    constexpr C operator () ( F f, G g ) {
        return C( move(f), move(g) );
    }
} compose{};

/* A const composition for when g is a constant function. */
template< class F, class G > struct CComposition {
    F f; G g;

    template< class _F, class _G >
    constexpr CComposition( _F&& f, _G&& g ) 
        : f(forward<_F>(f)), g(forward<_G>(g)) { }

    template< class ...Y >
    constexpr decltype( f(g(), declval<Y>()...) )
    operator() ( Y&& ...y ) {
        return f( g(), forward<Y>(y)... );
    }
};

constexpr struct CCompose : Chainable<CCompose> {
    using Chainable<CCompose>::operator();

    template< class F, class G, class C = CComposition<F,G> >
    constexpr C operator () ( F f, G g ) {
        return C( move(f), move(g) );
    }
} ccompose{};

/* N-ary composition assumes a unary f and N-ary g. */
template< class F, class G > struct NCompoposition {
    F f; G g;

    template< class _F, class _G >
    constexpr NCompoposition( _F&& f, _G&& g ) 
        : f(forward<_F>(f)), g(forward<_G>(g)) { }

    template< class ...X >
    constexpr decltype( f(g(declval<X>()...)) )
    operator() ( X&& ...x ) {
        return f( g( forward<X>(x)... ) );
    }
};

constexpr struct NCompose : Chainable<NCompose> {
    using Chainable<NCompose>::operator();

    template< class F, class ...G, class C = NCompoposition<F,G...> >
    constexpr C operator ()( F f, G ...g ) {
        return C( move(f), move(g)... );
    }
} ncompose{};

/*
 * Binary composition 
 *      (compose2 http://www.sgi.com/tech/stl/binary_compose.html)
 * Given g(x)=y,  h(x)=z, and f(y,z), let bf(x) = f( g(x), h(x) )
 * This implementation diverges from compose2 in that it allows g and h to be
 * n-ary.
 */
template< class F, class G, class H >
struct BComposition
{
    F f; G g; H h;

    template< class _F, class _G, class _H >
    constexpr BComposition( _F&& f, _G&& g, _H&& h ) 
        : f(forward<_F>(f)), g(forward<_G>(g)), h(forward<_H>(h)) { }

    template< class ...X >
    constexpr decltype( f(g(declval<X>()...), h(declval<X>()...)) )
    operator() ( X&& ...x ) {
        return f( g( forward<X>(x)... ), h( forward<X>(x)... ) );
    }
};

template< class F, class G, class H, class C = BComposition<F,G,H> >
constexpr C bcompose( F f, G g, H h ) {
    return C( move(f), move(g), move(h) );
}

template< size_t N, class P >
using Nth = decltype( std::get<N>( declval<P>() ) );

/*
 * Function Pair.
 * pair_compose( f, g ) = \(x,y) -> (f x, g y) 
 */
template< class F, class G > struct PairComposition {
    F f; G g;

    template< class _F, class _G >
    constexpr PairComposition( _F&& f, _G&& g )
        : f(forward<_F>(f)), g(forward<_G>(g))
    {
    }

    template< class Fn, size_t N, class P >
    using Nth = decltype( declval<Fn>()( declval<Nth<N,P>>() ) );

    template< class P >
    using Result = decltype( std::make_pair(declval<Nth<F,0,P>>(),
                                            declval<Nth<G,1,P>>()) );

    template< class P/*air*/ >
    constexpr Result<P> operator() ( const P& p ) {
        return std::make_pair( f(std::get<0>(p)), g(std::get<1>(p)) );
    }
};

constexpr struct PairCompose : Binary<PairCompose> {
    using Binary<PairCompose>::operator();

    template< class F, class G >
    using result = PairComposition<F,G>;

    template< class F, class G >
    constexpr result<F,G> operator () ( F f, G g ) {
        return result<F,G>( move(f), move(g) );
    }
} pairCompose{};

template< class F, class G > struct FanComposition {
    F f = F();
    G g = G();

    constexpr FanComposition( F f, G g )
        : f( std::move(f) ), g( std::move(g) )
    {
    }

    template< class X >
    using resultF = Result<F,X>;

    template< class X >
    using resultG = Result<G,X>;

    template< class X >
    using result = std::pair< resultF<X>, resultG<X> >;

    template< class X, class R = result<X> >
    constexpr R operator () ( const X& x ) {
        return R{ f(x), g(x) };
    }
};

constexpr struct FanCompose : Binary<FanCompose> {
    using Binary<FanCompose>::operator();

    template< class F, class G >
    using Fn = FanComposition<F,G>;

    template< class F, class G >
    constexpr Fn<F,G> operator () ( F f, G g ) {
        return Fn<F,G>( std::move(f), std::move(g) );
    }
} fanCompose{};

template< class X > constexpr X inc( X x ) { return ++x; }
template< class X > constexpr X dec( X x ) { return --x; }

constexpr struct Add : Chainable<Add> {
    using Chainable<Add>::operator();

    template< class X, class Y >
    constexpr auto operator() ( X&& x, Y&& y ) 
        -> decltype( declval<X>() + declval<Y>() )
    {
        return forward<X>(x) + forward<Y>(y);
    }
} add{};

constexpr struct Sub : Chainable<Sub> {
    using Chainable<Sub>::operator();

    template< class X, class Y >
    constexpr auto operator() ( X&& x, Y&& y ) 
        -> decltype( declval<X>() - declval<Y>() )
    {
        return forward<X>(x) - forward<Y>(y);
    }
} sub{};

constexpr struct Mult : Chainable<Mult> {
    using Chainable<Mult>::operator();

    template< class X, class Y >
    constexpr auto operator() ( X&& x, Y&& y ) 
        -> decltype( declval<X>() * declval<Y>() )
    {
        return forward<X>(x) * forward<Y>(y);
    }
} mult{};

constexpr struct NotEq : Binary<NotEq> {
    using Binary<NotEq>::operator();

    template< class X, class Y >
    constexpr bool operator() ( X&& x, Y&& y ) {
        return forward<X>(x) != forward<Y>(y);
    }
} notExqualTo{};

constexpr struct Eq : Binary<Eq> {
    using Binary<Eq>::operator();

    template< class X, class Y >
    constexpr bool operator() ( X&& x, Y&& y ) {
        return forward<X>(x) == forward<Y>(y);
    }
} equalTo{};

constexpr struct Less : Binary<Less> {
    using Binary<Less>::operator();

    template< class X, class Y >
    constexpr bool operator() ( X&& x, Y&& y ) {
        return forward<X>(x) < forward<Y>(y);
    }
} less{};

constexpr struct LessEq : Binary<LessEq> {
    using Binary<LessEq>::operator();

    template< class X, class Y >
    constexpr bool operator() ( X&& x, Y&& y ) {
        return forward<X>(x) <= forward<Y>(y);
    }
} lessEq{};

constexpr struct Greater : Binary<Greater> {
    using Binary<Greater>::operator();

    template< class X, class Y >
    constexpr bool operator() ( X&& x, Y&& y ) {
        return forward<X>(x) > forward<Y>(y);
    }
} greater{};

constexpr struct GreaterEq : Binary<GreaterEq> {
    using Binary<GreaterEq>::operator();

    template< class X, class Y >
    constexpr bool operator() ( X&& x, Y&& y ) {
        return forward<X>(x) >= forward<Y>(y);
    }
} greaterEq{};

struct BinaryNot {
    template< class B >
    constexpr bool operator() ( B&& b ) {
        return not (bool)forward<B>(b);
    }
};

template< class F >
constexpr auto fnot( F f ) -> decltype( compose(BinaryNot(),declval<F>()) ) {
    return compose( BinaryNot(), move(f) );
}

constexpr struct Mod : Chainable<Mod> {
    using Chainable<Mod>::operator();

    template< class X, class Y >
    constexpr CommonType<X,Y> operator() ( X&& x, Y&& y ) {
        return forward<X>(x) % forward<Y>(y);
    }
} mod{};

template< class X >
constexpr auto divisibleBy( X x ) -> Composition<BinaryNot,RCloset<Mod,X>> {
    return fnot(rcloset( Mod(), move(x) ));
}

template< class X >
constexpr auto multipleOf( X x ) -> decltype( divisibleBy(move(x)) ) {
    return divisibleBy( move(x) );
}

template< class X >
constexpr auto divisorOf( X x ) -> Composition<BinaryNot,Closet<Mod,X>> {
    return fnot( closet(Mod(), move(x)) );
}

constexpr struct Max {
    template< class X, class Y >
    constexpr auto operator () ( X&& x, Y&& y ) 
        -> decltype( declval<X>() + declval<Y>() )
    {
        static_assert( std::is_integral<X>::value, "Non-integral x!" );
        static_assert( std::is_integral<Y>::value, "Non-integral y!" );
        return x > y ? x : y;
    }

    template< class X >
    constexpr const X& operator() ( const X& a, const X& b ) {
        return a > b ? a : b;
    }

    template< class X >
    X& operator() (  X& a,  X& b ) const {
        return a > b ? a : b;
    }
} max{};

constexpr struct Min {
    template< class X, class Y >
    constexpr auto operator () ( X&& x, Y&& y ) 
        -> decltype( declval<X>() + declval<Y>() )
    {
        static_assert( std::is_integral<X>::value, "Non-integral x!" );
        static_assert( std::is_integral<Y>::value, "Non-integral y!" );
        return x > y ? x : y;
    }

    template< class X >
    constexpr const X& operator() ( const X& a, const X& b ) {
        return a < b ? a : b;
    }

    template< class X >
    X& operator() (  X& a,  X& b ) const {
        return a < b ? a : b;
    }
} min{};

} // namespace pure.
