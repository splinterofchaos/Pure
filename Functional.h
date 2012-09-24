
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
 * partial( f, x ) -> g(y)
 * partial( f, x, y ) -> g()
 */
template< class F, class ...X >
struct Part;

template< class F, class X >
struct Part< F, X >
{
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

/* Recursive, variadic version. */
template< class F, class X1, class ...Xs >
struct Part< F, X1, Xs... > 
    : public Part< Part<F,X1>, Xs... >
{
    template< class _F, class _X1, class ..._Xs >
    constexpr Part( _F&& f, _X1&& x1, _Xs&& ...xs )
        : Part< Part<F,X1>, Xs... > (
            Part<F,X1>( forward<_F>(f), forward<_X1>(x1) ),
            forward<_Xs>(xs)...
        )
    {
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
template< class F, class ...X >
constexpr Part<F,X...> closure( F&& f, X&& ...x ) {
    return Part<F,X...>( forward<F>(f), forward<X>(x)... );
}

/*
 * Thinking as closures as open (having references to variables outside of
 * itself), let's refer to a closet as closed. It contains a function and its
 * arguments (or environment).
 */
template< class F, class ...X >
constexpr Part<F,X...> closet( F f, X ...x ) {
    return Part<F,X...>( move(f), move(x)... );
}

/*
 * Reverse partial application. 
 * g(z) = f( x, y, z )
 * rpartial( f, y, z ) -> g(x)
 */
template< class F, class ...X >
struct RPart;

template< class F, class X >
struct RPart< F, X > {
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

template< class F, class X1, class ...Xs >
struct RPart< F, X1, Xs... > 
    : public RPart< RPart<F,Xs...>, X1 >
{
    template< class _F, class _X1, class ..._Xs >
    constexpr RPart( _F&& f, _X1&& x1, _Xs&& ...xs )
        : RPart< RPart<F,Xs...>, X1 > (
            RPart<F,Xs...>( forward<_F>(f), forward<_Xs>(xs)... ),
            forward<_X1>(x1)
        )
    {
    }
};

template< class F, class ...X, class P = RPart<F,X...> >
constexpr P rclosure( F&& f, X&& ...x ) {
    return P( forward<F>(f), forward<X>(x)... );
}

template< class F, class ...X, class P = RPart<F,X...> >
constexpr P rcloset( F f, X ...x ) {
    return P( move(f), move(x)... );
}

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

template< class F, class ...G >
struct Composition;

template< class F, class G >
struct Composition<F,G>
{
    F f; G g;

    constexpr Composition( F f, G g) 
        : f(move(f)), g(move(g)) { }

    template< class X, class ...Y >
    constexpr decltype( f(g(declval<X>()), declval<Y>()...) )
    operator() ( X&& x, Y&& ...y ) {
        return f( g( forward<X>(x) ), forward<Y>(y)... );
    }
};

template< class F, class G, class ...H >
struct Composition<F,G,H...> : Composition<F,Composition<G,H...>>
{
    typedef Composition<G,H...> Comp;

    template< class _F, class _G, class ..._H >
    constexpr Composition( _F&& f, _G&& g, _H&& ...h )
        : Composition<_F,Composition<_G,_H...>> ( 
            forward<_F>(f), 
            Comp( forward<_G>(g), forward<_H>(h)... )
        )
    {
    }
};

template< class F, class ...G, class C = Composition<F,G...> >
constexpr C compose( F f, G ...g ) {
    return C( move(f), move(g)... );
}

/* A const composition for when g is a constant function. */
template< class F, class ...G >
struct CComposition;

template< class F, class G >
struct CComposition<F,G>
{
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

template< class F, class G, class ...H >
struct CComposition<F,G,H...> : CComposition<Composition<F,G>,H...>
{
    typedef Composition<F,G> Then;
    typedef CComposition<Then,H...> CComp;

    template< class _F, class _G, class ..._H >
    constexpr CComposition( _F&& f, _G&& g, _H&& ...h )
        : CComp ( 
            Then( forward<_F>(f), forward<_G>(g) ),
            forward<_H>(h)...
        )
    {
    }
};

template< class F, class ...G, class C = CComposition<F,G...> >
constexpr C ccompose( F f, G ...g ) {
    return C( move(f), move(g)... );
}

/* N-ary composition assumes a unary f and N-ary g. */
template< class F, class ...G >
struct NCompoposition;

template< class F, class G >
struct NCompoposition<F,G>
{
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

template< class F, class G, class ...H >
struct NCompoposition<F,G,H...> : NCompoposition<Composition<F,G>,H...>
{
    typedef Composition<F,G> Then;
    typedef NCompoposition<Then,H...> NComp;

    template< class _F, class _G, class ..._H >
    constexpr NCompoposition( _F&& f, _G&& g, _H&& ...h )
        : NComp ( 
            Then( forward<_F>(f), forward<_G>(g) ),
            forward<_H>(h)...
        )
    {
    }
};

template< class F, class ...G, class C = NCompoposition<F,G...> >
constexpr C ncompose( F f, G ...g ) {
    return C( move(f), move(g)... );
}

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
template< class F, class G > struct PairCompose {
    F f; G g;

    template< class _F, class _G >
    constexpr PairCompose( _F&& f, _G&& g ) 
        : f(forward<_F>(f)), g(forward<_G>(g)) { }

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

template< class F, class G, class P = PairCompose<F,G> > 
constexpr P pair_compose( F f, G g ) {
    return P( move(f), move(g) );
}

template< class X > constexpr X inc( X x ) { return ++x; }
template< class X > constexpr X dec( X x ) { return --x; }

struct Add {
    template< class X, class Y >
    constexpr CommonType<X,Y> operator() ( X&& x, Y&& y ) {
        return forward<X>(x) + forward<Y>(y);
    }
};

struct Subtract {
    template< class X, class Y >
    constexpr CommonType<X,Y> operator() ( X&& x, Y&& y ) {
        return forward<X>(x) - forward<Y>(y);
    }
};

struct Mult {
    template< class X, class Y >
    constexpr CommonType<X,Y> operator() ( X&& x, Y&& y ) {
        return forward<X>(x) * forward<Y>(y);
    }
};

template< class X >
constexpr Closet<Mult,X> times( X x ) {
    return closet( Mult(), move(x) );
}

template< class X > 
constexpr auto plus( X x ) -> Closet<Add,X> {
    return closet( Add(), move(x) );
}

struct Less {
    template< class X, class Y >
    constexpr bool operator() ( X&& x, Y&& y ) {
        return forward<X>(x) < forward<Y>(y);
    }
};

struct LessEq {
    template< class X, class Y >
    constexpr bool operator() ( X&& x, Y&& y ) {
        return forward<X>(x) <= forward<Y>(y);
    }
};

template< class X >
constexpr RCloset<Less,X> less_than( X x ) {
    return rcloset(Less(), move(x));
}

template< class X >
constexpr Closet<LessEq,X> less_equal_to( X x ) {
    return closet(LessEq(), move(x));
}

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

struct Mod {
    template< class X, class Y >
    constexpr CommonType<X,Y> operator() ( X&& x, Y&& y ) {
        return forward<X>(x) % forward<Y>(y);
    }
};

template< class X >
constexpr auto divisible_by( X x ) -> Composition<BinaryNot,RCloset<Mod,X>> {
    return fnot(rcloset( Mod(), move(x) ));
}

template< class X >
constexpr auto multiple_of( X x ) -> decltype( divisible_by(move(x)) ) {
    return divisible_by( move(x) );
}

template< class X >
constexpr auto divisor_of( X x ) -> Composition<BinaryNot,Closet<Mod,X>> {
    return fnot( closet(Mod(), move(x)) );
}

} // namespace pure.
