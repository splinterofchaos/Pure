
#include "Common.h"

#pragma once

namespace pure {

/* 
 * FUNCTION TRANSFORMERS
 * The fallowing are types that contain one or more functions and act like a
 * function. 
 */

template< class F > struct Forwarder {
    using function = F;
    function f = F();

    template< class ...G >
    constexpr Forwarder( G&& ...g ) : f( std::forward<G>(g)... ) { }

    template< class ...X >
    constexpr auto operator() ( X&& ...x )
        -> decltype( f(declval<X>()...) )
    {
        return f( std::forward<X>(x)... );
    }
};

template< class X > struct Construct {
    template< class ...Y >
    constexpr X operator() ( Y&& ...y ) {
        return X( forward<Y>(y)... );
    }
};

template< template<class...> class X > struct ConstructT {
    template< class ...Y, class R = X< Decay<Y>... > >
    constexpr R operator () ( Y&& ...y ) {
        return R( forward<Y>(y)... );
    }
};

template< template<class...> class X > struct ConstructF {
    template< class ...Y, class R = X< Y... > >
    constexpr R operator () ( Y&& ...y ) {
        return R( forward<Y>(y)... );
    }
};

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

constexpr auto flip = ConstructT<Flip>();

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
    constexpr auto operator () ( Xs&& ...xs )
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

/*
 * Given any binary function, f(x,y):
 *      Let f(x) represent a partial application.
 *      Left f.with(y) represent a right-oriented application.
 */
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

template< template<class...> class X >
struct ConstructBinary : Binary<ConstructBinary<X>> {
    using Binary<ConstructBinary<X>>::operator();

    template< class Y, class Z, class R = X< Decay<Y>, Decay<Z> > >
    constexpr R operator () ( Y&& y, Z&& z ) {
        return R( forward<Y>(y), forward<Z>(z) );
    }
};

template< template<class...> class X >
struct ForwardBinary : Binary<ForwardBinary<X>> {
    using Binary<ForwardBinary<X>>::operator();

    template< class Y, class Z, class R = X< Y, Z > >
    constexpr R operator () ( Y&& y, Z&& z ) {
        return R( forward<Y>(y), forward<Z>(z) );
    }
};

/*
 * Left Associativity
 * Given a binary function, f(x,y):
 *      Let f(x,y,z,h) = f( f( f(x,y) ,z ), h ) -- Chaining
 */
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

template< template<class...> class X >
struct ConstructChainable : Chainable<ConstructT<X>> {
    using Self = ConstructChainable<X>;
    using Chainable<ConstructT<X>>::operator();

    template< class Y, class Z, class R = X< Decay<Y>, Decay<Z> > >
    constexpr R operator () ( Y&& y, Z&& z ) {
        return R( forward<Y>(y), forward<Z>(z) );
    }
};

template< template<class...> class X >
struct ForwardChainable : Chainable<ConstructF<X>> {
    using Self = ForwardChainable<X>;
    using Chainable<ConstructF<X>>::operator();

    template< class Y, class Z, class R = X<Y,Z> >
    constexpr R operator () ( Y&& y, Z&& z ) {
        return R( forward<Y>(y), forward<Z>(z) );
    }
};


/*
 * Transitivity:
 * Given a transitive function, f(x,y,z), f(x,y) and f(y,z) implies f(x,z).
 * Let "and" be some function that folds the return of f.
 */
template< class F, class Fold > struct Transitive : Binary<F> {
    using Binary<F>::operator();

    template< class X, class Y, class Z >
    constexpr auto operator () ( X&& x, Y&& y, Z&& z )
        -> Result<F,X,Y>
    {
        return Fold() (
            F()( forward<X>(x), forward<Y>(y) ),
            F()( forward<Y>(y), forward<Z>(z) )
        );
    }

    template< class X, class Y, class Z, class A, class ...B >
    constexpr auto operator () ( X&& x, Y&& y, Z&& z, A&& a, B&& ...b )
        -> Result<F,X,Y>
    {
        return Fold() ( F()( forward<X>(x), forward<Y>(y) ),
                        F()( forward<Y>(y), forward<Z>(z),
                             forward<A>(a), forward<B>(b)... ) );
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
 *
 * Thinking as closures as open (having references to variables outside of
 * itself), let's refer to a closet as closed. It contains a function and its
 * arguments (or environment).
 */

constexpr auto closure  = ForwardChainable<Part>();
constexpr auto rclosure = ForwardChainable<RPart>();
constexpr auto closet   = ConstructChainable<Part>();
constexpr auto rcloset  = ConstructChainable<RPart>();

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

constexpr auto enclosure = ConstructT<Enclosure>();

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

constexpr auto compose = ConstructChainable<Composition>();

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

constexpr auto ccompose = ConstructChainable<CComposition>();

/* N-ary composition assumes a unary f and N-ary g. */
template< class F, class G > struct NComposition {
    F f; G g;

    template< class _F, class _G >
    constexpr NComposition( _F&& f, _G&& g )
        : f(forward<_F>(f)), g(forward<_G>(g)) { }

    template< class ...X >
    constexpr decltype( f(g(declval<X>()...)) )
    operator() ( X&& ...x ) {
        return f( g( forward<X>(x)... ) );
    }
};

constexpr auto ncompose = ConstructChainable<NComposition>();

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

constexpr auto bcompose = ConstructT<BComposition>();

constexpr auto returnPair = ConstructChainable<std::pair>();

template< size_t N, class P >
using Nth = decltype( std::get<N>(declval<P>()) );

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

// TODO: Chainable?
constexpr auto pairCompose = ConstructBinary<PairComposition>();

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

constexpr auto fanCompose = ConstructBinary<FanComposition>();

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

struct And : Chainable<And> {
    using Chainable<And>::operator();

    template< class X, class Y >
    constexpr auto operator () ( X&& x, Y&& y )
        -> decltype( declval<X>() && declval<Y>() )
    {
        return forward<X>(x) && forward<Y>(y);
    }
};

constexpr struct Eq : Transitive<Eq,And> {
    using Transitive<Eq,And>::operator();

    template< class X, class Y >
    constexpr bool operator() ( X&& x, Y&& y ) {
        return forward<X>(x) == forward<Y>(y);
    }
} equalTo{};

constexpr struct Less : Transitive<Less,And> {
    using Transitive<Less,And>::operator();

    template< class X, class Y >
    constexpr bool operator() ( X&& x, Y&& y ) {
        return forward<X>(x) < forward<Y>(y);
    }
} less{};

constexpr struct LessEq : Transitive<LessEq,And> {
    using Transitive<LessEq,And>::operator();

    template< class X, class Y >
    constexpr bool operator() ( X&& x, Y&& y ) {
        return forward<X>(x) <= forward<Y>(y);
    }
} lessEq{};

constexpr struct Greater : Transitive<Greater,And> {
    using Transitive<Greater,And>::operator();

    template< class X, class Y >
    constexpr bool operator() ( X&& x, Y&& y ) {
        return forward<X>(x) > forward<Y>(y);
    }
} greater{};

constexpr struct GreaterEq : Transitive<GreaterEq,And> {
    using Transitive<GreaterEq,And>::operator();

    template< class X, class Y >
    constexpr bool operator() ( X&& x, Y&& y ) {
        return forward<X>(x) >= forward<Y>(y);
    }
} greaterEq{};

constexpr struct BinaryNot {
    template< class B >
    constexpr bool operator() ( B&& b ) {
        return not (bool)forward<B>(b);
    }
} binaryNot{};

constexpr auto fnot = ncompose( binaryNot );

constexpr struct Mod : Chainable<Mod> {
    using Chainable<Mod>::operator();

    template< class X, class Y >
    constexpr CommonType<X,Y> operator() ( X&& x, Y&& y ) {
        return forward<X>(x) % forward<Y>(y);
    }
} mod{};

constexpr auto divisorOf = compose( fnot, mod );
constexpr auto divisibleBy = compose( fnot, rcloset(mod) );
constexpr auto multipleOf = divisibleBy;

constexpr struct Max : Chainable<Max> {
    using Chainable<Max>::operator();

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

constexpr struct Min : Chainable<Min> {
    using Chainable<Min>::operator();

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
