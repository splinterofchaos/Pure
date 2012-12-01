
#include "Common.h"

#pragma once

namespace pure {

/* Forwarder<F>(x...) = f(x...) */
template< class F > struct Forwarder {
    using function = Decay<F>;
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

/* Make<T>(x...) = T(x...) */
template< class T > struct Make {
    template< class ...X >
    constexpr T operator () ( X&& ...x ) {
        return T( forward<X>(x)... );
    }
};

template< class T > struct Initialize {
    template< class ...X >
    constexpr T operator () ( X&& ...x ) {
        return T{ forward<X>(x)... };
    }
};

/* MakeT<T>(x) = T<X>(x) */
template< template<class...> class T > struct MakeT {
    template< class ...X, class R = T< Decay<X>... > >
    constexpr R operator () ( X&& ...x ) {
        return R( forward<X>(x)... );
    }
};

/* ForwardT<T>(X&& x) = T<X>(x) */
template< template<class...> class T > struct ForwardT {
    template< class ...X, class R = T< X... > >
    constexpr R operator () ( X&& ...x ) {
        return R( forward<X>(x)... );
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

constexpr auto flip = MakeT<Flip>();

/* 
 * Partial application.
 * g(y) = f( x, y )
 * h()  = f( x, y )
 * partial( f, x )    = g
 * partial( f, x, y ) = h
 */
template< class F, class X >
struct Part {
    F f = F();
    X x;

    template< class _F, class _X >
    constexpr Part( _F&& f, _X&& x )
        : f(forward<_F>(f)), x(forward<_X>(x))
    {
    }

    constexpr Part( X x )
        : x(move(x))
    {
    }

    /* 
     * The return type of F only gets deduced based on the number of arguments
     * supplied. Part otherwise has no idea whether f takes 1 or 10 xs.
     */
    template< class ... Xs >
    auto operator () ( Xs&& ...xs )
        -> decltype( f(x,declval<Xs>()...) )
    {
        return f( x, forward<Xs>(xs)... );
    }

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
    F f = F();
    X x;

    template< class _F, class _X >
    constexpr RPart( _F&& f, _X&& x ) 
        : f( forward<_F>(f) ), x( forward<_X>(x) ) { }

    constexpr RPart( X x )
        : x(move(x))
    {
    }

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

/* ConstructBinary<T>(x,y) = T<X,Y>(x,y) */
template< template<class...> class T >
struct MakeBinaryT : Binary<MakeBinaryT<T>> {
    using Binary<MakeBinaryT<T>>::operator();

    template< class X, class Y, class R = T< Decay<X>, Decay<Y> > >
    constexpr R operator () ( X&& x, Y&& y ) {
        return R( forward<X>(x), forward<Y>(y) );
    }
};

/* ForwardBinary<T>(x,y) = T<X&&,Y&&>(x,y) */
template< template<class...> class T >
struct ForwardBinaryT : Binary<ForwardBinaryT<T>> {
    using Binary<ForwardBinaryT<T>>::operator();

    template< class X, class Y, class R = T<X,Y> >
    constexpr R operator () ( X&& x, Y&& y ) {
        return R( forward<X>(x), forward<Y>(y) );
    }
};

/*
 * Left Associativity
 * Given a binary function, f(x,y):
 *      Let f(x,y,z,h) = f( f( f(x,y) ,z ), h ) -- Chaining
 */
template< class F > struct Chainable : Binary<F> {
    using Binary<F>::operator();

    template< class X, class Y >
    using R = typename std::result_of< F(X,Y) >::type;

    // Three arguments: unroll.
    template< class X, class Y, class Z >
    constexpr auto operator () ( X&& x, Y&& y, Z&& z )
        -> R< R<X,Y>, Z >
    {
        return F()(
            F()( std::forward<X>(x), std::forward<Y>(y) ),
            std::forward<Z>(z)
        );
    }

    template< class X, class Y, class ...Z >
    using Unroll = Result <
        Chainable<F>,  R<X,Y>, Z...
    >;

    // Any more? recurse.
    template< class X, class Y, class Z, class H, class ...J >
    constexpr auto operator () ( X&& x, Y&& y, Z&& z, H&& h, J&& ...j )
        -> Unroll<X,Y,Z,H,J...>
    {
        // Notice how (*this) always gets applied at LEAST three arguments.
        return (*this)(
            F()( std::forward<X>(x), std::forward<Y>(y) ),
            std::forward<Z>(z), std::forward<H>(h), std::forward<J>(j)...
        );
    }
};

template< class F > struct ReverseChainable : Binary<F> {
    using Self = ReverseChainable<F>;

    using Binary<F>::operator();

    template< class X, class Y >
    using R = typename std::result_of< F(X,Y) >::type;

    // Three arguments: unroll.
    template< class X, class Y, class Z >
    constexpr auto operator () ( X&& x, Y&& y, Z&& z )
        -> R< R<X,Z>, Y >
    {
        return F()(
            F()( std::forward<X>(x), std::forward<Z>(z) ),
            std::forward<Y>(y)
        );
    }

    // Any more? recurse.
    template< class X, class Y, class Z, class H, class ...J >
    constexpr auto operator () ( X&& x, Y&& y, Z&& z, H&& h, J&& ...j )
        -> decltype(
            F() (
                (*this)( forward<X>(x), forward<Z>(z),
                         forward<H>(h), forward<J>(j)... ),
                forward<Y>(y)
            )
        )
    {
        // Notice how (*this) always gets applied at LEAST three arguments.
        return F() (
            (*this)( forward<X>(x), forward<Z>(z),
                     forward<H>(h), forward<J>(j)... ),
            forward<Y>(y)
        );
    }
};

template< template<class...> class T >
struct MakeChainableT : Chainable<MakeT<T>> {
    using Chainable<MakeT<T>>::operator();

    template< class X, class Y, class R = T< Decay<X>, Decay<Y> > >
    constexpr R operator () ( X&& x, Y&& y ) {
        return R( forward<X>(x), forward<Y>(y) );
    }
};

template< template<class...> class T >
struct MakeReverseChainableT : ReverseChainable<MakeT<T>> {
    using ReverseChainable<MakeT<T>>::operator();

    template< class X, class Y, class R = T< Decay<X>, Decay<Y> > >
    constexpr R operator () ( X&& x, Y&& y ) {
        return R( forward<X>(x), forward<Y>(y) );
    }
};

template< template<class...> class T >
struct ForwardChainableT : Chainable<ForwardT<T>> {
    using Chainable<ForwardT<T>>::operator();

    template< class X, class Y, class R = T<X,Y> >
    constexpr R operator () ( X&& x, Y&& y ) {
        return R( forward<X>(x), forward<Y>(y) );
    }
};

template< template<class...> class T >
struct ForwardReverseChainableT : ReverseChainable<ForwardT<T>> {
    using ReverseChainable<ForwardT<T>>::operator();

    template< class X, class Y, class R = T<X,Y> >
    constexpr R operator () ( X&& x, Y&& y ) {
        return R( forward<X>(x), forward<Y>(y) );
    }
};

// Define And early as a helper for Transitive.
struct And : Chainable<And> {
    using Chainable<And>::operator();

    template< class X, class Y >
    constexpr auto operator () ( X&& x, Y&& y )
        -> decltype( declval<X>() && declval<Y>() )
    {
        return forward<X>(x) && forward<Y>(y);
    }
};

/*
 * Transitivity:
 * Given a transitive function, f(x,y,z), f(x,y) and f(y,z) implies f(x,z).
 * Let "and" be some function that folds the return of f.
 */
template< class F, class Fold=And > struct Transitive : Binary<F> {
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
 * PARTIAL APPLICATION
 * Some languages implement partial application through closures, which hold
 * references to the function's arguments. But they also often use reference
 * counting. We must consider the scope of the variables we want to apply. If
 * we apply references and then return the applied function, its references
 * will dangle.
 *
 * See:
 * upward funarg problem: http://en.wikipedia.org/wiki/Upward_funarg_problem
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

constexpr auto closure  = ForwardChainableT<Part>();
constexpr auto closet   = MakeChainableT<Part>();
constexpr auto rclosure = ForwardReverseChainableT<RPart>();
constexpr auto rcloset  = MakeReverseChainableT<RPart>();

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

constexpr auto enclosure = MakeT<Enclosure>();

/* 
 * Composition. 
 * Given f(x,y...) and g(z)
 * The composition of f and g, f . g, equals h(z,y...)
 *      h(z,y...) = f( g(z), y... )
 *
 */
template< class F, class G > struct Composition {
    F f = F();
    G g = G();

    constexpr Composition() { }
    constexpr Composition( F f, G g) 
        : f(move(f)), g(move(g)) { }

    template< class X, class ...Y >
    constexpr decltype( f(g(declval<X>()), declval<Y>()...) )
    operator () ( X&& x, Y&& ...y ) {
        return f( g( forward<X>(x) ), forward<Y>(y)... );
    }
};

constexpr auto compose = MakeChainableT<Composition>();

/* A const composition for when g is a constant function. */
template< class F, class G > struct CComposition {
    F f = F();
    G g = G();

    constexpr CComposition() { }

    template< class _F, class _G >
    constexpr CComposition( _F&& f, _G&& g ) 
        : f(forward<_F>(f)), g(forward<_G>(g)) { }

    template< class ...Y >
    constexpr decltype( f(g(), declval<Y>()...) )
    operator() ( Y&& ...y ) {
        return f( g(), forward<Y>(y)... );
    }
};

constexpr auto ccompose = MakeChainableT<CComposition>();

/* N-ary composition assumes a unary f and N-ary g. */
template< class F, class G > struct NComposition {
    F f = F();
    G g = G();

    constexpr NComposition() { }

    template< class _F, class _G >
    constexpr NComposition( _F&& f, _G&& g )
        : f(forward<_F>(f)), g(forward<_G>(g)) { }

    template< class ...X >
    constexpr decltype( f(g(declval<X>()...)) )
    operator() ( X&& ...x ) {
        return f( g( forward<X>(x)... ) );
    }
};

constexpr auto ncompose = MakeChainableT<NComposition>();

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
    F f = F();
    G g = G();
    H h = H();

    constexpr BComposition() {}

    template< class _F, class _G, class _H >
    constexpr BComposition( _F&& f, _G&& g, _H&& h ) 
        : f(forward<_F>(f)), g(forward<_G>(g)), h(forward<_H>(h)) { }

    template< class ...X >
    constexpr decltype( f(g(declval<X>()...), h(declval<X>()...)) )
    operator() ( X&& ...x ) {
        return f( g( forward<X>(x)... ), h( forward<X>(x)... ) );
    }
};

constexpr auto bcompose = MakeT<BComposition>();

constexpr auto returnPair = MakeChainableT<std::pair>();

template< size_t N, class P >
using Nth = decltype( std::get<N>(declval<P>()) );

/*
 * Function Pair.
 * pair_compose( f, g ) = \(x,y) -> (f x, g y) 
 */
template< class F, class G > struct PairComposition {
    F f = F();
    G g = G();

    constexpr PairComposition() {}

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

constexpr auto pairCompose = MakeChainableT<PairComposition>();

template< class F, class G > struct FanComposition {
    F f = F();
    G g = G();

    constexpr FanComposition() {}

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

constexpr auto fanCompose = MakeChainableT<FanComposition>();

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

constexpr struct AddEq : Chainable<AddEq> {
    using Chainable<AddEq>::operator();

    template< class X, class Y >
    constexpr auto operator() ( X&& x, Y&& y )
        -> decltype( declval<X>() += declval<Y>() )
    {
        return forward<X>(x) += forward<Y>(y);
    }
} addEq{};

constexpr struct Sub : Chainable<Sub> {
    using Chainable<Sub>::operator();

    template< class X, class Y >
    constexpr auto operator() ( X&& x, Y&& y ) 
        -> decltype( declval<X>() - declval<Y>() )
    {
        return forward<X>(x) - forward<Y>(y);
    }
} sub{};

constexpr struct SubEq : Chainable<SubEq> {
    using Chainable<SubEq>::operator();

    template< class X, class Y >
    constexpr auto operator() ( X&& x, Y&& y )
        -> decltype( declval<X>() -= declval<Y>() )
    {
        return forward<X>(x) -= forward<Y>(y);
    }
} subEq{};

constexpr struct Mult : Chainable<Mult> {
    using Chainable<Mult>::operator();

    template< class X, class Y >
    constexpr auto operator() ( X&& x, Y&& y ) 
        -> decltype( declval<X>() * declval<Y>() )
    {
        return forward<X>(x) * forward<Y>(y);
    }
} mult{};

constexpr struct MultEq : Chainable<MultEq> {
    using Chainable<MultEq>::operator();

    template< class X, class Y >
    constexpr auto operator() ( X&& x, Y&& y )
        -> decltype( declval<X>() *= declval<Y>() )
    {
        return forward<X>(x) *= forward<Y>(y);
    }
} multEq{};

constexpr struct Div : Chainable<Div> {
    using Chainable<Div>::operator();

    template< class X, class Y >
    constexpr auto operator() ( X&& x, Y&& y )
        -> decltype( declval<X>() / declval<Y>() )
    {
        return forward<X>(x) / forward<Y>(y);
    }
} div{};

constexpr struct DivEq : Chainable<DivEq> {
    using Chainable<DivEq>::operator();

    template< class X, class Y >
    constexpr auto operator() ( X&& x, Y&& y )
        -> decltype( declval<X>() /= declval<Y>() )
    {
        return forward<X>(x) /= forward<Y>(y);
    }
} divEq{};

constexpr struct Mod : Chainable<Mod> {
    using Chainable<Mod>::operator();

    template< class X, class Y >
    constexpr auto operator() ( X&& x, Y&& y )
        -> decltype( declval<X>() % declval<Y>() )
    {
        return forward<X>(x) % forward<Y>(y);
    }
} mod{};

constexpr struct ModEq : Chainable<ModEq> {
    using Chainable<ModEq>::operator();

    template< class X, class Y >
    constexpr auto operator() ( X&& x, Y&& y )
        -> decltype( declval<X>() %= declval<Y>() )
    {
        return forward<X>(x) %= forward<Y>(y);
    }
} modEq{};

constexpr struct LShift : Chainable<LShift> {
    using Chainable<LShift>::operator();

    template< class X, class Y >
    constexpr auto operator() ( X&& x, Y&& y )
        -> decltype( declval<X>() << declval<Y>() )
    {
        return forward<X>(x) << forward<Y>(y);
    }
} lshift{};

constexpr struct LShiftEq : Chainable<LShiftEq> {
    using Chainable<LShiftEq>::operator();

    template< class X, class Y >
    constexpr auto operator() ( X&& x, Y&& y )
        -> decltype( declval<X>() <<= declval<Y>() )
    {
        return forward<X>(x) <<= forward<Y>(y);
    }
} lshiftEq{};

constexpr struct RShift : Chainable<RShift> {
    using Chainable<RShift>::operator();

    template< class X, class Y >
    constexpr auto operator() ( X&& x, Y&& y )
        -> decltype( declval<X>() >> declval<Y>() )
    {
        return forward<X>(x) >> forward<Y>(y);
    }
} rshift{};

constexpr struct RShiftEq : Chainable<RShiftEq> {
    using Chainable<RShiftEq>::operator();

    template< class X, class Y >
    constexpr auto operator() ( X&& x, Y&& y )
        -> decltype( declval<X>() >>= declval<Y>() )
    {
        return forward<X>(x) >>= forward<Y>(y);
    }
} rshiftEq{};

// TODO: notEq(x,y,z) should be equivalent to x!=y and y!=z and x!=z.
constexpr struct NotEq : Binary<NotEq> {
    using Binary<NotEq>::operator();

    template< class X, class Y >
    constexpr bool operator() ( X&& x, Y&& y ) {
        return forward<X>(x) != forward<Y>(y);
    }
} notEq{};

constexpr struct Eq : Transitive<Eq> {
    using Transitive<Eq,And>::operator();

    template< class X, class Y >
    constexpr bool operator() ( X&& x, Y&& y ) {
        return forward<X>(x) == forward<Y>(y);
    }
} eq{};

struct Or : Chainable<Or> {
    using Chainable<Or>::operator();

    template< class X, class Y >
    constexpr auto operator () ( X&& x, Y&& y )
        -> decltype( declval<X>() || declval<Y>() )
    {
        return forward<X>(x) || forward<Y>(y);
    }
};

constexpr struct BitOr : Chainable<BitOr> {
    using Chainable<BitOr>::operator();

    template< class X, class Y >
    constexpr auto operator () ( X&& x, Y&& y )
        -> decltype( declval<X>() | declval<Y>() )
    {
        return forward<X>(x) | forward<Y>(y);
    }
} bitOr{};

constexpr struct BitOrEq : Chainable<BitOrEq> {
    using Chainable<BitOrEq>::operator();

    template< class X, class Y >
    constexpr auto operator () ( X&& x, Y&& y )
        -> decltype( declval<X>() |= declval<Y>() )
    {
        return forward<X>(x) |= forward<Y>(y);
    }
} bitOrEq{};

constexpr struct XOr : Chainable<XOr> {
    using Chainable<XOr>::operator();

    template< class X, class Y >
    constexpr auto operator () ( X&& x, Y&& y )
        -> decltype( declval<X>() ^ declval<Y>() )
    {
        return forward<X>(x) ^ forward<Y>(y);
    }
} xOr{};

constexpr struct XOrEq : Chainable<XOrEq> {
    using Chainable<XOrEq>::operator();

    template< class X, class Y >
    constexpr auto operator () ( X&& x, Y&& y )
        -> decltype( declval<X>() ^= declval<Y>() )
    {
        return forward<X>(x) ^= forward<Y>(y);
    }
} xOrEq{};

constexpr struct BitAnd : Chainable<BitAnd> {
    using Chainable<BitAnd>::operator();

    template< class X, class Y >
    constexpr auto operator () ( X&& x, Y&& y )
        -> decltype( declval<X>() & declval<Y>() )
    {
        return forward<X>(x) & forward<Y>(y);
    }
} bitAnd{};

constexpr struct BitAndEq : Chainable<BitAndEq> {
    using Chainable<BitAndEq>::operator();

    template< class X, class Y >
    constexpr auto operator () ( X&& x, Y&& y )
        -> decltype( declval<X>() &= declval<Y>() )
    {
        return forward<X>(x) &= forward<Y>(y);
    }
} bitAndEq{};

constexpr struct Less : Transitive<Less> {
    using Transitive<Less,And>::operator();

    template< class X, class Y >
    constexpr bool operator() ( X&& x, Y&& y ) {
        return forward<X>(x) < forward<Y>(y);
    }
} less{};

constexpr struct LessEq : Transitive<LessEq> {
    using Transitive<LessEq,And>::operator();

    template< class X, class Y >
    constexpr bool operator() ( X&& x, Y&& y ) {
        return forward<X>(x) <= forward<Y>(y);
    }
} lessEq{};

constexpr struct Greater : Transitive<Greater> {
    using Transitive<Greater,And>::operator();

    template< class X, class Y >
    constexpr bool operator() ( X&& x, Y&& y ) {
        return forward<X>(x) > forward<Y>(y);
    }
} greater{};

constexpr struct GreaterEq : Transitive<GreaterEq> {
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
