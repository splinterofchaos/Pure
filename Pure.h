
#pragma once

#include <algorithm>
#include <numeric>
#include <functional>
#include <iterator>
#include <array>
#include <vector>
#include <utility>
#include <tuple>
#include <memory>
#include <limits>

#include <iostream>

#include "Common.h"
#include "List.h"
#include "Functional.h"
#include "IO.h"
#include "Monad.h"

namespace pure {

using category::Cat;

using data::Just;
using data::Nothing;
using data::maybe;

using data::Either;
using data::Left;
using data::Right;
using data::either;

using monad::Functor;
using monad::fmap;
using monad::FMap;
using monad::Monad;
using monad::mdo;
using monad::mbind;
using monad::mreturn;
using monad::Return;
using monad::operator >>=;

template< typename Container, typename F >
void for_each( F&& f, const Container& cont ) {
    for_each( begin(cont), end(cont), forward<F>(f) );
}

template< class F, class I, class J >
void for_ij( const F& f, I i, const I& imax, J j, const J& jmax ) {
    for( ; j != jmax; j++ )
        for( ; i != imax; i++ )
            f( i, j );
}

template< class F, class I, class J >
void for_ij( const F& f, const I& imax, const J& jmax ) {
    for( J j = J(); j != jmax; j++ )
        for( I i = I(); i != imax; i++ )
            f( i, j );
}



/*
 * In category theory, a functor is a mapping between categories.
 * Pure provides the minimal wrappings to lift X to a higher category.
 *
 * pure(x) -> F(x) where F(_) = x
 */
template< class X >
struct Pure
{
    X x;

    explicit constexpr Pure( X x ) : x( x ) { }
    constexpr Pure( Pure&& ) = default;

    template< class ...Args >
    constexpr X operator() ( Args ... ) { return x; }
};

template< class X >
constexpr Pure<X> pure( X&& x )
{
    return Pure<X>( forward<X>(x) );
}

template< class F > struct PureFunction {
    F f;

    template< class ...X >
    constexpr auto operator () ( X&& ... ) -> decltype( f() ) {
        return f();
    }
};

template< class F > 
constexpr PureFunction<F> pureFunction( F f ) {
    return { move(f) };
}

template< class C > struct IsSeqImpl {
    // Can only be supported on STL-like sequence types, not pointers.
    template< class _C > static std::true_type f(typename _C::iterator*);
    template< class _C > static std::false_type f(...);
    typedef decltype( f<C>(0) ) type;
};

template< class C > struct IsSeq : public IsSeqImpl<C>::type { };

/* Enable if is an STL-like sequence. */
template< class C, class R > struct ESeq : std::enable_if<IsSeq<C>::value,R> { };

/* Disable if is sequence. */
template< class C, class R > struct XSeq : std::enable_if<not IsSeq<C>::value,R> { };

/* f <$> m */
template< class F, class M >
constexpr auto operator^ ( F&& f, M&& m ) 
    -> decltype( fmap(declval<F>(), declval<M>()) )
{
    return fmap( forward<F>(f), forward<M>(m) );
}

/*
 * Monoid M :
 *      mempty -> M
 *      mappend M M -> M
 *      mconcat [M] -> M
 *
 *      mconcat = foldr mappend mempty
 */
template< class > struct Monoid;

template< class M, class Mo = Monoid<Cat<M>> >
auto mempty() -> decltype( Mo::template mempty<M>() ) {
    return Mo::template mempty<M>(); 
}

constexpr struct MAppend : Chainable<MAppend> {
    using Chainable<MAppend>::operator();

    template< class M1, class M2, class Mo = Monoid<Cat<M1>> >
    constexpr auto operator() ( M1&& a, M2&& b ) 
        -> decltype( Mo::mappend(declval<M1>(),declval<M2>()) )
    {
        return Mo::mappend( forward<M1>(a), forward<M2>(b) );
    }
} mappend{};

constexpr struct MConcat {
    template< class S, class V = list::SeqVal<S>, class M = Monoid<Cat<V>> >
    constexpr auto operator () ( S&& s )
        -> decltype( M::mconcat(declval<S>()) )
    {
        return M::mconcat( forward<S>(s) );
    }
} mconcat{};

template<> struct Monoid< category::sequence_type > {
    template< class S >
    static S mempty() { return S{}; }

    static constexpr auto mappend = list::append;
    static constexpr auto mconcat = list::concat;
};

/* Monoid (Maybe X) -- where X is a monoid. */
template<> struct Monoid< category::maybe_type > {
    template< class M >
    static constexpr M mempty() { return nullptr; }

    template< class M >
    static M dup( const M& m ) {
        return m ? Just(*m) : nullptr;
    }

    template< class X >
    using ERVal = ERVal<X,Decay<X>>;

    template< class M >
    constexpr static ERVal<M> dup( M&& m ) {
        return m;
    }

    /*
     * Just x <> Just y = Just (x <> y)
     *      a <> b      = a | b
     */
    template< class M >
    static constexpr Decay<M> mappend( M&& x, M&& y ) {
        return x and y ? Just( pure::mappend(*forward<M>(x), *forward<M>(y)) )
            : x ? dup(forward<M>(x)) : dup(forward<M>(y));
    }

    /* mconcat [Just x, Just y, Nothing] = Just (x <> y <> mempty)*/
    template< class S, class M = list::SeqRef<S>, class R = Decay<M> > 
    static R mconcat( S&& s ) {
        return list::foldl( MAppend(), mempty<R>(), forward<S>(s) );
    }
};

/* Monoid (Pair X X) */
template< class X, class Y > struct Monoid< std::pair<X,Y> > {
    typedef std::pair<X,Y> P;

    static P mempty() { return P( pure::mempty<X>(), pure::mempty<Y>() ); }

    static P mappend( const P& a, const P& b ) {
        return P( pure::mappend(a.first,b.first),
                  pure::mappend(a.second,b.second) );
    }

    template< class S > static P mconcat( const S& s ) {
        return list::foldr( mappend, mempty(), s );
    }
};

/*
 * MonadPlus M :
 *      mzero -> M
 *      mplus M M -> M
 *
 *      mzero >>= f = mzero
 */
template< class ...F > struct MonadPlus;

template< class M, class Mo = MonadPlus<Cat<M>> >
decltype( Mo::mzero() ) mzero() { return Mo::mzero(); }

constexpr struct MPlus : Chainable<MPlus> {
    using Chainable<MPlus>::operator();

    template< class M1, class M2,
              class Mo = MonadPlus<Cat<M1>> >
    constexpr auto operator () ( M1&& a, M2&& b )
        -> decltype( Mo::mplus(declval<M1>(),declval<M2>()) )
    {
        return Mo::mplus( forward<M1>(a), forward<M2>(b) );
    }
} mplus{};

template<> struct MonadPlus< category::sequence_type > {
    template< class S >
    static S mzero() { return S(); }

    static constexpr auto mplus = list::append;
};

template<> struct MonadPlus< category::maybe_type > {
    template< class M >
    static M mzero() { return nullptr; }

    template< class A, class B >
    using Result = decltype( declval<A>() || declval<B>() );

    template< class A, class B >
    static constexpr Result<A,B> mplus( A&& a, B&& b ) {
        return forward<A>(a) || forward<B>(b);
    }
};

/*
* Rotation.
* f(x...,y) = g(y,x...)
* rot f = g
* rrot g = f
* rrot( rot f ) = f
*
* In stack-based (or concatenative languages) rotation applies to the top
* three elements on the stack. Here, the function's arguments are treated as a
* stack and rotated. The entire stack gets rotated, rather than the top three
* elements.
*/
template< class F, class ...Args >
struct PartLast; // Apply the last argument.

template< class F, class Last >
struct PartLast< F, Last > : public Part< F, Last > 
{ 
    template< class _F, class _Last >
    constexpr PartLast( _F&& f, _Last&& last )
        : Part<F,Last>( forward<_F>(f), forward<_Last>(last) )
    {
    }
};

// Remove one argument each recursion.
template< class F, class Arg1, class ...Args >
struct PartLast< F, Arg1, Args... > : public PartLast< F, Args... > 
{
    template< class _F, class _Arg1, class ..._Args >
    constexpr PartLast( _F&& f, _Arg1&&, _Args&& ...args )
        : PartLast<F,Args...>( forward<_F>(f), forward<_Args>(args)... )
    {
    }
};

template< class F, class ...Args >
struct PartInit; // Apply all but the last argument.

template< class F, class Arg1, class Arg2 >
struct PartInit< F, Arg1, Arg2 > : public Part< F, Arg1 >
{
    template< class _F, class _Arg1, class _Arg2 >
    constexpr PartInit( _F&& f, _Arg1&& arg1, _Arg2&& )
        : Part<F,Arg1>( forward<_F>(f), forward<_Arg1>(arg1) )
    {
    }
};

template< class F, class Arg1, class ...Args >
struct PartInit< F, Arg1, Args... > 
: public PartInit< Part<F,Arg1>, Args... >
{
    template< class _F, class _Arg1, class ..._Args >
    PartInit( _F&& f, _Arg1&& arg1, _Args&& ...args )
        : PartInit<Part<F,Arg1>,Args...> (
            partial( forward<_F>(f), forward<_Arg1>(arg1) ),
            forward<_Args>(args)...
        )
    {
    }
};

template< class F >
struct Rot
{
    F f;

    template< class _F >
    constexpr Rot( _F&& f ) : f( forward<_F>(f) ) { }

    template< class ...Args >
    constexpr auto operator() ( Args&& ...args )
        -> decltype ( 
            declval< PartInit<PartLast<F,Args...>,Args...> >()() 
        )
    {
        /* We can't just (to my knowledge) pull the initial and final args
         * apart, so first reverse-apply the last argument, then apply each
         * argument forward until the last. The result is a zero-arity function
         * to invoke.
         */
        return PartInit< PartLast<F,Args...>, Args... > (
            PartLast< F, Args... >( f, forward<Args>(args)... ),
            forward<Args>( args )...
        )();
    }
};

template< class F >
struct RRot // Reverse Rotate
{
    F f;

    template< class _F >
    constexpr RRot( _F&& f ) : f( forward<_F>(f) ) { }

    template< class Arg1, class ...Args >
    constexpr auto operator() ( Arg1&& arg1, Args&& ...args )
        -> decltype( f(declval<Args>()..., declval<Arg1>()) )
    {
        return f( forward<Args>(args)..., forward<Arg1>(arg1) );
    }
};

    template< class F >
constexpr Rot<F> rot( F&& f )
{
    return Rot<F>( forward<F>(f) );
}

    template< class F >
constexpr RRot<F> rrot( F&& f )
{
    return RRot<F>( forward<F>(f) );
}

/* Rotate F by N times. */
template< unsigned int N, class F >
struct NRot;

template< class F >
struct NRot< 1, F > : public Rot<F>
{
    template< class _F >
    constexpr NRot( _F&& f ) : Rot<F>( forward<_F>(f) ) { }
};

template< unsigned int N, class F >
struct NRot : public NRot< N-1, Rot<F> >
{
    template< class _F >
    constexpr NRot( _F&& f ) : NRot< N-1, Rot<F> >( rot(forward<_F>(f)) ) { }
};

template< unsigned int N, class F >
struct RNRot; // Reverse nrot.

template< class F >
struct RNRot< 1, F > : public RRot<F>
{
    template< class _F >
    RNRot( _F&& f ) : RRot<F>( forward<_F>(f) ) { }
};

template< unsigned int N, class F >
struct RNRot : public RNRot< N-1, RRot<F> >
{
    template< class _F >
        RNRot( _F&& f ) : RNRot<N-1,RRot<F>>( rrot(forward<_F>(f)) ) { }
};

template< unsigned int N, class F >
constexpr NRot<N,F> nrot( F&& f ) {
    return NRot<N,F>( forward<F>(f) );
}

template< unsigned int N, class F >
constexpr RNRot<N,F> rnrot( F&& f ) {
    return RNRot<N,F>( forward<F>(f) );
}

/* squash[ f(x,x) ] = g(x) */
template< class F >
struct Squash
{
    F f;

    template< class _F >
    constexpr Squash( _F&& f ) : f( forward<_F>(f) ) { }

    template< class X, class ...Y >
    constexpr auto operator() ( X&& x, Y&& ...y )
        -> decltype( f(declval<X>(),declval<X>(),declval<Y>()...) )
    {
        return f( forward<X>(x), forward<X>(x), 
                  forward<Y>(y)... );
    }
};

template< class F >
constexpr Squash<F> squash( F f ) {
    return Squash<F>( move(f) );
}

/* 
 * f( x, y, a... ) = f( l(z), r(z), a... ) = g(z,a...)
 *  where x = l(z) and y = r(z)
 * join( f, l, r ) = g
 *
* Similar to bcompose, but expects a unary l and r and an nary f.
*/
template< class F, class Left, class Right >
struct Join
{
    F f;
    Left l;
    Right r;

    template< class _F, class _L, class _R >
    constexpr Join( _F&& f, _L&& l, _R&& r )
        : f(forward<_F>(f)), l(forward<_L>(l)), r(forward<_R>(r))
    {
    }

    template< class X >
    using L = decltype( l(declval<X>()) );
    template< class X >
    using R = decltype( r(declval<X>()) );

    template< class A, class ...AS >
    constexpr Result< F, L<A>, R<A>, AS... > 
    operator() ( A&& a, AS&& ...as ) {
        return f( l( forward<A>(a) ), 
                  r( forward<A>(a) ), 
                  forward<AS>(as)... );
    }
};

template< class F, class L, class R >
constexpr Join<F,L,R> join( F&& f, L&& l, R&& r ) {
    return Join<F,L,R>( forward<F>(f), forward<L>(l), forward<R>(r) );
}


/*
 * cleave x f g h -> { f(x), g(x), h(x) }
 * Inspired by Factor, the stack-based language.
 */
template< typename X, typename F, typename ... Fs,
          typename R = decltype( declval<F>()(declval<X>()) ) >
constexpr std::array<R,sizeof...(Fs)+1> cleave( X&& x, F&& f, Fs&& ... fs ) 
{
    return {{ forward<F> (f )( forward<X>(x) ), 
              forward<Fs>(fs)( forward<X>(x) )... }};
}

/* cleave_with f x y z =  { f(x), f(y), f(z) } */
template< class F, class A, class ...B >
constexpr auto cleave_with( F&& f, A&& a, B&& ...b )
    -> std::array< decltype( declval<F>()(declval<A>()) ), sizeof...(B)+1 > 
{
    return {{ f(forward<A>(a)), f(forward<B>(b))... }};
}

template< class T, unsigned int N, class F >
std::array<T,N> generate( F&& f ) {
    std::array<T,N> cont;
    generate( begin(cont), end(cont), forward<F>(f) );
    return cont;
}

template< class T, class F >
std::vector<T> generate( F&& f, unsigned int n ) {
    std::vector<T> c; 
    c.reserve(n);
    while( n-- )
        c.push_back( forward<F>(f)() );
    return c;
}

template< class X > struct Identity {
    using value_type = Decay<X>;
    using reference  = value_type&;
    using const_reference = const value_type&;

    value_type x;

    const_reference get() const { return x; }
    reference get() { return x; }
};

constexpr struct ReturnIdentity {
    template< class X > 
    constexpr Identity<X> operator () ( X x ) { 
        return {move(x)};
    }
} identity{};

template< class I >
using IdentGet = decltype( declval<I>().get() );

namespace monad {
    template< class _X > struct Monad< Identity<_X> > {
        template< class _, class X >
        constexpr static Identity< X > mreturn( X x ) {
            return { move(x) };
        }

        template< class I, class F, class X = IdentGet<I> >
        static auto mbind( I&& i, F&& f ) 
            -> decltype( declval<F>()( declval<X>() ) )
        {
            return forward<F>(f)( forward<I>(i).get() );
        }
    };

    template< class _X > struct Functor< Identity<_X> > {
        template< class F > 
        static constexpr auto fmap( F&& f, Identity<_X> i )
            -> Identity < 
                decltype( declval<F>()(i.get()) )
            >
        {
            return identity( forward<F>(f)( move(i.get()) ) );
        }
    };
} // namespace monad

template< class F, class G > struct  MComposition {
    F f;
    G g;


    constexpr MComposition( F f, G g ) : f(move(f)), g(move(g)) { }

    template< class ...X >
    constexpr auto operator () ( X&& ...x )
        -> decltype( g(declval<X>()...)>>=f )
    {
        return g( forward<X>(x)... ) >>= f;
    }
};

// TODO: Chainable?
constexpr struct MCompose : Binary<MCompose> {
    using Binary<MCompose>::operator();

    template< class G, class F >
    constexpr MComposition<G,F> operator () ( G g, F f ) {
        return { move(g), move(f) };
    }
} mcompose{};


constexpr struct FCompose : Binary<FCompose> {
    using Binary<FCompose>::operator();

    template< class F, class G,
              class FM = decltype( fmap(declval<F>()) ) >
    constexpr auto operator () ( F f, G g ) -> NCompoposition<FM,G> {
        return { fmap( move(f) ), move(g) };
    }
} fcompose{};


} // namespace pure

