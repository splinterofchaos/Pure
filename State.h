
#include "Pure.h"
#include "Arrow.h"

#pragma once

namespace pure {

namespace state {

template< class S, class A, template<class...>class M, class F > 
struct StateT { 
    using function_type = F;
    using pair_type = std::pair<A,S>;

    template< class P >
    using monad_type = M<P>;

    function_type f; 

    template< class X >
    using Result = Result< F, X >; // Should be of Monad type.

    template< class X >
    constexpr monad_type<pair_type> runState( X&& x ) {
        return f( forward<X>(x) );
    }
};

template< class S, class A >
using StateIdent = Identity< std::pair<A,S> >;

template< class S, class A, class F > 
using State = StateT< S, A, Identity, F >;

template< class S, class A=S, class F, 
          class I = StateIdent< S, A > >
constexpr auto state( F f ) 
    -> State< S, A, Composition<pure::Return<I>,F> >
{
    return { compose( pure::Return<I>(), move(f) ) }; 
}

template< class S, template<class...>class M, class A=S, class F,
          class R = pure::Return<M< std::pair<A,S> >> >
constexpr auto stateT( F f ) 
    -> StateT< S, A, M, Composition<R,F> >
{
    return { compose( R(), move(f) ) };
}

constexpr struct Run {
    template< class ST, class X > 
    constexpr auto operator () ( ST&& s, X&& x ) 
        -> decltype( declval<ST>().runState(declval<X>()) )
    {
        return forward<ST>(s).runState( forward<X>(x) );
    }
} run{};

constexpr struct Fst {
    template< class T >
    constexpr auto operator() ( T&& t ) 
        -> Decay<decltype( get<0>(declval<T>()) )>
    {
        return get<0>( forward<T>(t) );
    }
} fst{};

constexpr struct Snd {
    template< class T >
    constexpr auto operator() ( T&& t ) 
        -> Decay<decltype( get<1>(declval<T>()) )>
    {
        return get<1>( forward<T>(t) );
    }
} snd{};

constexpr auto eval = fcompose( fst, run );
constexpr auto exec = fcompose( snd, run );

} // namespace state

namespace monad {

template< class S, class A, template<class...>class M, class F > 
struct Functor< state::StateT<S,A,M,F> > {
    /* 
     * fmap g (State f) =
     *      StateT $ \x -> (f x) >>= \(a,s) -> return (g a, s)
     */
    template< class G > static
    auto fmap( G g, state::StateT<S,A,M,F> s ) 
        -> state::StateT< S, A, M, FCompose<arr::First::template type<G>,F> >
    {
        return { fcompose( arr::first(move(g)), move(s.f) ) };
    }
};

template< class S, class A, template<class...>class M, class F >
struct Monad< state::StateT<S,A,M,F> > {
    using State = state::StateT<S,A,M,F>;
    using P = typename State::pair_type;

    template< class Pair >
    using Mon = typename State::template monad_type<Pair>;

    static constexpr P _return( A a, S s ) {
        return { move(a), move(s) }; 
    }

    template< class ST, class X >
    static constexpr Closet<decltype(_return),X> mreturn( X x ) {
        return closet( _return, move(x) );
    }

    template< class K >
    static constexpr auto mbind( State m, K k ) 
        -> state::StateT< S, A, M, decltype (
            mcompose( compose(arr::unsplit(state::run),arr::first(move(k))), 
                      closet(state::run,m) )
        ) >
    {
        return { mcompose ( 
            compose( arr::unsplit(state::run), arr::first(move(k)) ),
            closet( state::run, move(m) ) 
        ) };
    }

    template< class SA, class SB >
    static constexpr auto mdo( SA&& sa, SB&& sb ) 
        -> decltype( mbind( forward<SA>(sa), pure(forward<SB>(sb)) ) )
    {
        return mbind( forward<SA>(sa), pure(forward<SB>(sb)) );
    }
};

} // namespace monad

namespace state {

constexpr struct ReturnPair {
    template< class X, class Y >
    constexpr std::pair<X,Y> operator () ( X x, Y y ) {
        return { move(x), move(y) };
    }
} returnPair{};

/* returnState a = State (\s -> itentity . (\s'->(a,s)) ) */
template< class S, template<class...>class M=Identity, class A = S >
struct ReturnT {
    using pair_type = std::pair<A,S>;
    using monad_type = M< pair_type >;

    using state_type = StateT < 
            S, A, M,
            Composition< pure::Return<monad_type>, 
                         Closet<ReturnPair,A> >
    >;

    static constexpr Closet<ReturnPair,A> ret( A a ) {
        return { returnPair, move(a) };
    }

    constexpr state_type operator () ( A a ) {
        return stateT<S,M,A>( ret(move(a)) );
    }
};

template< class S, class A=S > struct Return 
    : public ReturnT<S,Identity,A> 
{
};

template< class S, class A >
constexpr auto returnState( A a ) 
    -> typename Return<S,A>::state_type 
{
    return Return<S,A>()( move(a) );
}

template< class S, template<class...>class M=Identity, class A >
constexpr auto returnStateT( A a ) 
    -> typename ReturnT<S,M,A>::state_type 
{
    return ReturnT<S,M,A>()( move(a) );
}

template< class ... > struct MonadState;

template< class S, template<class...>class M = Identity,
          class MS = MonadState< StateT<S,S,M,Id> > >
constexpr auto get() -> decltype( MS::get() ) {
    return MS::get();
}

template< template<class...>class M = Identity, class S, 
          class MS = MonadState< StateT<S,S,M,Id> > >
constexpr auto put( S s ) -> decltype( MS::put(move(s)) ) {
    return MS::put( move(s) );
}

template< class S, template<class...>class M = Identity >
struct SPut {
    using MS = MonadState< StateT<S,S,M,Id> >;
    using type = decltype( put( declval<S>() ) );
    constexpr type operator () ( S s ) { return put( move(s) ); }
};

template< class S, template<class...>class M = Identity, class F >
constexpr auto modify( F f ) 
    -> decltype( get<S,M>()>>=compose(SPut<S,M>(),f) )
{
    return get<S,M>() >>= compose( SPut<S,M>(), f );
}

template< class S, template<class...>class M = Identity, class F,
          class Fst = typename arr::First::template type<F>,
          class G = Composition< Fst, arr::Splitter > >
constexpr auto gets( F f ) 
    -> decltype( stateT<S,M>( declval<G>() ) )
{
    return stateT<S,M>( compose( arr::first(move(f)), arr::splitter ) );
}

template< class S, template<class...>class M, class _F >
struct MonadState< StateT<S,S,M,_F> > {
    template< class F >
    using State = StateT<S,S,M,F>;

    using P = std::pair<S,S>;

    using Monad = M<P>;

    using RetM  = pure::Return< Monad >;

    using GetF = Composition< RetM, arr::Splitter >;
    using SetF = Composition< RetM, RCloset<ReturnPair,S> >;


    //static constexpr State<GetF> getter = state<S,S>( splitter );

    static constexpr State<GetF> get() { return stateT<S,M>( arr::splitter ); }

    static constexpr State<SetF> put( S s ) {
        return stateT<S,M>( rcloset(returnPair,move(s)) );
    }

};
}

}
