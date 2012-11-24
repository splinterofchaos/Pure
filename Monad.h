
#pragma once

#include "Data.h"
#include "List.h"
#include "Category.h"

namespace pure {

namespace monad {

using namespace pure::category;

/* 
 * Functor F:
 *      fmap f (F a) -> F (f a)
 *
 * fmap maps a function, f, to a functor, F such that for each f:X->Y, there
 * exists an F(f):F(X)->F(Y). 
 *
 * fmap f F(x) = F(f x) 
 *
 * In Haskell, each mappable type class must be specialized with a Functor
 * instance. Likewise, here, we must use template specialization for each type
 * we want mappable.
 */
template< class ...F > struct Functor;

constexpr struct FMap : Binary<FMap> {
    using Binary<FMap>::operator();

    template< class F, class G, class ...H,
              class Fn = Functor< Cat<G> > >
    constexpr auto operator () ( F&& f, G&& g, H&& ...h )
        -> decltype( Fn::fmap(declval<F>(),declval<G>(),declval<H>()...) )
    {
        return Fn::fmap( forward<F>(f), forward<G>(g), forward<H>(h)... );
    }

    template< class F, class X,
              class Fn = Functor< category::sequence_type > >
    constexpr auto operator () ( F&& f, const std::initializer_list<X>& l )
        -> decltype( Fn::fmap(declval<F>(),l) )
    {
        return Fn::fmap( forward<F>(f), l );
    }
} fmap{};

/* fmap f g = compose( f, g ) */
template< class Function >
struct Functor<Function> {
    template< class F, class G >
    static constexpr auto fmap( F&& f, G&& g )
        -> decltype( compose(declval<F>(),declval<G>()) )
    {
        return compose( forward<F>(f), forward<G>(g) );
    }
};

/* fmap f Pair(x,y) = Pair( f x, f y ) */
template< class X, class Y >
struct Functor< std::pair<X,Y> > {
    template< unsigned N, class P >
    using Nth = decltype( get<N>( declval<P>() ) );

    template< class F, class ...P >
    using PX = decltype( declval<F>()( declval<Nth<0,P>>()... ) );
    template< class F, class ...P >
    using PY = decltype( declval<F>()( declval<Nth<1,P>>()... ) );

    template< class F, class ...P >
    static constexpr auto fmap( F&& f, P&& ...p ) 
        -> decltype (
            std::make_pair( declval<PX<F,P...>>(), declval<PY<F,P...>>() )
        )
    {
        return std::make_pair( forward<F>(f)(forward<P>(p).first...),
                               forward<F>(f)(forward<P>(p).second...) );
    }
};

template<>
struct Functor< maybe_type > {
    template< class M > 
    static constexpr bool each( const M& m ) {
        return (bool)m;
    }

    template< class M1, class ...Ms >
    static constexpr bool each( const M1& m1, const Ms& ...ms ) {
        return (bool)m1 && each( ms... );
    }

    /*
     * f <$> Just x = Just (f x)
     * f <$> Nothing = Nothing
     */
    template< class F, class ...M >
    static constexpr auto fmap( F&& f, M&& ...m ) 
        -> decltype( data::Just(declval<F>()( *declval<M>()... )) )
    {
        return each( forward<M>(m)... ) ? 
            data::Just( forward<F>(f)( *forward<M>(m)... ) ) : nullptr;
    }
};

template<>
struct Functor< category::sequence_type > {
    /* f <$> [x0,x1,...] = [f x0, f x1, ...] */
    template< class F, class ...S >
    static constexpr decltype( list::map(declval<F>(),declval<S>()...) )
    fmap( F&& f, S&& ...s ) {
        return list::map( forward<F>(f), forward<S>(s)... );
    }
};

template< class L, class R >
struct Functor< data::Either<L,R> > {
    template< class F, class FR = decltype( declval<F>()(declval<R>()) ) >
    static data::Either<L,FR>  fmap( F&& f, const data::Either<L,R>& e ) {
        return e.right ? data::Right<L>( forward<F>(f)(*e.right) ) 
                       : data::Left<FR>( *e.left );
    }
};

/*
 * Monad M :
 *   mdo (M a) (M b) -> M b        -- (M-do) Do from left to right.
 *   mbind (M a) (a -> M b) -> M b -- Apply to the right the value of the left.
 *   mreturn a -> M a              -- Lift a.
 *   mfail String -> M a           -- Produce a failure value.
 *
 *   a >>  b = mdo   a b
 *   m >>= f = mbind m f
 */
template< class ...M > struct Monad;

/* m >> k */
constexpr struct Mdo : Chainable<Mdo> {
    using Chainable<Mdo>::operator();

    template< class A, class B,
              class Mo = Monad<Cat<A>> >
    constexpr auto operator () ( A&& a, B&& b )
        -> decltype( Mo::mdo(declval<A>(),declval<B>()) )
    {
        return Mo::mdo( forward<A>(a), forward<B>(b) );
    }
} mdo{};

constexpr struct Mbind : Chainable<Mbind> {
    using Chainable<Mbind>::operator();

    /* m >>= k */
    template< class M, class F,
              class Mo = Monad<Cat<M>> >
    constexpr auto operator () ( F&& f, M&& m )
        -> decltype( Mo::mbind(declval<F>(),declval<M>()) )
    {
        return Mo::mbind( forward<F>(f), forward<M>(m) );
    }
} mbind{};

/* return<M> x = M x */
template< class M, class X > 
M mreturn( X&& x ) {
    return Monad< Cat<M> >::template mreturn<M>( forward<X>(x) );
}

template< template<class...>class M, class X, class _X = Decay<X> >
M<_X> mreturn( X&& x ) {
    return Monad< Cat<M<_X>> >::template mreturn<M<_X>>( forward<X>(x) );
}

/* mreturn () = (\x -> return x) */
template< class M, class Mo = Monad<Cat<M>> > 
constexpr auto mreturn() -> decltype( Mo::mreturn() ) { 
    return Mo::mreturn();
}

template< template<class...> class M > struct MReturn {
    template< class X >
    constexpr auto operator () ( X&& x )
        -> decltype( mreturn<M>(std::declval<X>()) )
    {
        return mreturn<M>( std::forward<X>(x) );
    }
};

template< class M > struct Return {
    template< class X >
    constexpr auto operator() ( X&& x ) 
        -> decltype( mreturn<M>(declval<X>()) ) 
    {
        return mreturn<M>( forward<X>(x) );
    }
};


template< class M >
M mfail( const char* const why ) {
    return Monad< Cat<M> >::template mfail<M>( why );
}

template< class X, class Y >
auto operator >>= ( X&& x, Y&& y )
    -> decltype( mbind(declval<Y>(),declval<X>()) )
{
    return mbind( forward<Y>(y), forward<X>(x) );
}

template< class X, class Y >
decltype( mdo(declval<X>(),declval<Y>()) ) 
operator >> ( X&& x, Y&& y ) {
    return mdo( forward<X>(x), forward<Y>(y) );
}

template<> struct Monad< category::sequence_type > {
    template< class S, class X >
    constexpr static S mreturn( X&& x ) { 
        return S{ forward<X>(x) }; 
    }

    template< class S >
    constexpr static S mfail(const char*) { return S(); }

    template< class S, class YS >
    static YS mdo( const S& a, const YS& b ) {
        // In Haskell, this is defined as
        //      m >> k = foldr ((++) . (\ _ -> k)) [] m
        // In other words, 
        //      for each element in a, duplicate b.
        //      [] >> k = []
        YS c;
        auto size = list::length( a );
        while( size-- )
            c = list::append( move(c), b );
        return c;
    }

    template< class F, class S >
    static auto mbind( F&& f, const S& s )
        -> decltype( list::concatMap(declval<F>(),s) )
    {
        return list::concatMap( forward<F>(f), s );
    }
};

template< class P > struct IsPointerImpl { 
    using reference = decltype( *declval<P>() );
    using bool_type = decltype( (bool)declval<P>() );
};

template<> struct Monad< maybe_type > {
    template< class M > using traits = maybe_traits<M>;
    template< class M > using value_type = typename traits<M>::value_type;
    template< class M > using smart_ptr  = typename traits<M>::smart_ptr;

    template< class M, class X, class P = smart_ptr<M> >
    static P mreturn( X&& x ) {
        return P( new Decay<X>(forward<X>(x)) );
    }

    template< class M >
    using Return = smart_ptr<M> (*) ( value_type<M> );

    static constexpr auto _ret = data::Just;

    static constexpr decltype(_ret) mreturn() { return _ret; }

    template< class M >
    static constexpr smart_ptr<M> mfail(const char*) { return nullptr; }

    template< class M, class PZ >
    static constexpr PZ mdo( const M& x, PZ&& z ) {
        return x ? forward<PZ>(z) : nullptr;
    }

    template< class M, class F >
    using Result = Result< F, value_type<M> >;

    template< class M, class F >
    static constexpr Result<M,F> mbind( F&& f, M&& m ) {
        return m ? forward<F>(f)( *forward<M>(m) ) : nullptr;
    }
};

/*
 * liftM f m = m >>= return . f
 * Similar to fmap, but not all Monads are Functors.
 */
constexpr struct LiftM : Binary<LiftM> {
    using Binary<LiftM>::operator();

    template< class F, class M, class D = Decay<M> >
    constexpr auto operator () ( F&& f, M&& m )
        -> decltype( declval<M>() >>= Return<D>()^declval<F>() )
    {
        return forward<M>(m) >>= Return<D>() ^ forward<F>(f);
    }

    template< class F, class A, class B, class ...C >
    constexpr auto operator () ( F&& f, A&& a, B&& b, C&& ...c )
        -> decltype( declval<A>() >>= compose (
                rcloset( LiftM(), declval<B>(), declval<C>()... ),
                closet(declval<F>())
            ) )
    {
        return forward<A>(a) >>= compose (
            rcloset( LiftM(), forward<B>(b), forward<C>(c)... ),
            closet(forward<F>(f))
        );
    }
} liftM{};

/* liftCons my mx = mx >>= (\x -> liftM( cons x, my )) */
constexpr struct LiftCons {
    struct Close {
        template< class M, class X >
        constexpr auto operator () ( M&& m, X&& x )
            -> decltype( liftM( list::cons.with(declval<X>()),
                         declval<M>() ) )
        {
            return liftM( list::cons.with(forward<X>(x)), forward<M>(m) );
        }
    };

    /*
     * Haskell says
     *   k m m' = do { x <- m; xs <- m'; return (x:xs) }
     *
     * But this works backwords.
     */
    template< class MX, class MY >
    constexpr auto operator () ( MY&& my, MX&& mx )
        -> decltype( declval<MX>() >>= closure(Close(),declval<MY>()) )
    {
        return forward<MX>(mx) >>= closure( Close(), forward<MY>(my) );
    }
} liftCons{};

/*
 * sequence [mx] = m[x]
 * sequence [Just 1, Just 2] = Just [1,2]
 * sequence [[1,2],[3,4]] = [[1,3],[2,4]]
 */
constexpr struct Sequence {
    template< template<class...> class S, template<class...> class M, class X >
    constexpr M<S<X>> operator () ( const S<M<X>>& smx ) {
        return list::foldl( liftCons, mreturn<M>(S<X>{}), smx );
    }
} sequence{};

constexpr auto mapM = ncompose( sequence, list::map );

/*
 * MonadPlus M :
 *      mzero -> M
 *      mplus M M -> M
 *
 *      mzero >>= f = mzero
 */
template< class ...F > struct MonadPlus;

template< class M, class Mo = MonadPlus<Cat<M>> >
constexpr auto mzero() -> decltype( Mo::template mzero<M>() ) {
    return Mo::template mzero<M>();
}

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

template< class X, class Y >
auto operator + ( X&& x, Y&& y )
    -> decltype( mplus(declval<X>(),declval<Y>()) )
{
    return mplus( std::forward<X>(x), std::forward<Y>(y) );
}

template<> struct MonadPlus< category::sequence_type > {
    template< class S >
    static S mzero() { return S(); }

    static constexpr auto mplus = list::append;
};

template<> struct MonadPlus< category::maybe_type > {
    template< class M >
    static M mzero() { return nullptr; }

    template< class P >
    static constexpr P mplus( const P& a, const P& b ) {
        return a ? data::Just(*a) : data::Just(*b);;
    }

    template< class P >
    static constexpr ERVal<P,P> mplus( P&& a, P&& b ) {
        return a ? a : b;
    }
};

/*
 * gaurd(b) >> m = m, if b.
 * In Haskell, this would return an M (),
 * but I don't see how to translate that.
 */
template< template<class...> class M >
constexpr M<bool> guard( bool b ) {
    return b ? mreturn<M>(true) : mzero<M<bool>>();
}

/* gaurd(b,m) = gaurd(b) >> m */
template< class M >
constexpr M guard( bool b, M m )
{
    return b ? m : mzero<M>();
}

template< template<class...> class M >
struct Guard {
    constexpr M<bool> operator () ( bool b ) {
        return guard<M>(b);
    }

    template< class ...X >
    constexpr M<X...> operator () ( bool b, M<X...> m ) {
        return guard( b, move(m) );
    }
};

constexpr struct GuardIf {
    template< class P, class F, class X, class R = Result<F,X> >
    constexpr R operator () ( P&& p, F&& f, X&& x )
    {
        return forward<P>(p)(x) ? forward<F>(f)(forward<X>(x)) : mzero<R>();
    }

    template< class P, class F >
    constexpr Closet<GuardIf,P,F> operator () ( P&& p, F&& f )
    {
        return closet( GuardIf(), forward<P>(p), forward<F>(f) );
    }
} guardIf{};

/* msum( {x,y} ) = mplus(x,y) */
constexpr struct MSum {
    template< template<class...> class S, class MX >
    MX operator () ( const S<MX>& ms ) const {
        return list::foldr( mplus, mzero<MX>(), ms );
    }
} msum{};

/* kcompose(f,g)(x) = f(x) >>= g */
template< class F, class G >
struct KComposition {
    F f;
    G g;

    template< class _F, class _G >
    constexpr KComposition( _F&& f, _G&& g )
        : f(forward<_F>(f)), g(forward<_G>(g))
    {
    }

    template< class X >
    constexpr auto operator () ( X&& x )
        -> decltype( f(declval<X>()) >>= g )
    {
        return f( forward<X>(x) ) >>= g;
    }
};

constexpr auto kcompose = ConstructBinary<KComposition>();

/*
 * join( {{1},{2,3}} ) = {1,2,3}
 * join( Just(Just(2)) ) = Just(2)
 */
constexpr auto join = mbind( id );

constexpr auto zipWithM = ncompose( sequence, list::zipWith );

/*
 * foldM(b,x,xs) = b(x,head(xs)) >>= foldM(b,_,tail(xs)
 *      where b is a function from (a,b) to some monad.
 *
 * foldM(f,0,{1,2,3}) = f(0,1) >>= f(_,2) >>= f(_,3)
 */
constexpr struct FoldM {
    static constexpr struct DoFold {
        template< class B, class S, class I,
                  class R = Result< B, I, list::SeqVal<S> > >
        R operator () ( B&& b, const S& s, I&& i ) const {
            if( list::null(s) )
                return mreturn<R>( forward<I>(i) );

            auto acc = forward<B>(b)( forward<I>(i), list::head(s) );
            return acc >>= closure (
                DoFold(), forward<B>(b), list::tail_wrap( s )
            );
        }
    } do_fold{};

    template< class Binary, class Init, class S,
              class R = Result< Binary, Init, Result<list::Head,S> > >
    R operator () ( Binary&& b, Init&& i, const S& s ) const
    {
        return DoFold() (
            forward<Binary>(b), list::range(s), forward<Init>(i)
        );
    }
} foldM{};

constexpr auto replicateM = ncompose( sequence, list::replicate );

constexpr auto ap = closure( liftM, id );

constexpr struct MFilter {
    template< class R > struct DoFilter{
        template< class P, class X >
        constexpr R operator () ( P&& p, X&& x ) {
            return forward<P>(p)(x) ? mreturn<R>(forward<X>(x)) : mzero<R>();
        }
    };

    template< class P, class M, class R = Decay<M> >
    constexpr R operator () ( P&& p, M&& m ) {
        return forward<M>(m) >>= closet( DoFilter<R>(), forward<P>(p) );
    }
} mfilter{};

} // namespace monad

} // namespace pure
