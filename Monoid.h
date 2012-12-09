
#pragma once

namespace pure {

namespace monoid {

/*
 * Monoid M :
 *      mempty -> M
 *      mappend M M -> M
 *      mconcat [M] -> M
 *
 *      mconcat = foldr mappend mempty
 */
template< class... > struct Monoid;

template< class M, class Mo = Monoid<Cat<M>> >
constexpr auto mempty() -> decltype( Mo::template mempty<M>() ) {
    return Mo::template mempty<M>();
}

template< class M > struct MEmpty {
    constexpr auto operator () () -> decltype( mempty<M>() )
    {
        return mempty<M>();
    }
};

constexpr struct MAppend : Chainable<MAppend> {
    using Chainable<MAppend>::operator();

    template< class M1, class M2, class Mo = Monoid<Cat<M1>> >
    constexpr auto operator() ( M1&& a, M2&& b )
        -> decltype( Mo::mappend(declval<M1>(),declval<M2>()) )
    {
        return Mo::mappend( forward<M1>(a), forward<M2>(b) );
    }
} mappend{};

template< class X, class Y >
constexpr auto operator + ( X&& x, Y&& y )
    -> decltype( mappend(declval<X>(),declval<Y>()) )
{
    return mappend( std::forward<X>(x), std::forward<Y>(y) );
}

constexpr struct MConcat {
    template< class S, class V = list::SeqVal<S>, class M = Monoid<Cat<V>> >
    constexpr auto operator () ( S&& s )
        -> decltype( M::mconcat(declval<S>()) )
    {
        return M::mconcat( forward<S>(s) );
    }
} mconcat{};


constexpr struct _MConcat {
    template< class SS, class S = typename SS::value_type >
    constexpr S operator () ( const SS& ss ) {
        return list::foldr( mappend, mempty<S>(), ss );
    }
} _mconcat{};

template< class F > struct Monoid< F > {
    template< class G >
    constexpr BComposition<MAppend,F,G> mappend( F f, G g ) {
        return bcompose( mappend, std::move(f), std::move(g) );
    }

    static constexpr auto mconcat = _mconcat;
};

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
        return x and y ? Just( *forward<M>(x) + *forward<M>(y) )
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

    static P mempty() { return P( monoid::mempty<X>(), monoid::mempty<Y>() ); }

    static P mappend( const P& a, const P& b ) {
        return P( a.first  + b.first,
                  a.second + b.second );
    }

    template< class S > static P mconcat( const S& s ) {
        return list::foldr( mappend, mempty(), s );
    }
};

/* dual(x) = Dual(x) -- where type X is monoidal. */
template< class X > struct Dual {
    X x;

    constexpr Dual( X&& y ) : x( std::forward<X>(y) ) { }

    template< class Y >
    constexpr Dual( const Dual<Y>& y ) : x( y() ) { }

    template< class Y >
    constexpr Dual( std::initializer_list<Y> y ) : x( std::move(y) ) { }

    constexpr X operator () () { return x; }
};

template< class X > struct Monoid< Dual<X> > {
    template< class D, class Y >
    static constexpr D mempty( Y&& y ) {
        return D( std::forward<Y>(y) );
    }

    template< class Y >
    static Dual<Y> mappend( const Dual<X>& dx, Dual<Y> dy ) {
        dy.x = std::move(dy.x) + dx.x;
        return dy;
    }
};

/* Endo F = F -- where F is a function with the signature X(X) */
template< class F > struct Endo : Forwarder<F> {
    template< class G >
    constexpr Endo( G&& g ) : Forwarder<F>( std::forward<G>(g) ) { }
};

constexpr auto endo = MakeT<Endo>();

template< class F > struct Monoid< Endo<F>  > {
    template< class E >
    static constexpr Endo<Id> mempty() { return endo(id); }

    static constexpr auto mappend = ncompose( endo, compose );
};

struct All {
    bool b = true;

    constexpr All() {}
    constexpr All( bool b ) : b(b) { }

    constexpr operator bool () { return b; }
};

constexpr All operator && ( All a, All b ) {
    return a.b && b.b;
}

template<> struct Monoid< All > {
    template< class _ >
    static constexpr All mempty() { return true; }
    static constexpr auto mappend = And();
    static constexpr auto mconcat = compose( Make<All>(),
                                             list::all(id) );
};

struct Any {
    bool b = false;

    constexpr Any() {}
    constexpr Any( bool b ) : b(b) { }

    constexpr operator bool () { return b; }
};

constexpr Any operator || ( Any a, Any b ) {
    return a.b || b.b;
}

template<> struct Monoid< Any > {
    template< class _ >
    static constexpr Any mempty() { return false; }
    static constexpr auto mappend = Or();
    static constexpr auto mconcat = compose( Make<Any>(),
                                             list::any(id) );
};

struct Sum {
    int sum = 0;

    constexpr Sum() {}
    constexpr Sum( int x ) : sum(x) { }

    constexpr operator int () { return sum; }
};

constexpr Sum operator + ( Sum a, Sum b ) {
    return a.sum + b.sum;
}

template<> struct Monoid< Sum > {
    template< class _ >
    static constexpr Sum mempty() { return 0; }

    static constexpr Sum mappend( Sum a, Sum b ) {
        return Sum( a + b );
    }

    static constexpr auto mconcat = list::sum;
};

struct Product {
    int prod = 1;

    constexpr Product() {}
    constexpr Product( int x ) : prod(x) { }

    constexpr operator int () { return prod; }
};

constexpr Product operator * ( Product a, Product b ) {
    return a.prod * b.prod;
}

template<> struct Monoid< Product > {
    template< class _ >
    static constexpr Sum mempty() { return 0; }
    static constexpr auto mappend = mult;
    static constexpr auto mconcat = list::product;
};

} // namespace monoid

} // namespace pure
