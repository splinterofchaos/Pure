
#include "Common.h"
#include "Functional.h"
#include "tpl.h"

#pragma once

#include <set>
#include <vector>
#include <list>
#include <memory>
#include <limits>

namespace pure { 

constexpr struct Begin {
    template< class X >
    constexpr auto operator () ( X&& x )
        -> decltype( std::begin(declval<X>()) )
    {
        return std::begin( std::forward<X>(x) );
    }
} begin{};

constexpr struct End {
    template< class X >
    constexpr auto operator () ( X&& x )
        -> decltype( std::end(declval<X>()) )
    {
        return std::end( std::forward<X>(x) );
    }
} end{};

constexpr struct Next {
    template< class X >
    constexpr auto operator () ( X&& x, size_t n=1 )
        -> decltype( std::next(declval<X>()) )
    {
        return std::next( std::forward<X>(x), n );
    }
} next{};

constexpr struct Prev {
    template< class X >
    constexpr auto operator () ( X&& x, size_t n=1 )
        -> decltype( std::prev(declval<X>()) )
    {
        return std::prev( std::forward<X>(x), n );
    }
} prev{};

namespace category {

    template< class I >
    auto _dist( const I& i ) -> decltype( i - i );
    template< class I >
    auto _dist( ... ) -> int;

    template< class Seq > struct sequence_traits {
        using sequence   = Seq;
        using iterator   = Decay<decltype( begin(declval<Seq>()) )>;
        using distance_type = decltype( _dist<iterator>(declval<iterator>()) );
        using reference  = decltype( *declval<iterator>() );
        using value_type = Decay<reference>;
    };
}

namespace list {

template< class S >
using SeqRef = typename category::sequence_traits<S>::reference;
template< class S >
using SeqVal = typename category::sequence_traits<S>::value_type;
template< class S >
using SeqIter = typename category::sequence_traits<S>::iterator;
template< class S >
using SeqDist = typename category::sequence_traits<S>::distance_type;

template< class I >
using ItDist = decltype( declval<I>() - declval<I>() );

template< class S >
constexpr auto _length( S&& s ) -> decltype( declval<S>().size() )
{
    return forward<S>(s).size();
}

template< class S >
constexpr size_t _length( const S& s ) {
    return std::distance( begin(s), end(s) );
}

template< class X, unsigned N >
constexpr size_t _length( X (&)[N] ) {
    return N;
}

template< size_t N >
constexpr size_t _length( const char (&)[N] ) {
    return N - 1;
}


constexpr struct Length {
    template< class S >
    constexpr size_t operator () ( S&& s ) {
        return _length( forward<S>(s) );
    }
} length{};

constexpr struct Null {
    template< class S >
    constexpr bool operator () ( const S& s ) {
        return begin(s) == end(s);
    }
} null{};

constexpr auto notNull = fnot( null );

constexpr struct Head {
    template< class S >
    constexpr SeqRef<S> operator () ( S&& s, size_t n = 0 ) {
        return *next( begin(forward<S>(s)), n );
    }

    template< class X >
    constexpr X operator () ( const std::initializer_list<X>& l, size_t n = 0 ) {
        return *next( begin(l), n );
    }

    constexpr RPart<Head,size_t> with( size_t n ) {
        return rcloset( Head(), n );
    }
} head{};

constexpr struct Last {
    template< class S >
    constexpr SeqRef<S> operator () ( S&& s, size_t n = 0 ) {
        return *prev( end(forward<S>(s)), n+1 );
    }

    template< class X >
    constexpr X operator () ( const std::initializer_list<X>& l, size_t n = 0 ) {
        return *prev( end(l), n+1 );
    }

    constexpr RPart<Last,size_t> with( size_t n ) {
        return rcloset( Last(), n );
    }
} last{};

template< class S, class I = typename S::iterator > struct Range {
    I b, e;
    
    using sequence_type = Decay<S>;
    using iterator = I; 
    using reference = decltype( *declval<I>() );
    using value_type = Decay<reference>;

    template< class _I >
    constexpr Range( _I b, _I e ) : b(b), e(e) { }

    constexpr I begin() { return b; }
    constexpr I end()   { return e; }

    operator sequence_type () const { return sequence_type( b, e ); }
};

template< class S, class I1, class I2 >
struct Range< Range<S,I1>, I2 > : public Range<S,I2> {
    using parent = Range<S,I2>;

    template< class _I >
    constexpr Range( _I b, _I e ) : parent( b, e ) { }

    using sequence_type = typename parent::sequence_type;
    using iterator      = typename parent::iterator; 
    using reference     = typename parent::reference;
    using value_type    = typename parent::value_type;
};

template< class S, class R = Range<S,SeqIter<S>> >
constexpr R range( S&& s ) {
    return R( begin(forward<S>(s)), end(forward<S>(s)) );
}

template< class S, class I, class R = Range<S,I> >
constexpr auto range( I a, I b ) -> R {
    return R( a, b );
}

template< class S, class R = Range<S,SeqIter<S>> >
constexpr R range( S&& s,  size_t b, size_t e ) {
    return R( next(begin(forward<S>(s)),b), next(begin(forward<S>(s)),e) );
}

template< class S, class R = Range<S,SeqIter<S>> >
constexpr R range( S&& s,  size_t e ) {
    return range( forward<S>(s), 0, e );
}

template< class S, class R = Range<S,SeqIter<S>> >
constexpr R tail_wrap( S&& s ) {
    return length(forward<S>(s)) ? R( next( begin(forward<S>(s)) ), 
                                      end( forward<S>(s) ) )
        : range( forward<S>(s) );
}

template< class S, class R = Range<S,SeqIter<S>> >
constexpr R tail_wrap( size_t n, S&& s ) {
    return R( next(begin( forward<S>(s) ),n),
              end(forward<S>(s)) );
}

/* 
 * Prevent infinitely recursive types! 
 * tail_wrap( tail_wrap(r) ) will not imply a Range<Range<S,I>,I>.
 */
template< class S, class I >
Range<S,I> tail_wrap( size_t n, Range<S,I> r ) {
    r.b = next(r.b, n);
    return r;
}

template< class S, class I >
Range<S,I> tail_wrap( Range<S,I> r ) {
    return tail_wrap( 1, r );
}

template< class S, class R = Range<S,SeqIter<S>> >
constexpr R init_wrap( S&& s ) {
    return length(forward<S>(s)) ? R( begin(forward<S>(s)), 
                                      prev( end(forward<S>(s)) ) )
        : range( forward<S>(s) );
}

template< class S, class R = Range<S,SeqIter<S>> >
constexpr R init_wrap( size_t n, S&& s ) {
    return R ( 
        begin( forward<S>(s) ),
        next( begin(forward<S>(s)), n )
    );
}

template< class S, class I >
Range<S,I> init_wrap( size_t n, Range<S,I> r ) {
    r.e = prev(r.e, n);
    return r;
}

template< class S, class I >
Range<S,I> init_wrap( Range<S,I> r ) {
    return init_wrap( 1, r );
}

template< class S, class I = SeqIter<S>, 
          class R = Range<S,std::reverse_iterator<I>> >
constexpr R reverse_wrap( S&& s ) {
    return R( end(forward<S>(s)), begin(forward<S>(s)) );
}

template< class S > struct CanBackInsertT {
    template< class _S > static auto f(_S& s) 
        -> decltype( s.push_back(s.front()), std::true_type() );
    template< class _S > static auto f(...) -> std::false_type;

    using type = decltype( f<S>( declval<S&>() ) );
};

template< class S > using CanBackInsert = typename CanBackInsertT<S>::type;

template< class S, class R >
using EBackInsert = typename std::enable_if< CanBackInsert<S>::value, R >::type;
template< class S, class R >
using XBackInsert = typename std::enable_if< not CanBackInsert<S>::value, R >::type;

template< class S, class _S = Decay<S> >
auto tailInserter( S&& s ) -> EBackInsert< S, std::back_insert_iterator<_S> > {
    return std::back_inserter( forward<S>(s) );
}

template< class S, class _S = Decay<S> >
auto tailInserter( S&& s ) -> XBackInsert< S, std::insert_iterator<_S> > {
    return std::inserter( forward<S>(s), end(forward<S>(s)) );
}

template< class R, class S >
R dupExactly( const S& s ) {
    R r;
    copy( begin(s), end(s),
          tailInserter(r) );
    return r;
}

template< class R, class S >
R dupExactly( const S& s, size_t n, size_t off = 0 ) {
    R r;
    copy_n( next( begin(s), off ),
            std::min( n+1, length(s) ) - 1,
            tailInserter(r) );
    return r;
}

template< template<class...> class _R, class S,
          class R = _R< SeqVal<S> > >
R dupTo( S&& s ) {
    return dupExactly<R>( forward<S>(s) );
}

template< template<class...> class _R, class S,
          class R = _R< SeqVal<S> > >
R dupTo( S&& s, size_t n, size_t off = 0 ) {
    return dupExactly<R>( forward<S>(s), n, off );
}

template< template<class...> class R > struct DupTo {
    template< class S > using type = R< SeqVal<S> >;

    template< class S >
    type<S> operator() ( S&& s ) const {
        return dupTo<R>( forward<S>(s) );
    }
};

template< class S >
S dup( const S& s ) {
    return s;
}

template< class X >
std::vector<X> dup( const std::initializer_list<X>& l ) {
    return dupTo<std::vector>( l );
}

template< class S > using Dup = decltype( dup(declval<S>()) );

template< class S, class I, class R = Range<S,I>,
          class RS = typename R::sequence_type >
RS dup( const Range<S,I>& r ) {
    return dupExactly<RS>( r );
}

constexpr struct DupIf : Binary<DupIf> {
    using Binary<DupIf>::operator();

    template< class P, class S, class R = Dup<S> >
    R operator () ( P&& p, const S& s ) const {
        R r;
        std::copy_if( begin(s), end(s), tailInserter(r), forward<P>(p) );
        return r;
    }
} dupIf{};

template< class... > struct ReMapT;

template< template<class...> class S, class X > 
struct ReMapT< S<X> > {
    template< class Y > using remap = S<Y>;
};

template< class X, size_t N >
struct ReMapT< std::array<X,N> > {
    template< class Y > using remap = std::array<Y,N>;
};

template< class S, class Y >
using Remap = typename ReMapT< Dup<S> >::template remap<Y>;

template< class S, class D = Dup<S> >
D dup( S&& s, size_t n ) {
    return dupExactly<D>( forward<S>(s), n );
}

template< class S, class D = Dup<S> >
D dup( S&& s, size_t start, size_t end ) {
    return dupExactly<D>( forward<S>(s), end-start, end );
}

template< class S > 
Dup<S> tail( const S& s ) {
    return dup( tail_wrap(s) );
}

template< class S > 
Dup<S> init( const S& s ) {
    return dup( init_wrap(s) );
}

constexpr struct Reverse {
    template< class S >
    Dup<S> operator () ( S&& s ) const {
        return dup( reverse_wrap(forward<S>(s)) );
    }
} reverse{};

template< class F, class RI, class XS >
void _map( F&& f, RI&& ri, XS&& xs ) {
    std::transform( begin(forward<XS>(xs)), end(forward<XS>(xs)), 
                    forward<RI>(ri), forward<F>(f) );
}

template< class F, class RI, class XS, class YS, class ...ZS >
void _map( F&& f, RI&& ri, 
           XS&& xs, YS&& ys, ZS&& ...zs ) 
{
    for( auto x : forward<XS>(xs) )
        _map( closure(forward<F>(f),x), forward<RI>(ri), 
              forward<YS>(ys), forward<ZS>(zs)... );
}

template< class R, class F, class XS, class ...YS >
R mapExactly( F&& f, XS&& xs, YS&& ...ys ) {
    R r;
    _map( forward<F>(f), tailInserter(r), forward<XS>(xs), forward<YS>(ys)... );
    return r;
}

template< class R, class F, class S >
auto mapExactly( F&& f, S&& s ) -> XSame<R,Decay<S>,R> {
    R r;
    _map( forward<F>(f), tailInserter(r), forward<S>(s) );
    return r;
}

template< class R, class F, class S >
auto mapExactly( F&& f, S r ) -> ESame<R,S,S> {
    _map( forward<F>(f), begin(r), r );
    return r;
}

template< class F, class X, size_t N, size_t ...i,
          class R = Result<F,X> >
constexpr auto mapExactlyA( F&& f, const std::array<X,N>& a, tpl::IndexList<i...> )
    -> XSame< X, R, std::array<R,N> >
{
    return {{ forward<F>(f)( std::get<i>(a) )... }};
}

template< size_t I, class F, class X >
void _doMapA( const F&, std::array<X,I>& ) {
}

template< size_t I, class F, class X, size_t N >
auto _doMapA( F&& f, std::array<X,N>& a ) 
    -> typename std::enable_if<(I<N), void >::type
{
    static_assert( I < N, "Index larger than container!" );
    std::get<I>(a) = std::forward<F>(f)( std::get<I>(a) );
    _doMapA<I+1>( std::forward<F>(f), a );
}

template< class F, class X, size_t N, size_t ...i,
          class R = Result<F,X> >
auto mapExactlyA( F&& f, std::array<X,N> a, tpl::IndexList<i...> )
    -> ESame< X, R, std::array<X,N> >
{
    // We could break down here and do an std::transform on a, but this has
    // proven slightly more performant.
    _doMapA<0>( forward<F>(f), a );
    return a;
}

template< class R, class F, class X, size_t N >
constexpr auto mapExactly( F&& f, std::array<X,N>&& a ) 
    -> std::array< Result<F,X>, N >
{
    return mapExactlyA( forward<F>(f), a, typename tpl::IListBuilder<N-1>::type() );
}

/* mapTo<R> v = R( map(f,v) ) */
template< template<class...> class _R, class F, class ...S,
          class R = _R< Decay<decltype(declval<F>()(*begin(declval<S>())...))> > >
constexpr R mapTo( F&& f, S&& ...s ) {
    return mapExactly<R>( forward<F>(f), forward<S>(s)... );
}
template< template<class...> class R, class F, class ...S >
using ResultMapTo = decltype( mapTo<R>(declval<F>(),declval<S>()...) );

constexpr struct Map : Binary<Map> {
    using Binary<Map>::operator();

    /* map f {1,2,3} -> { f(1), f(2), f(3) } */
    template< class S, class X = SeqVal<S>,
              class F, class FX = Result<F,X>, class R = Remap<S,FX> >
    constexpr R operator () ( F&& f, S&& xs ) const
    {
        return mapExactly<R>( forward<F>(f), forward<S>(xs) );
    }

    template< class F, class ...X >
    auto operator () ( F&& f, const std::initializer_list<X>& ...l ) const
        -> ResultMapTo< std::vector, F, std::initializer_list<X>... >
    {
        return mapTo<std::vector>( forward<F>(f), l... );
    }

    /*
     * Variadic map.
     * In Haskell, one might write with Control.Applicative...
     *      (+) <$> [1,2] <*> [3,4]
     * but if we make map variadic to begin with, we can duplicate this
     * behaviour without Control.Applicative and just write
     *      map( (+), [1,2], [3,4] )
     *
     * map f xs ys = concatMap (\x -> map (f x) ys) xs
     * map is, however, more efficient because it does not construct a new
     * sequence for every x.
     */
    template< class F, class XS, class YS, class ...ZS,
              class R = Remap <
                  XS,
                  decltype (
                      declval<F>()( declval<SeqVal<XS>>(),
                                    declval<SeqVal<YS>>(),
                                    declval<SeqVal<ZS>>()... )
                  ) > >
    R operator () ( F&& f, XS&& xs, YS&& ys, ZS&& ...zs ) const {
        return mapExactly<R>( forward<F>(f),
                              forward<XS>(xs),
                              forward<YS>(ys), forward<ZS>(zs)... );
    }
} map{};

constexpr struct MapSquared : Binary<MapSquared> {
    using Binary<MapSquared>::operator();

    template< class F, class S >
    Dup<S> operator () ( F&& f, const S& s ) const {
        Dup<S> r;
        for( auto i = begin(s); i != end(s); i++ )
            for( auto j=i; j != end(s); j++ )
                r.emplace_back( forward<F>(f)( *i, *j ) );
        return r;
    }
} mapSquared{};

template< class F, class ...S >
using ResultMap = decltype( map(declval<F>(), declval<S>()...) );

template< class F, class S >
void vmap( F&& f, S&& s ) {
    std::for_each( begin(forward<S>(s)), end(forward<S>(s)), forward<F>(f) );
}

template< template<class...> class S, class X, class ...XS, class F,
          class R = S<X,XS...> >
auto map_it( F&& f, S<X,XS...> xs ) -> ESame< Result<F,X>, X, R >
{
    for( auto it = begin(xs); it != end(xs); it++ )
        *it = forward<F>(f)( it );
    return xs;
}

template< template<class...> class S, class X, class ...XS, class F,
          class FX = Result<F,X>, class R = S<FX,XS...> > 
auto map_it( F&& f, const S<X,XS...>& xs ) -> XSame< FX, X, R >
{
    R r;
    for( auto it = begin(xs); it != end(xs); it++ )
        _consRef( r, forward<F>(f)( it ) );
    return r;
}

template< class F, class X, class V = std::vector< Result<F,X> > >
V map_iter( F&& f, const std::initializer_list<X>& l ) {
    V v; 
    v.reserve( length(l) );
    _map( forward<F>(f), begin(v), l );
    return v;
}

constexpr struct Each : Binary<Each> {
    using Binary<Each>::operator();

    /* each f a b c... = all f [a,b,c...] */
    template< class F, class X >
    constexpr bool operator () ( F&& f, X&& x ) {
        return forward<F>(f)( forward<X>(x) );
    }

    template< class F, class X, class Y, class ...Z >
    constexpr bool operator () ( F&& f, X&& x, Y&& y, Z&& ...z ) {
        return (*this)( forward<F>(f), forward<X>(x) ) and
            (*this)( forward<F>(f), forward<Y>(y), forward<Z>(z)... );
    }
} each{};

/* accuml -- foldl implementaton */
template< class F, class X, class ...XS >
constexpr Decay<X> accuml( F&& f, X&& x, const XS& ...xs ) {
    return each( notNull, xs... ) ?
        accuml( forward<F>(f), 
                forward<F>(f)( forward<X>(x), 
                               head(xs)... ),
                tail_wrap(xs)... )
        : forward<X>(x);
}

// GCC does not apply proper tail recursion here,
// so optimize for the one-list and two-list cases.
// We use && only to let GCC know to prefer this version.
template< class F, class X, class S >
constexpr Decay<X> accuml( F&& f, X&& x, S&& s ) {
    return std::accumulate( begin(forward<S>(s)), end(forward<S>(s)), 
                            forward<X>(x), forward<F>(f) );
}

template< class F, class X, class XS, class YS >
X accuml( F&& f, X x, XS&& xs, YS&& ys ) {
    auto xi = begin( forward<XS>(xs) );
    auto yi = begin( forward<YS>(ys) );

    for( ; xi != end(xs) and yi != end(ys); xi++, yi++ )
        x = forward<F>(f)( std::move(x), *xi, *yi );
    return x;
}

/* foldl f x {1,2,3} -> f(f(f(x,1),2),3) */
template< class F, class X, class S, class ...YS >
constexpr Decay<X> foldl( F&& f, X&& x, S&& xs, YS&& ...ys ) {
    return accuml( forward<F>(f), forward<X>(x), 
                   forward<S>(xs), forward<YS>(ys)... );
}

template< class F, class X, class Y >
constexpr Decay<X> foldl( F&& f, X&& x, std::initializer_list<Y> s ) {
    return accuml( forward<F>(f), forward<X>(x), move(s) );
}

template< class F, class S, 
          class X = typename category::sequence_traits<S>::value_type >
constexpr X foldl( F&& f, S&& s ) {
    return list::foldl( forward<F>(f), 
                        head(forward<S>(s)), tail_wrap(forward<S>(s)) );
}

template< class F, class X >
constexpr X foldl( F&& f, std::initializer_list<X> s ) {
    return list::foldl( forward<F>(f), head(s), tail_wrap(move(s)) );
}

/* foldr f x {1,2,3} -> f(1,f(2,f(3,x))) */
template< class F, class X, class ...S >
constexpr Decay<X> foldr( F&& f, X&& x, S&& ...s ) {
    return list::foldl( flip(forward<F>(f)), forward<X>(x), 
                        reverse_wrap(forward<S>(s))... );
}

template< class F, class X, class Y >
constexpr Decay<X> foldr( F&& f, X&& x, std::initializer_list<Y> s ) {
    return list::foldl( flip(forward<F>(f)), 
                        forward<X>(x), reverse_wrap(move(s)) );
}

template< class F, class S, 
          class X = typename category::sequence_traits<S>::value_type >
constexpr X foldr( F&& f, S&& s ) {
    return list::foldr( forward<F>(f), 
                        last(forward<S>(s)), init_wrap(forward<S>(s)) );
}

template< class F, class Y >
constexpr Y foldr( F&& f, std::initializer_list<Y> s ) {
    return foldr( forward<F>(f), head(s), tail_wrap(move(s)) );
}

constexpr struct Append_ {
    template< typename A, typename B >
    A& operator () ( A& a, B&& b ) const {
        std::copy( begin(forward<B>(b)), end(forward<B>(b)), tailInserter(a) );
        return a;
    }
} append_{};

constexpr struct Append : Chainable<Append> {
    using Chainable<Append>::operator();

    /*
     * append({1},{2,3},{4}) -> {1,2,3,4}
     * Similar to [1]++[2,3]++[4]
     */
    template< typename A, typename B = A >
    A operator () ( A a, const B& b ) const {
        append_( a, b );
        return a;
    }
} append{};

constexpr struct Cons_ {
    template< class XS, class X >
    EBackInsert<XS,XS&> operator () ( XS& xs, X&& x ) const {
        xs.push_back( forward<X>(x) );
        return xs;
    }

    template< class XS, class X >
    XBackInsert<XS,XS&> operator () ( XS& xs, X&& x ) const {
        xs.insert( forward<X>(x) );
        return xs;
    }
} cons_{};

template< class XS, class X >
XS _cons( XS xs, X&& x ) {
    cons_( xs, forward<X>(x) );
    return xs;
}

template< class S, class X >
void _consRef( S& s, X&& x ) {
    s = _cons( move(s), forward<X>(x) );
}

template< class XS, class X, class Y, class ...Z >
XS _cons( XS xs, X&& x, Y&& y, Z&& ...z ) {
    return _cons (
        _cons( move(xs), forward<X>(x) ),
        forward<Y>(y), forward<Z>(z)...
    );
}

struct RCons : Chainable<RCons> {
    using Chainable<RCons>::operator();

    template< class XS, class X >
    XS operator () ( XS xs, X&& x ) const {
        xs.push_front( forward<X>(x) );
        return xs;
    }
};

constexpr struct Cons : Chainable<Cons> {
    using Chainable<Cons>::operator();

    template< class S, class X >
    S operator() ( S s, X&& x ) const {
        return _cons( move(s), forward<X>(x) );
    }
} cons{};


template< class F, class Acc, class S >
std::pair<Acc,S> mapAccumL( F&& f, Acc a, const S& s ) {
    if( null(s) )
        return { std::move(a), {} };

    using X = SeqVal<S>;
    using Y = typename decltype ( 
        declval<F>()( declval<Acc>(), declval<X>() )
    )::second_type;

    S r;
    Y y;
    for( const auto& x : s ) {
        std::tie(a,y) = std::forward<F>(f)( std::move(a), x );
        r.push_back( std::move(y) );
    }

    return { std::move(a), std::move(r) };
}

template< class F, class Acc, class S >
std::pair<Acc,S> mapAccumR( F&& f, Acc a, const S& s ) {
    if( null(s) )
        return { std::move(a), {} };

    using X = SeqVal<S>;
    using Y = typename decltype ( 
        declval<F>()( declval<Acc>(), declval<X>() )
    )::second_type;

    S r;
    Y y;
    for( const auto& x : reverse_wrap(s) ) {
        std::tie(a,y) = std::forward<F>(f)( std::move(a), x );
        r.push_back( std::move(y) );
    }

    return { std::move(a), std::move(r) };
}

template< class F, class X, class S,
          class V = std::vector<Decay<X>> >
V scanl( F&& f, X&& x, const S& s ) {
    V v{ forward<X>(x) };
    for( const auto& y : s )
        _consRef( v, forward<F>(f)( last(v), y ) );
    return v;
}

template< class F, class S >
using Scanl = decltype( scanl( declval<F>(), declval<SeqRef<S>>(), 
                               declval<S>() ) );

template< class F, class S >
Scanl<F,S> scanl( F&& f, S&& s ) {
    return scanl( forward<F>(f), 
                  head(forward<S>(s)), tail_wrap(forward<S>(s)) );
}

template< class F, class X, class S >
Scanl<F,S> scanr( F&& f, X&& x, const S& s ) {
    return reverse(scanl( forward<F>(f), forward<X>(x), reverse_wrap(s) ));
}

template< class F, class S >
using Scanr = decltype( scanr( declval<F>(), declval<SeqRef<S>>(), 
                               declval<S>() ) );

template< class F, class S >
Scanr<F,S> scanr( F&& f, S&& s ) {
    return scanr( forward<F>(f), 
                  last(forward<S>(s)), init_wrap(forward<S>(s)) );
}

template< class F, class X > struct Remember {
    using container  = std::vector<Decay<X>>;
    using reference  = typename container::reference;
    using const_reference = typename container::const_reference;
    using value_type = typename container::value_type;
    using citerator  = typename container::iterator;
    using difference_type = typename container::difference_type;

    // Unfortunately, as a requirement for declaring the Fnct::operator() as a
    // constexper convention, this must define its () as const. This feels
    // hackish, but the standard doesn't provide a way of saying a constexpr
    // function is non-const when its arguments aren't constexprs. We can
    // either remove the constexper convention for generality, or just define
    // these as mutable.
    mutable F f;
    mutable container c;

    struct iterator 
        : std::iterator<std::bidirectional_iterator_tag,value_type> 
    {
        const Remember& c;
        citerator it;

        iterator( const Remember<F,X>& _c )
            : c(_c), it(std::begin(c.c))
        {
        }
        iterator( const Remember<F,X>& _c, citerator i )
            : c(_c), it(i)
        {
        }

        iterator& operator= ( const iterator& other ) {
            it = other.it;
            return *this;
        };

        void _grow() {
            size_t dist = it - std::begin(c.c);
            c.c.emplace_back( c.f(c.c) );
            it = std::begin(c.c) + dist;
        }

        void grow() {
            if( it == std::end(c.c) ) 
                _grow();
        }


        iterator& operator++ () { 
            grow();
            it++;
            return *this;
        }

        iterator& operator-- () { 
            it--;
            return *this;
        }

        iterator operator++ (int) { 
            ++(*this);
            return iterator( c, prev(it) );
        }

        iterator operator-- (int) { 
            iterator copy = *this;
            --(*this);
            return copy;
        }

        const_reference operator* () {
            grow();
            return *it;
        }

        difference_type operator- ( const iterator& i ) {
            return it - i.it;
        }

        constexpr bool operator== ( const iterator& ) { return false; }
        constexpr bool operator!= ( const iterator& ) { return true;  }

        constexpr operator citerator () { return it; }
    };

    
    template< class ..._X >
    Remember( F f, _X&& ...x ) 
        : f(move(f)), c{forward<_X>(x)...} { }
    
    iterator begin() const { return *this; }
    iterator end()   const { return iterator(*this,std::end(c)); }
};

template< class F, class X >
constexpr size_t _length( const Remember<F,X>& ) {
    return std::numeric_limits<size_t>::max();
}

template< class F, class X, class I = Remember<F,X> >
std::vector<X> dup( const Remember<F,X>& c ) {
    return c.c;
}

template< class F, class X, class I = Remember<F,X> >
std::vector<X> dup( Remember<F,X> c, size_t n ) {
    return dupTo<std::vector>( move(c), n );
}

constexpr struct Memorize {
    template< class F, class X, class ...Y, class R = Remember<F,X> >
    constexpr R operator () ( F f, X x, Y&& ...y ) {
        return R( move(f), move(x), forward<Y>(y)... );
    }
} memorize{};

template< class F, class X >
using Iterate = Remember< Composition<F,Last>, X >;

struct ReturnIterate : Binary<ReturnIterate> {
    using Binary<ReturnIterate>::operator();

    template< class F, class X, class I = Iterate<F,X> >
    constexpr I operator () ( F f, X x ) {
        return memorize (
            compose( move(f), Last() ),
            move(x)
        );
    }
} iterate{};

constexpr auto repeat = iterate( Id() );

constexpr struct BiIterate {
    using L1 = decltype( last.with(1) );
    using L2 = Last;

    template< class F, class X,
              class I = Remember< BComposition<F,L1,L2>, X > >
    I operator () ( F f, X a, X b ) const {
        return I (
            bcompose( move(f), last.with(1), last ),
            move(a), move(b)
        );
    }
} biIterate{};

template< class I > struct XRange {
    using value_type = I;
    using difference_type = ItDist<I>;
    using pointer         = I;
    using const_pointer   = I;
    using reference       = I;
    using const_reference = I;

    struct iterator 
        : std::iterator< std::random_access_iterator_tag,
                         value_type, difference_type, pointer, reference >
    {
        value_type i;
        value_type stride;

        constexpr iterator( value_type i, value_type stride ) 
            : i(i), stride(stride) { }
        constexpr iterator( iterator it, value_type stride ) 
            : i(*it), stride(stride) { }

        constexpr const_reference operator* () const { return i; }
        reference operator* () { return i; }
        iterator operator++ () { ++i; return *this; }
        iterator operator-- () { --i; return *this; }
        iterator operator-- (int) { auto cpy = *this; --(*this); return cpy; }
        iterator operator++ (int) { auto cpy = *this; ++(*this); return cpy; }


        constexpr iterator operator+ ( difference_type n ) { 
            return iterator( i + n, stride );
        }
        constexpr iterator operator- ( difference_type n ) {
            return iterator( i - n, stride );
        } 
        constexpr difference_type operator- ( iterator other ) { 
            return (i - *other) / stride;
        }

        constexpr bool operator== ( iterator other ) { return i == *other; }
        constexpr bool operator!= ( iterator other ) { return i != *other; }

        iterator& operator+= ( difference_type other ) { 
            i += other * stride; 
            return *this;
        }

        iterator& operator-= ( difference_type other ) { 
            i -= other * stride;
            return *this;
        }
    };

    value_type stride;
    iterator b, e;

    constexpr XRange( value_type b, value_type e, value_type stride=1 ) 
        : stride(stride), b(b,stride), e(e,stride)  { }
    constexpr XRange( iterator b, iterator e, value_type stride=1 ) 
        : stride(stride), b(b,stride), e(e,stride)  { }

    constexpr iterator begin() { return b; }
    constexpr iterator end()   { return e; }
};

template< class I >
std::vector<I> dup( XRange<I> r ) {
    return dupTo<std::vector>( r );
}

template< class I, class R = XRange<I> >
constexpr R init( XRange<I> r ) {
    return R( r.b, prev(r.e) );
}

template< class I, class R = XRange<I> >
constexpr R tail( XRange<I> r ) {
    return R( next(r.b), r.e );
}

using IRange = XRange<unsigned int>;

/* 
 * enumerate [b,e] = XRange( [b,e+1) )
 * Creates an inclusive range from b to e.
 */
constexpr IRange enumerate( unsigned int b, unsigned int e, 
                            unsigned int stride = 1 ) {
    // Adding one to the en
    return IRange( b, e + stride, stride ); 
}

/* enumerate n = [n,n+1,n+2,...] */
constexpr IRange enumerate( unsigned int b ) {
    return IRange( b, std::numeric_limits<unsigned int>::max(), 1 ); 
}

constexpr IRange enumerateTo( unsigned int e ) {
    return enumerate( 0, e ); 
}

constexpr IRange enumerateN( unsigned int b, unsigned int n ) {
    return IRange( b, b+n );
}

template< class S >
constexpr IRange enumerate( const S& s ) {
    return IRange( 0, length(s) );
}

template< class F, class Y > struct PartMiddle {
    F f;
    Y y;

    template< class _F, class _Y >
    constexpr PartMiddle( _F&& f, _Y&& y ) 
        : f(forward<F>(f)), y(forward<Y>(y)) 
    {
    }

    template< class X, class Z >
    constexpr auto operator() ( X&& x, Z&& z ) 
        -> decltype( f(declval<X>(),y,declval<Z>()) )
    {
        return f( forward<X>(x), y, forward<Z>(z) );
    }
};

template< class F, class Y, class P = PartMiddle<F,Y> >
P middleClosure( F&& f, Y&& y ) {
    return P( forward<F>(f), forward<Y>(y) );
}

/* everyOther x [a,b,c...] = [x,a,x,b,x,c...] */
template< class X, class S, class R >
constexpr R _everyOther( const X& x, const S& s, R r ) {
    return foldl( middleClosure(Cons(),x), move(r), s );
}

template< class X, class S >
constexpr S everyOther( const X& x, const S& s ) {
    return foldl( middleClosure(Cons(),x), S(), s );
}

/* intersparse x [a,b,c] = [a,x,b,x,c] */
template< class X, class S >
constexpr S intersparse( const X& x, const S& s ) {
    return length(s) > 1 ?
        _everyOther( x, tail_wrap(s), S{head(s)} ) : s;
}

template< class S, class SS, class R >
constexpr R _everyOther_append( const S& s, const SS& ss, R r ) {
    return foldl( middleClosure(Append(),s), move(r), ss );
}

template< class S, class SS >
constexpr S everyOther_append( const S& s, const SS& ss ) {
    return _everyOther_append( s, ss, S() );
}

constexpr struct Intercalcate : Binary<Intercalcate> {
    using Binary<Intercalcate>::operator();

    /* intercalcate "--" {"ab","cd","ef"} */
    template< class S, class SS >
    S operator () ( const S& s, const SS& ss ) const {
        return not null(ss) ?
            _everyOther_append( s, tail_wrap(ss), S{head(ss)} )
            : S();
    }
} intercalcate{};

constexpr struct Concat {
    /* concat {{1},{2,3},{4}} = {1,2,3,4} */
    template< class SS, class S = typename SS::value_type >
    S operator () ( const SS& ss ) const {
        return foldl( append, S(), ss );
    }
} concat{};

constexpr struct Sort_ {
    template< class S >
    S& operator () ( S& s ) const {
        std::sort( begin(s), end(s) );
        return s;
    }
} sort_{};

constexpr struct Sort {
    template< class S >
    S operator () ( S s ) const {
        std::sort( begin(s), end(s) );
        return s;
    }
} sort{};

constexpr struct Ordered {
    template< typename Container >
    bool operator () ( const Container& c ) const {
        return std::is_sorted( begin(c), end(c) );
    }
} ordered{};

template< class I >
using ItTraits = std::iterator_traits<I>;

template< class I >
using ItCata = typename ItTraits<I>::iterator_category;

constexpr struct Filter : Binary<Filter> {
    using Binary<Filter>::operator();

    template< typename S, typename F >
    static Dup<S> filt( F&& f, S s, std::output_iterator_tag ) {
        s.erase ( 
            remove_if( begin(s), end(s), 
                       fnot( forward<F>(f) ) ),
            end( s )
        );
        return s;
    }

    template< typename S, typename F >
    static Dup<S> filt( F&& f, S s, std::input_iterator_tag ) {
        return dupIf( forward<F>(f), move(s) );
    }

    /* filter f C -> { x for x in C such that f(x) is true. } */
    template< typename S, typename F >
    Dup<S> operator () ( F&& f, S s ) const {
        using I = decltype( begin(s) );
        return filt( forward<F>(f), move(s), ItCata<I>() );
    }

    template< class X, class F, class V = std::vector<X> >
    V operator () ( F&& f, const std::initializer_list<X>& l ) const {
        return dupIf( forward<F>(f), l );
    }
} filter{};

constexpr struct Filtrate {
    // TODO: Perhaps this could be implemented with a filterFold...
    /* filtrate f pred xs = filter pred (map f xs) */
    template< class F, class P, class S,
              class R = decltype( map(declval<F>(),declval<S>()) ) >
    R operator () ( F&& f, P&& p, S&& s ) const {
        R r;
        for( auto& x : forward<S>(s) ) {
            auto y = forward<F>(f)( x );
            if( forward<P>(p)(y) )
                _consRef( r, move(y) );
        }
        return r;
    }
} filtrate{};

template< class I, class X = decltype(*declval<I>()) > 
struct MaybeElem {
    I pos, end;

    constexpr MaybeElem( I p, I e ) : pos(p), end(e) { }
    constexpr MaybeElem( std::nullptr_t ) : end(pos) { }

    constexpr operator bool () { return pos != end; }

    constexpr X operator * () { return *pos; }
};

constexpr struct Find : Binary<Find> {
    using Binary<Find>::operator();

    /* find pred xs -> Maybe x */
    template< class F, class S,
              class I   = typename category::sequence_traits<S>::iterator,
              class Val = typename category::sequence_traits<S>::value_type >
    MaybeElem<I> operator () ( F&& f, const S& s ) const {
        const auto& e = end(s);
        const auto it = find_if( begin(s), e, forward<F>(f) );
        return MaybeElem<I>( it, e );
    }
} find{};

constexpr struct FindFirst : Binary<FindFirst> {
    using Binary<FindFirst>::operator();

    template< class X, class S,
              class I = typename category::sequence_traits<S>::iterator >
    const MaybeElem<I> operator () ( const X& x, const S& s ) const {
        return MaybeElem<I>( std::find( begin(s), end(s), x ),
                             end(s) );
    }
} findFirst{};

constexpr struct CFind : Binary<CFind> {
    using Binary<CFind>::operator();

    /* cfind x C -> C::iterator */
    template< typename S, typename T >
    constexpr auto operator () ( T&& x, S&& s )
        -> decltype( begin(declval<S>()) )
    {
        return std::find( begin(forward<S>(s)), end(forward<S>(s)),
                          forward<T>(x) );
    }
} cfind{};

constexpr struct CFindIf : Binary<CFindIf> {
    using Binary<CFindIf>::operator();

    template< typename S, typename F >
    constexpr auto operator () ( F&& f, S&& s )
        -> decltype( begin(declval<S>()) )
    {
        return std::find_if( begin(forward<S>(s)), end(forward<S>(s)),
                             forward<F>(f) );
    }
} cfindIf{};

constexpr struct CFindNot : Binary<CFindNot> {
    using Binary<CFindNot>::operator();

    template< class S, class T, class F = std::equal_to<T> >
    constexpr auto operator () ( const T& x, S&& s, F&& f = F() )
        -> decltype( begin(declval<S>()) )
    {
        return cfindIf (
            fnot( closure(forward<F>(f),x) ),
            forward<S>(s)
        );
    }
} cfindNot{};

struct MaybeIndex {
    size_t i;
    static constexpr size_t NOT = std::numeric_limits<size_t>::max();

    constexpr MaybeIndex( size_t i ) : i(i) { }
    constexpr MaybeIndex() : i(NOT) { }
    constexpr MaybeIndex( std::nullptr_t ) : i(NOT) { }

    constexpr operator bool () { return i != NOT; }

    constexpr size_t operator * () { return i; }
};

MaybeIndex JustIndex( size_t x ) { return MaybeIndex( x ); }
MaybeIndex NothingIndex() { return MaybeIndex(); }

/* Construct MaybeIndex from an iterator. */
template< class I, class S >
MaybeIndex PossibleIndex( const I& i, const S& s ) {
    return i != end(s) ? JustIndex( distance(begin(s),i) ) : NothingIndex();
}


template< class F, class S >
MaybeIndex findIndex( F&& f, const S& s ) {
    return PossibleIndex( cfindIf(forward<F>(f),s), s );
}

template< class F, class S, class V = std::vector<size_t> >
V findIndecies( F&& f, const S& s ) {
    return PossibleIndex( cfindIf(forward<F>(f),s), s );
}

constexpr struct Take : Binary<Take> {
    using Binary<Take>::operator();

    template< class S, class D = Dup<S> >
    D operator () ( size_t n, S&& s ) const {
        return dup( forward<S>(s), n );
    }
} take{};

constexpr struct Replicate {
    template< class X >
    std::vector<Decay<X>> operator () ( size_t n, X&& x ) const {
        return take( n, repeat(forward<X>(x)) );
    }
} replicate{};

constexpr struct TakeWhile : Binary<TakeWhile> {
    using Binary<TakeWhile>::operator();

    template< class P, class S, class _S = Dup<S> >
    XSame<Decay<S>,_S,_S> operator () ( P&& p, S&& s ) const {
        auto it = cfindIf( fnot(forward<P>(p)), forward<S>(s) );

        // TODO: This is a hack and won't work on non-random iterators, but
        // required to work with Remember. I should implement Dup as a class
        // that can be specialized similarly to Functor and Monad.
        return take (
            it - begin(s),
            forward<S>(s)
        );
    }
    template< class P, class S, class _S = Dup<S> >
    ESame<S,_S,S> operator () ( P&& p, S s ) const {
        s.erase ( 
            cfindIf( fnot(forward<P>(p)), forward<S>(s) ),
            end(forward<S>(s)) 
        );
        return s;
    }
} takeWhile{};

constexpr struct Drop : Binary<Drop> {
    using Binary<Drop>::operator();

    template< class S >
    constexpr S operator () ( size_t n, const S& s ) {
        return S( next(begin(s),n), end(s) );
    }
} drop{};

constexpr struct DropWhile : Binary<DropWhile> {
    using Binary<DropWhile>::operator();

    template< class P, class S, class _S = Dup<S> >
    constexpr _S operator () ( P&& p, S&& s ) {
        return _S (
            cfindIf( fnot(forward<P>(p)), forward<S>(s) ),
            end( forward<S>(s) )
        );
    }
} dropWhile{};

constexpr struct DropWhileEnd : Binary<DropWhileEnd> {
    using Binary<DropWhileEnd>::operator();

    template< class Pred, class S >
    S operator () ( Pred p, S s ) const {
        s.erase( cfindIf(p,s), end(s) );
        return s;
    }
} dropWhileEnd{};

template< class X, class R > 
using XInt = typename std::enable_if< !std::is_integral<X>::value, R >::type;

constexpr struct SplitAt : Binary<SplitAt> {
    using Binary<SplitAt>::operator();

    template< class I, class S, class _S = Decay<S>,
              class P = std::pair<_S,_S> >
    constexpr XInt<I,P> operator () ( I it, S&& s )
    {
        return { { begin(forward<S>(s)), it },
                 {  it,  end(forward<S>(s)) } };
    }

    template< class S, class _S = Decay<S>,
              class P = std::pair<_S,_S> >
    constexpr P operator () ( size_t n, S&& s ) {
        return splitAt (
            next( begin(forward<S>(s)),
                  min(length(s), n) ), // Don't split passed the end!
            forward<S>(s)
        );
    }
} splitAt{};

template< class P, class S, class _S = Decay<S> >
std::pair<_S,_S> sbreak( P&& p, S&& s ) {
    return splitAt( cfindIf(forward<P>(p),forward<S>(s)), forward<S>(s) );
}

template< class P, class S, class _S = Decay<S>, class V = std::vector<_S> >
V splitBy( P&& p, S&& s ) {
    _S low, high;
    std::tie(low,high) = sbreak( forward<P>(p), forward<S>(s) );
    high = dropWhile( forward<P>(p), move(high) );

    V r{ move(low) };
    return null(high) ? r
        : append( move(r), 
                  splitBy( forward<P>(p), move(high) ) );
}

template< class P,
          class S, class _S = Decay<S>, 
          class R = std::pair<_S,_S> >
R span( P&& p, S&& s ) {
    R r;
    for( auto& x : s )
        _consRef( forward<P>(p)( x ) ? r.second:r.first,
                  x );
    return r;
}

template< class XS, class YS >
bool prefix( const XS& xs, const YS& ys ) {
    return std::equal( begin(xs), end(xs), begin(ys) );
}

template< class X, class S >
bool prefix( const std::initializer_list<X>& l, const S& s ) {
    return std::equal( begin(l), end(l), begin(s) );
}

template< class XS, class YS >
bool suffix( const XS& xs, const YS& ys ) {
    return prefix( reverse_wrap(xs), reverse_wrap(ys) );
}

template< class X, class S >
bool suffix( const std::initializer_list<X>& l, const S& s ) {
    return prefix( reverse_wrap(l), reverse_wrap(s) );
}

template< class XS, class YS >
bool infix( const XS& xs, const YS& ys ) {
    return std::includes( begin(ys), end(ys), begin(xs), end(xs) );
}

template< class X, class S >
bool infix( const std::initializer_list<X>& l, const S& s ) {
    return std::includes( begin(s), end(s), begin(l), end(l) );
}

template< class XS, class YS >
bool equal( const XS& xs, const YS& ys ) {
    return length(xs) == length(ys) and prefix( xs, ys );
}

constexpr struct Elem : Binary<Elem> {
    using Binary<Elem>::operator();

    template< class X, class S >
    bool operator () ( const X& x, const S& s ) const {
        return std::find( begin(s), end(s), x ) != end(s);
    }
} elem{};

constexpr auto notElem = fnot( elem );

template< class X, class S >
MaybeIndex elemIndex( const X& x, const S& s ) {
    return PossibleIndex( cfind(x,s), s );
}

template< class X, class S, class V = std::vector<size_t> >
V elemIndecies( const X& x, const S& s ) {
    V v;
    auto it = begin(s);
    while(true) {
        it = cfind( x, range<S>(it,end(s)) );
        if( it != end(s) )
            _consRef( v, distance(begin(s),it++) );
        else 
            break;
    }
    return v;
}

template< class X, class S >
S nubInsert( X&& x, S s ) {
    auto it = std::lower_bound( begin(s), end(s), forward<X>(x) );
    if( *it != x )
        s.insert( it, forward<X>(x) );
    return s;
}

constexpr struct Insert : Binary<Insert> {
    using Binary<Insert>::operator();

    /* insert 3 [1,2,4] = [1,2,3,4] */
    template< class X, class S >
    S operator () ( X&& x, S s ) const {
        s.insert(
            std::upper_bound( begin(s), end(s), forward<X>(x) ),
            forward<X>(x)
        );
        return s;
    }

    /* insert p y xs -- inserts y into the first place where (p x) == True. */
    template< class P, class X, class S >
    S operator () ( P&& p, X&& x, S s ) const {
        s.insert(
            std::upper_bound( begin(s), end(s), forward<X>(x), forward<P>(p) ),
            forward<X>(x)
        );
        return s;
    }
} insert{};

constexpr struct Erase : Binary<Erase> {
    using Binary<Erase>::operator();

    template< class X, class S >
    S operator () ( const X& x, S s ) const {
        auto it = cfind( x, s );
        if( it != end(s) )
            s.erase( it );
        return s;
    }

    template< class F, class X, class S >
    S operator () ( F&& f, const X& x, S s ) const {
        auto it = cfindIf( closure(forward<F>(f),x), s );
        if( it != end(s) )
            s.erase( it );
        return s;
    }
} erase{};

template< class P, class XS, class YS >
XS eraseFirst( P&& p, XS xs, YS&& ys ) {
    using F = XS(*)( const P&, SeqRef<YS>, XS );
    return list::foldl (
        // \s x -> erase p x s
        flip( erase(forward<P>(p)) ),
        move(xs), forward<YS>(ys) 
    ); 
}

template< class P, class XS, class Y >
XS eraseFirst( P&& p, XS xs, std::initializer_list<Y> l ) {
    using F = XS(*)( const P&, const Y&, XS );
    return list::foldl( flip( erase( forward<P>(p) ) ),
                        xs, move(l) );
}

template< class S, class X >
S consSet( S s, X&& x ) {
    return nubInsert( forward<X>(x), move(s) );
}

template< class DS, class S, class X >
S consDifference( const DS& ds, S s, X&& x ) {
    return notElem(x,ds) ?  cons( move(s), forward<X>(x) ) : s;
}

template< class DS, class S, class X >
S consIntersection( const DS& ds, S s, X&& x ) {
    return elem(x,ds) ?  cons( move(s), forward<X>(x) ) : s;
}

template< class S, class X >
S consWhen( bool b, S s, X&& x ) {
    return b ? cons( move(s), forward<X>(x) ) : s;
}

template< class S >
S nub( S s ) {
    s = sort( move(s) );
    auto e = std::unique( begin(s), end(s) );
    s.erase( e, end(s) );
    return s;
}

template< class XS, class YS, class R = Decay<XS> >
R difference( XS&& xs, const YS& ys ) {
    using F = R(*)( const YS&, XS, SeqRef<XS> );
    return list::foldl ( 
        closure( (F)consDifference, ys ),
        R(), forward<XS>(xs)
    );
}

constexpr struct Union : Chainable<Union> {
    using Chainable<Union>::operator();

    template< class XS, class YS >
    XS operator () ( XS xs, YS&& ys ) const {
        using F = XS(*)( XS, SeqRef<YS> );
        return foldl( (F)consSet, move(xs), forward<YS>(ys) );
    }
} sunion{};

template< class XS, class YS, class R = Decay<XS> >
R intersect( XS&& xs, const YS& ys ) {
    using F = R(*)( const YS&, XS, SeqRef<XS> );
    return list::foldl (
        closure( (F)consIntersection, ys ),
        R(), forward<XS>(xs)
    );
}

constexpr struct Sum {
    template< class S >
    constexpr SeqVal<S> operator() ( const S& s ) {
        return list::foldl( Add(), SeqVal<S>(0), s );
    }

    template< class I >
    constexpr I operator() ( XRange<I> r ) {
        // sum [m,n] = (n + 1 - m)(n + m)/2
        return (*r.e - *r.b) * (*r.e - 1 + *r.b) / 2;
    }
} sum{};

constexpr struct Product {
    template< class S >
    constexpr SeqVal<S> operator () ( const S& s ) {
        return list::foldl( Mult(), SeqVal<S>(1), s );
    }
} product{};

constexpr struct Maximum {
    template< class S >
    SeqRef<S> operator () ( S&& s ) const {
        return *std::max_element( begin(forward<S>(s)), end(forward<S>(s)) );
    }
} maximum{};

constexpr struct All : Binary<All> {
    using Binary<All>::operator();

    template< class F, class S >
    bool operator () ( F&& f, const S& s ) const {
        return std::all_of( begin(s), end(s), forward<F>(f) );
    }
} all{};

constexpr struct Any : Binary<Any> {
    using Binary<Any>::operator();

    template< class F, class S >
    bool operator () ( F&& f, const S& s ) const {
        return any_of( begin(s), end(s), forward<F>(f) );
    }
} any{};

constexpr struct None : Binary<None> {
    using Binary<None>::operator();

    template< class F, class S >
    bool operator () ( F&& f, const S& s ) const {
        return std::none_of( begin(s), end(s), forward<F>(f) );
    }
} none{};

template< class F, class XS, class YS >
XS intersectIf( F&& f, const XS& xs, const YS& ys ) {
    XS r;
    for( auto& x : xs )
        if( any( closure(forward<F>(f),x), ys ) )
            _consRef( r, x );
    return r;
}

template< class XS, class YS, class U = std::unique_ptr<YS> >
U stripPrefix( const XS& xs, const YS& ys ) {
    return not prefix( xs, ys ) ? nullptr :
        U( new YS( next(begin(ys),length(xs)), end(ys) ) );
}

template< class XS, class YS >
constexpr XS maybeConsRange( XS xs, YS&& ys ) {
    return not null(ys) ? cons( move(xs), forward<YS>(ys) )
        : xs;
}

template< class S, class P = std::equal_to<SeqRef<S>>,
          class _S = Decay<S>, class V = std::vector<_S> >
V group( S&& s, P&& p = P() ) {
    V v;

    auto it = begin( forward<S>(s) ), next = it;
    const auto e = end( forward<S>(s) );

    for( ; it != e; it = next) {
        auto adj = std::adjacent_find( it, e, forward<P>(p) );

        // First, cons all the non-adjacent members.
        for( const auto& x : range<S>(it,adj) )
            v.push_back(_S{x});

        // Then cons the adjacent members.
        next = cfindNot( *adj, range<S>(adj,e), forward<P>(p) );
        if( next != adj )
            v.emplace_back( adj, next ); 
    }

    return v;
}

constexpr struct Inits {
    template< class S, class V = std::vector<S> >
    static V _inits( const S& s ) {
        V v;
        for( auto it = begin(s); it != end(s); it++ )
            v.emplace_back( begin(s), it );
        return v;
    }

    template< class S, class V = std::vector<S> >
    V operator() ( const S& s ) const {
        return not null(s) ? _inits(s) : V();
    }
} inits{};

constexpr struct Tails {
    template< class S, class V = std::vector<S> >
    static V _tails( const S& s ) {
        V v{ S() };
        for( auto it = end(s); it != begin(s); )
            v.emplace_back( --it, end(s) );
        return v;
    }

    template< class S, class V = std::vector<S> >
    V operator() ( const S& s ) const {
        return not null(s) ? _tails(s) : V();
    }
} tails{};

constexpr struct Permutations {
    template< class S >
    static bool _next_p_ref( S& s ) {
        return std::next_permutation( begin(s), end(s) );
    }

    template< class S >
    static S _next_p( S s ) {
        _next_p_ref( s );
        return s;
    }

    template< class S >
    using SeqSeq = std::vector<Decay<S>>;

    template< class S, class V = SeqSeq<S> >
    static V sortedPermutations( S&& original ) {
        V r{ forward<S>(original) };
        Dup<S> next;
        while( next = last(r), _next_p_ref(next) )
            r.emplace_back( move(next) );
        return r;
    }

    template< class S, class V = SeqSeq<S> >
    static V _permutations( S&& original ) {
        V r{ forward<S>(original) };
        while( true ) {
            auto next = _next_p( last(r) );
            if( next != head(r) )
                r.emplace_back( move(next) );
            else
                return r;
        }
    }

    template< class S, class V = SeqSeq<S> >
    V operator() ( S&& original ) const {
        return ordered(original) ? sortedPermutations( forward<S>(original) )
            : _permutations( forward<S>(original) );
    }
} permutations{};

/* 
 * concatMap f s = concat (map f s)
 *      where f is a function: a -> [b]
 *            map f s produces a type: [[b]]
 * The operation "map then concat" may be inefficient compared to an inlined
 * loop, which would not require allocating a sequence of sequences to be
 * concatenated. This is an optimization that should be equivalent to the
 * inlined loop.
 */
constexpr struct ConcatMap : Binary<ConcatMap> {
    using Binary<ConcatMap>::operator();

    template< class F, class S,
              class R = Result<F,SeqRef<S>> >
    R operator () ( F&& f, const S& xs ) const {
        // A Haskeller would define concatMap based on foldl, however GCC 4.7
        // cannot inline f or append_ at that point.
        R r;
        for( const auto& x : xs ) {
            append_( r, forward<F>(f)( x ) );
        }
        return r;
    }
} concatMap{};

constexpr struct FoldMap : Binary<FoldMap> {
    using Binary<FoldMap>::operator();

    template< class F, class Fold, class X, class S >
    X operator () ( F&& f, Fold&& fold, X x, const S& s ) const {
        using R = Result<F,SeqRef<S>>;
        for( const auto& y : s )
            x = forward<Fold>(fold) (
                move(x),
                forward<F>(f)( y )
            );
        return x;
    }

    template< class F, class Fold, class X, class XS, class YS, class ...ZS >
    X operator () ( F&& f, Fold&& fold, X x,
                    const XS& xs, const YS& ys, const ZS& ...zs ) const
    {
        using R = decltype (
            declval<F>()( head(xs), head(ys), head(zs)... )
        );
        for( const auto& y : xs )
            x = (*this) (
                closure( forward<F>(f), y ), forward<Fold>(fold),
                move(x),
                ys, zs...
            );
        return x;
    }
} foldMap{};

/* zipWith f A B -> { f(a,b) for a in A and b in B } */
template< class F, class R, class ...S >
R _zipWith( F&& f, R r, const S& ...s ) {
    return each( notNull, s... ) ?
        _zipWith( forward<F>(f),
                   cons( move(r), forward<F>(f)( head(s)... ) ),
                   tail_wrap( s )... )
        : r;
}

constexpr struct ZipWith {
    template< class F, class XS, class ...YS, class R = Dup<XS> >
    R operator () ( F&& f, const XS& xs, const YS& ...ys ) const {
        return _zipWith( forward<F>(f), R(), xs, ys... );
    }


    template< typename S, typename F >
    S operator () ( F&& f, const S& a, S b ) const {
        std::transform( begin(a), end(a), begin(b),
                        begin(b), forward<F>(f) );
        return b;
    }
} zipWith{};

template< class S >
std::vector<Decay<S>> lines( S&& s ) {
    return splitBy( eq('\n'), forward<S>(s) );
}

template< class SS, class S = SeqVal<SS> >
S unlines( const SS& ss ) {
    // Remove the last '\n'.
    return init (
        // Intersperse with '\n'.
        concatMap( cons.with('\n'), ss )
    );
}

#include <cctype>
template< class S >
std::vector<Decay<S>> words( S&& s ) {
    return splitBy( isspace, forward<S>(s) );
}

template< class SS, class S = SeqVal<SS> >
S unwords( const SS& ss ) {
    // Remove the last ' '.
    return init (
        // Intersperse with ' '.
        concatMap( cons.with(' '), ss )
    );
}

namespace misc {

template< class X, class ...Y, class Ar = std::array<Decay<X>,1+sizeof...(Y)> >
constexpr Ar A( X&& x, Y&& ...y ) {
    return {{
        std::forward<X>(x),
        std::forward<Y>(y)... 
    }}; 
}

template< class X, class ...Y, class V_ = std::vector<Decay<X>> >
constexpr V_ V( X&& x, Y&& ...y ) {
    return {
        std::forward<X>(x),
        std::forward<Y>(y)... 
    }; 
}

template< class X, class ...Y, class L_ = std::list<Decay<X>> >
constexpr L_ L( X&& x, Y&& ...y ) {
    return {
        std::forward<X>(x),
        std::forward<Y>(y)... 
    }; 
}

template< class F, class S >
auto operator * ( F&& f, S&& s ) -> ResultMap<F,S> {
    return map( forward<F>(f), forward<S>(s) );
}

template< class F, class S >
S& operator *= ( F&& f, S& s ) {
    s = forward<F>(f) * move(s);
    return s;
}

template< class S, class F > 
Decay<S> operator / ( S s, F&& f ) {
    return filter( forward<F>(f), move(s) );
}

template< class F, class S >
S& operator /= ( F&& f, S& s ) {
    s = forward<F>(f) / move(s);
    return s;
}

template< class XS, class YS >
Decay<XS> operator | ( XS&& xs, YS&& ys ) {
    return append( forward<XS>(xs), forward<YS>(ys) );
}

template< class F, class S >
S& operator |= ( F&& f, S& s ) {
    s = forward<F>(f) | move(s);
    return s;
}

template< class S >
SeqRef<S> operator + ( S&& s ) {
    return head(forward<S>(s));
}

template< class S >
auto operator -- ( S&& s ) -> decltype( tail(declval<S>()) ) {
    return tail( forward<S>(s) );
}

template< class S >
SeqRef<S> operator - ( S&& s ) {
    return last(forward<S>(s));
}

template< class S >
auto operator ++ ( S&& s ) -> decltype( init(declval<S>()) ) {
    return init( forward<S>(s) );
}

} // namespace misc

namespace taking {

// A simple symbolic notation for takeWhile.

template< class S >
auto operator < ( S&& s, unsigned long long x ) 
    -> decltype( takeWhile(less.with(x),declval<S>()) )
{
    return takeWhile( less.with(x), forward<S>(s) );
}

template< class S >
auto operator > ( S&& s, unsigned long long x ) 
    -> decltype( takeWhile(greater.with(x),declval<S>()) )
{
    return takeWhile( greater.with(x), forward<S>(s) );
}
                      
template< class S >
auto operator >= ( S&& s, unsigned long long x ) 
    -> decltype( takeWhile(greaterEq.with(x),declval<S>()) )
{
    return takeWhile( greaterEq.with(x), forward<S>(s) );
}

template< class S >
auto operator <= ( S&& s, unsigned long long x ) 
    -> decltype( takeWhile(lessEq.with(x),declval<S>()) )
{
    return takeWhile( lessEq.with(x), forward<S>(s) );
}

template< class S, class F >
auto operator && ( S&& s, F&& f ) 
    -> decltype( takeWhile(declval<F>(),declval<S>()) ) 
{
    return takeWhile( forward<F>(f), forward<S>(s) );
}

} // namespace taking

} // namespace list.
} // namespace pure.

