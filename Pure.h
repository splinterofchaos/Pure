
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

namespace pure {

using namespace std;

/* 
 * Maybe x : Just x | Nothing 
 * A simple type to hold a possible value. Similar to a bool that holds nothing
 * when false and any value when true. By default, owns its value.
 */
template< class T, class Ptr=unique_ptr<T>, class Value=decltype(*Ptr()) > 
struct BasicMaybe
{
    typedef Ptr pointer;
    typedef Value value_type;

    pointer ptr; // A pointer fits the definition of 'possible value'.

  protected:
    constexpr BasicMaybe( pointer ptr ) : ptr( ptr ) { }
  public:

    struct Nothing { }; // Definitely holds nothing.
    struct Just { // Holds a definite value.
        T value;
        constexpr Just( T value ) : value( std::move(value) ) { }
    };

    BasicMaybe( typename BasicMaybe::Just j ) : ptr( new  T(move(j.value)) ) { }
    constexpr BasicMaybe( Nothing ) : ptr( nullptr ) { }

    BasicMaybe( const BasicMaybe& m )    = delete;
    constexpr BasicMaybe( BasicMaybe&& ) = default;

    constexpr explicit operator bool () { return ptr != nullptr; }

    constexpr value_type operator* () { return *ptr; }

};

template< class T >
struct Maybe : public BasicMaybe< T > 
{ 
    typedef BasicMaybe< T > M;
    using typename M::value_type;
    using typename M::pointer;
    using typename M::Just;
    using typename M::Nothing;

    constexpr Maybe( Maybe&& ) = default;
    constexpr Maybe( Just j ) : M( move(j) ) { }
    constexpr Maybe( Nothing ) : M( Nothing() ) { }
};

/* 
 * Maybe x* 
 * A C-style pointer doesn't define ownership properties, but we can assume any
 * non-null value valid for the scope of this object.
 */
template< class T > 
struct Maybe<T*> : public BasicMaybe< T*, T* >
{
    typedef BasicMaybe< T*, T*, T& > M;
    using typename M::value_type;
    using typename M::pointer;
    using typename M::Just;
    using typename M::Nothing;

    constexpr Maybe( Maybe&& ) = default;
    constexpr Maybe( Just j ) : M( j.value ) { }
    constexpr Maybe( Nothing ) : M( Nothing() ) { }
};

/* 
 * Just  x -> Maybe (Just x) | with a definite value.
 * Nothing -> Maybe Nothing  | with definitely no value.
 * Mightbe pred x -> Maybe x | possible with a value.
 */
template< class T, class M = Maybe<T> > constexpr M Just( T t ) {
    return M( typename M::Just(move(t)) ); 
}

template< class T, class M = Maybe<T> > constexpr M Nothing() {
    return M( typename M::Nothing() );
}

// A simple short hand for constructing a Maybe based on a predicate.
template< class T > Maybe<T> constexpr mightbe( bool p, T&& t ) {
    typedef Maybe<T> M;
    return p ? M( typename M::Just(forward<T>(t)) )
             : M( typename M::Nothing() );
}

/* maybe b (a->b) (Maybe a) -> b */
template< class R, class F, class T >
constexpr R maybe( R&& nothingVal, F f, const Maybe<T>& m )
{
    return m ? f( *m ) : forward<R>( nothingVal );
}

/* 
 * Just f  * Just x  = Just (f x)
 * _       * _       = Nothing
 * Just x  | _       = Just x
 * Nothing | Just x  = Just x
 * Nothing | Nothing = Nothing
 */
template< class F, class T, 
          class Ret = decltype( declval<F>()(declval<T>()) ) >
auto operator* ( const Maybe<F>& a, const Maybe<T>& b )
    -> Maybe< Ret >
{
    return a and b ? Maybe<Ret>( (*a)(*b) ) : Nothing<Ret>();
}

template< class T >
const Maybe<T>& operator| ( const Maybe<T>& a, const Maybe<T>& b )
{ return a ? a : b; }

template< class T >
Maybe<T> operator| ( Maybe<T>&& a, Maybe<T>&& b )
{ return a ? move(a) : move(b); }

/* Either a b : Left a | Right b */
template< class L, class R >
struct Either
{
    typedef L left_type;
    typedef R right_type;

    unique_ptr<left_type> left;
    unique_ptr<right_type> right;

    struct Left { 
        left_type value;
        constexpr Left( left_type value ) : value(move(value)) { }
    };

    struct Right { 
        right_type value;
        constexpr Right( right_type value ) : value(move(value)) { }
    };

    constexpr Either( Left x )  : left(  new left_type (move(x.value)) ) { }
    constexpr Either( Right x ) : right( new right_type(move(x.value)) ) { }
};

/* 
 * Left<b>  a -> Either a b
 * Right<a> b -> Either a b
 * Since an Either cannot be constructed without two type arguments, the
 * 'other' type must be explicitly declared when called. It is, for
 * convenience, the first type argument in both.
 */
template< class R, class L, class E = Either<L,R> >
constexpr E Left( L x ) { return E( typename E::Left(move(x)) ); }

template< class L, class R, class E = Either<L,R> >
constexpr E Right( R x ) { return E( typename E::Right(move(x)) ); }

/* either (a->c) (b->c) (Either a b) -> c */
template< class F, class G, class L, class R >
constexpr auto either( F&& f, G&& g, const Either<L,R>& e )
    -> decltype( f(*e.right) )
{
    return e.right ? f(*e.right) : g(*e.left);
}

/*
 * Right f * Right x = Right (f x)
 * _       * _       = Left _
 */
template< class L, class F, class T, 
          class Ret = decltype( declval<F>()(declval<T>()) ) >
constexpr Either<L,Ret> operator* ( const Either<L,F>& a, 
                                    const Either<L,T>& b )
{
    return a.right and b.right ? Right<L>( (*a.right)(*b.right) )
        : a.left ? Left<Ret>( *a.left ) : Left<Ret>( *b.left );
}

/* 
 * Partial application.
 * g(y) = f( x, y )
 * partial( f, x ) -> g(y)
 * partial( f, x, y ) -> g()
 */
template< class F, class ...Arg >
struct PartialApplication;

template< class F, class Arg >
struct PartialApplication< F, Arg >
{
    F f;
    Arg arg;

    template< class _F, class _Arg >
    constexpr PartialApplication( _F&& f, _Arg&& arg )
        : f(forward<_F>(f)), arg(forward<_Arg>(arg))
    {
    }

    /* 
     * The return type of F only gets deduced based on the number of arguments
     * supplied. PartialApplication otherwise has no idea whether f takes 1 or 10 args.
     */
    template< class ... Args >
    constexpr auto operator() ( Args&& ...args )
        -> decltype( f(arg,declval<Args>()...) )
    {
        return f( arg, forward<Args>(args)... );
    }
};

/* Recursive, variadic version. */
template< class F, class Arg1, class ...Args >
struct PartialApplication< F, Arg1, Args... > 
    : public PartialApplication< PartialApplication<F,Arg1>, Args... >
{
    template< class _F, class _Arg1, class ..._Args >
    constexpr PartialApplication( _F&& f, _Arg1&& arg1, _Args&& ...args )
        : PartialApplication< PartialApplication<F,Arg1>, Args... > (
            PartialApplication<F,Arg1>( forward<_F>(f), forward<_Arg1>(arg1) ),
            forward<_Args>(args)...
        )
    {
    }
};

template< class F, class ...A >
constexpr PartialApplication<F,A...> partial( F&& f, A&& ...a )
{
    return PartialApplication<F,A...>( forward<F>(f), forward<A>(a)... );
}

/* 
 * Composition. 
 * normal notation:  h = f( g() )
 * Haskell notation: h = f . g
 * compose(f,g) -> f . g
 * compose(f,g,h...) -> f . g . h ...
 */

template< class F, class ...G >
struct Composition;

template< class F, class G >
struct Composition<F,G>
{
    F f; G g;

    template< class _F, class _G >
    constexpr Composition( _F&& f, _G&& g ) 
        : f(forward<_F>(f)), g(forward<_G>(g)) 
    {
    }

    template< class ...Args >
    constexpr auto operator() ( Args&& ...args )
        -> decltype( f(g(declval<Args>()...)) )
    {
        return f( g(forward<Args>(args)...) );
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

template< class F, class ...G >
constexpr Composition<F,G...> compose( F&& f, G&& ...g )
{
    return Composition<F,G...>( forward<F>(f), forward<G>(g)... );
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

    constexpr Pure( X x ) : x( x ) { }
    constexpr Pure( Pure&& ) = default;

    template< class ...Args >
    constexpr X operator() ( Args ... ) { return x; }
};

template< class X >
constexpr Pure<X> pure( X&& x )
{
    return Pure<X>( forward<X>(x) );
}

/* 
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

/* fmap f g = compose( f, g ) */
template< class F, class G >
struct Functor<F,G> : public Composition<F,G>
{
    template< class _F, class _G >
    constexpr Functor( _F&& f, _G&& g )
        : Composition<F,G>( forward<_F>(f), forward<_G>(g) )
    {
    }
};

/* fmap f Pair(x,y) = Pair( f x, f y ) */
template< class F, class X, class Y >
struct Functor< F, std::pair<X,Y> > : public std::pair<X,Y>
{
    template< class _F, class P >
    constexpr Functor( _F&& f, P&& p )
        : std::pair<X,Y>( f( forward<P>(p).first  ), 
                          f( forward<P>(p).second ) )
    {
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
struct PartialLast; // Apply the last argument.

template< class F, class Last >
struct PartialLast< F, Last > : public PartialApplication< F, Last > 
{ 
    template< class _F, class _Last >
    constexpr PartialLast( _F&& f, _Last&& last )
        : PartialApplication<F,Last>( forward<_F>(f), forward<_Last>(last) )
    {
    }
};

// Remove one argument each recursion.
template< class F, class Arg1, class ...Args >
struct PartialLast< F, Arg1, Args... > : public PartialLast< F, Args... > 
{
    template< class _F, class _Arg1, class ..._Args >
    constexpr PartialLast( _F&& f, _Arg1&&, _Args&& ...args )
        : PartialLast<F,Args...>( forward<_F>(f), forward<_Args>(args)... )
    {
    }
};

template< class F, class ...Args >
struct PartialInitial; // Apply all but the last argument.

template< class F, class Arg1, class Arg2 >
struct PartialInitial< F, Arg1, Arg2 > : public PartialApplication< F, Arg1 >
{
    template< class _F, class _Arg1, class _Arg2 >
    constexpr PartialInitial( _F&& f, _Arg1&& arg1, _Arg2&& )
        : PartialApplication<F,Arg1>( forward<_F>(f), forward<_Arg1>(arg1) )
    {
    }
};

template< class F, class Arg1, class ...Args >
struct PartialInitial< F, Arg1, Args... > 
    : public PartialInitial< PartialApplication<F,Arg1>, Args... >
{
    template< class _F, class _Arg1, class ..._Args >
    PartialInitial( _F&& f, _Arg1&& arg1, _Args&& ...args )
        : PartialInitial<PartialApplication<F,Arg1>,Args...> (
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
            declval< PartialInitial<PartialLast<F,Args...>,Args...> >()() 
        )
    {
        /* We can't just (to my knowledge) pull the initial and final args
         * apart, so first reverse-apply the last argument, then apply each
         * argument forward until the last. The result is a zero-arity function
         * to invoke.
         */
        return PartialInitial< PartialLast<F,Args...>, Args... > (
            PartialLast< F, Args... >( f, forward<Args>(args)... ),
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
        -> decltype( f(declval<Args>()..., arg1) )
    {
        return f( forward<Args>(args)..., arg1 );
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
constexpr NRot<N,F> nrot( F&& f )
{
    return NRot<N,F>( forward<F>(f) );
}

template< unsigned int N, class F >
constexpr RNRot<N,F> rnrot( F&& f )
{
    return RNRot<N,F>( forward<F>(f) );
}

template< class F, class T >
struct Functor< F, Maybe<T> > 
    : Maybe< decltype( declval<F>()(declval<T>()) ) >
{
    typedef decltype( declval<F>()(declval<T>()) ) R;
    typedef Maybe< R > M;

    template< class _F >
    Functor( _F&& f, const M& m ) 
        : M( m ? Just<R>( f(*m) ) : Nothing<R>() ) 
    { 
    }
};

template< class F, class L, class R >
struct Functor< F, Either<L,R> > 
    : public Either< L, decltype( declval<F>()(declval<R>()) ) >
{
    typedef decltype( declval<F>()(declval<R>()) ) R2;
    typedef Either< L, R2 > E;
    template< class _F >
    constexpr Functor( _F&& f, const Either<L,R>& e )
        : Either<L,R2>( e.right ? 
                        Right<L>( f(*e.right) ) : Left<R2>( *e.left ) )
    {
    }
};

/* map f {1,2,3} -> { f(1), f(2), f(3) } */
template< typename Sequence, typename F >
Sequence map( F&& f, Sequence cont ) 
{
    transform( begin(cont), end(cont), begin(cont), 
               forward<F>(f) );
    return cont;
}

template< class C > struct IsSeqImpl {
    // Can only be supported on STL-like sequence types, not pointers.
    template< class _C > static true_type f(typename _C::iterator*);
    template< class _C > static false_type f(...);
    typedef decltype( f<C>(0) ) type;
};

template< class C > struct IsSeq : public IsSeqImpl<C>::type { };

/* Enable if is an STL-like sequence. */
template< class C, class R > struct ESeq : enable_if<IsSeq<C>::value,R> { };

/* Disable if is sequence. */
template< class C, class R > struct XSeq : enable_if<not IsSeq<C>::value,R> { };

/* fmap f {...} = map f {...} where {...} is any sequence. */
template< class F, class C >
constexpr auto fmap( F&& f, C&& c )
    -> typename ESeq <
        C, decltype( map(forward<F>(f),forward<C>(c)) ) 
    >::type
{
    return map( forward<F>(f), forward<C>(c) );
}

template< class F, class G >
constexpr auto fmap( F&& f, G&& g )
    // Disallow on sequences.
    -> typename XSeq< G, Functor<F,G> >::type
{
    return Functor<F,G>( forward<F>(f), forward<G>(g) );
}

/* squash[ f(x,x) ] = g(x) */
template< class F >
struct Squash
{
    F f;

    template< class _F >
    constexpr Squash( _F&& f ) : f( forward<_F>(f) ) { }

    template< class Arg1, class ...Args >
    constexpr auto operator() ( Arg1&& arg1, Args&& ...args )
        -> decltype( f(declval<Arg1>(),declval<Arg1>(),declval<Args>()...) )
    {
        return f( forward<Arg1>(arg1), forward<Arg1>(arg1), 
                  forward<Args>(args)... );
    }
};

template< class F >
constexpr Squash<F> squash( F&& f )
{
    return Squash<F>( forward<F>(f) );
}

/* 
 * f( x, y ) = f( l(z), r(z) ) = g(z)
 *  where x = l(z) and y = r(z)
 * join( f, l, r ) = g
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

    template< class A, class ...AS >
    constexpr auto operator() ( A&& a, AS&& ...as )
        -> decltype( f(l(declval<A>()),r(declval<A>()),declval<AS>()...) )
    {
        return f( l(forward<A>(a)), r(forward<A>(a)), forward<AS>(as)... );
    }
};

template< class F, class L, class R >
constexpr Join<F,L,R> join( F&& f, L&& l, R&& r )
{
    return Join<F,L,R>( forward<F>(f), forward<L>(l), forward<R>(r) );
}

/* 
 * Concatenation.
 * concat({1},{2,3},{4}) -> {1,2,3,4}
 */
template< typename C1, typename C2 >
C1 concat( C1 a, const C2& b )
{
    a.reserve( a.size() + b.size() );
    copy( begin(b), end(b), back_inserter(a) );
    return a;
}

template< typename Cx1, typename Cx2, typename ... Cxs >
Cx1 concat( Cx1&& a, const Cx2& b, const Cxs& ... c )
{
    return concat( concat(forward<Cx1>(a), b), c... );
}

template< typename Container >
constexpr bool ordered( const Container& c )
{
    return c.size() <= 1
        or mismatch ( 
            begin(c), end(c)-1, 
            begin(c)+1, 
            less_equal<int>() 
        ).second == end(c);
}

/* filter f C -> { x for x in C such that f(x) is true. } */
template< typename Container, typename F >
Container filter( const F& f, Container cont )
{
    typedef typename remove_reference<Container>::type::value_type T;
    cont.erase ( 
        remove_if ( 
            begin(cont), end(cont), 
            [&](const T& t){ return not f(t); } 
        ),
        end( cont )
    );
    return cont;
}

/* find pred xs -> Maybe x */
template< class F, class Sequence,
          class Val = decltype(&declval<Sequence>().front()) >
Maybe<Val> find( F&& f, Sequence&& s )
{
    const auto e = end( s ), b = begin( s );
    const auto it = find_if( b, e, forward<F>(f) );

    typedef Maybe< Val > M;
    return mightbe( it != e, &(*it) ); 
}

/* cfind x C -> C::iterator */
template< typename Container, typename T >
constexpr auto cfind( const T& value, Container&& cont )
    -> decltype( begin(cont) )
{
    return find( begin(cont), end(cont), value );
}

template< typename Container, typename F >
constexpr auto cfind_if( const F& f, Container&& cont )
    -> decltype( begin(cont) )
{
    return find_if( begin(cont), end(cont), f );
}

/* all f C -> true when f(x) is true for all x in C; otherwise false. */
template< typename Container, typename F >
constexpr bool all( const F& f, const Container& cont )
{
    return all_of( begin(cont), end(cont), f );
}

/* foldl f x {1,2,3} -> f(f(f(x,1),2),3) */
template< typename Container, typename Value, typename F >
constexpr Value foldl( const F& f, const Value& val, const Container& cont )
{
    return accumulate( begin(cont), end(cont), val, f );
}

template< typename Value, typename F, typename Container >
constexpr Value foldl( F&& f, const Container& cont )
{
    return accumulate( next(begin(cont)), end(cont), 
                            cont.front(), f );
}

/* foldr f x {1,2,3} -> f(1,f(2,f(3,x))) */
template< typename F, typename Value, typename Container >
constexpr Value foldr( F&& f, Value&& val, const Container& cont )
{
    return accumulate ( cont.rbegin(), cont.rend(), 
                             forward<Value>(val), forward<F>(f) );
}

template< typename Value, typename F, typename Container >
constexpr Value foldr( F&& f, const Container& cont )
{
    return accumulate ( next(cont.rbegin()), cont.rend(), 
                             cont.back(), forward<F>(f) );
}

template< typename Container, typename F >
void for_each( const F& f, const Container& cont )
{
    for_each( begin(cont), end(cont), f );
}

template< class F, class I, class J >
void for_ij( const F& f, I i, const I& imax, J j, const J& jmax )
{
    for( ; j != jmax; j++ )
        for( ; i != imax; i++ )
            f( i, j );
}

template< class F, class I, class J >
void for_ij( const F& f, const I& imax, const J& jmax )
{
    for( J j = J(); j != jmax; j++ )
        for( I i = I(); i != imax; i++ )
            f( i, j );
}

/* zip_with f A B -> { f(a,b) for a in A and b in B } */
template< typename Container, typename F >
Container zip_with( F&& f, const Container& a, Container b )
{
    transform( begin(a), end(a), begin(b), 
                    begin(b), forward<F>(f) );
    return b;
}

/*
 * cleave x f g h -> { f(x), g(x), h(x) }
 * Inspired by Factor, the stack-based language.
 */
template< typename X, typename F, typename ... Fs >
constexpr array<X,sizeof...(Fs)+1> cleave( X x, const F& f, const Fs& ... fs ) 
{
    return {{ f(x), fs(x)... }};
}

/* cleave_with f x y z =  { f(x), f(y), f(z) } */
template< class F, class A, class ...B >
constexpr auto cleave_with( F&& f, A&& a, B&& ...b )
    -> array< decltype( declval<F>()(declval<A>()) ), sizeof...(B)+1 > 
{
    return {{ f(forward<A>(a)), f(forward<B>(b))... }};
}

template< class T, unsigned int N, class F >
array<T,N> generate( F f )
{
    array<T,N> cont;
    generate( begin(cont), end(cont), f );
    return cont;
}

template< class T, class F >
vector<T> generate( F f, unsigned int n )
{
    vector<T> c; c.reserve(n);
    while( n-- )
        c.push_back( f() );
    return c;
}

template< class Cmp, class Container >
constexpr auto max( const Cmp& cmp, Container&& cont )
    -> decltype( begin(cont) )
{
    return max_element( begin(cont), end(cont), cmp );
}

template< class Container >
constexpr auto max( Container&& cont )
    -> decltype( begin(cont) )
{
    return max_element( begin(cont), end(cont) );
}

template< class Cmp, class Container >
constexpr auto min( const Cmp& cmp, Container&& cont )
    -> decltype( begin(cont) )
{
    return min_element( begin(cont), end(cont), cmp );
}

template< class Container >
constexpr auto min( Container&& cont )
    -> decltype( begin(cont) )
{
    return min_element( begin(cont), end(cont) );
}

} // namespace pure

