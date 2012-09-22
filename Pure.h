
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

namespace pure {

using namespace std;

template< class X > using Decay = typename decay<X>::type;

template< class ...X > 
using CommonType = Decay<typename std::common_type<X...>::type>;

namespace cata {

/* 
 * CATEGORIES:
 * In order to easily implement algorithms for different categories of types,
 * we define categories and a type, Cat<T>, which defines Cat<T>::type as
 * either a category or T. This way, we can implement algorithms in terms of
 * either tags (categories) or specific types.
 *
 * This is similar to the tag dispatch pattern used in the STL (see:
 * random_access_iterator_tag, forward_iterator_tag, etc...) except that the
 * type is not erased.
 *
 * The following traits classes merely act as a shortcut to using decltype.
 */

struct other {};

/*
 * []
 * Any type that:
 *      Has a defined begin(s) and end(s).
 */
struct sequence {};

template< class Seq > struct sequence_traits {
    using sequence   = Seq;
    using iterator   = Decay<decltype( begin(declval<Seq>()) )>;
    using reference  = decltype( *declval<iterator>() );
    using value_type = typename remove_reference<reference>::type;
};

/*
 * Maybe 
 * Any type that: 
 *      Can be dereferenced like so: *Maybe(). (fromJust).
 *      Can be converted to a boolean. (isJust/isNothing)
 *      But is not a function
 */
struct maybe {};

// If we want to return a pointer that defines ownership, we want a smart
// pointer. Consider any non-raw pointer good 'nuff.
template< class Ptr > struct SmartPtr { using type = Ptr; };
template< class X > struct SmartPtr<X*> { using type = unique_ptr<X>; };

template< class Ptr > struct maybe_traits {
    using pointer    = Ptr;
    using smart_ptr  = typename SmartPtr<pointer>::type;
    using reference  = decltype( *declval<pointer>() );
    using value_type = Decay<reference>;
};

/*
 * cat performs the actual type deduction via its argument. If no other
 * definition of cat is valid, it returns a variable of the same type as its
 * argument. It is only for use in decltype expressions.
 */
template< class X >
X cat( ... );

template< class S >
auto cat( const S& s ) -> decltype( begin(s), end(s), sequence() );

template< class M >
auto cat( const M& m ) -> decltype( *m, (bool)m, maybe() );

template< class T > struct Cat {
    using type = decltype( cat<Decay<T>>(declval<T>()) );
};

// Prevent function pointers from being deduced as maybe types.
template< class R, class ...X > struct Cat< R(&)(X...) > {
    typedef R(&type)(X...);
};

} // namespace cata

template< class X >
using Cat = typename cata::Cat<X>::type;

template< class ... > struct Category;

/* The result of a function applied against X. */
template< class F, class ...X >
using Result = Decay<decltype( declval<F>()( declval<X>()... ) )>;

template< class X, class C = Category<X> >
auto cid( X&& x ) -> decltype( C::id( declval<X>() ) ) {
    return C::id( forward<X>(x) );
}

struct Id {
    template< class X >
    constexpr X operator() ( X&& x ) { return forward<X>(x); }
};

struct CId {
    template< class X >
    constexpr X operator() ( X&& x ) { return cid( forward<X>(x) ); }
};

/* 
 * Let comp be a generalization of compose. 
 */
template< class F, class ...G, class C = Category<F> > 
constexpr decltype( Category<F>::comp( declval<F>(), declval<G>()... ) )
comp( F&& f, G&& ...g ) {
    return C::comp( forward<F>(f), forward<G>(g)... );
}

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
using Nth = decltype( get<N>( declval<P>() ) );

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
    using Result = decltype( make_pair(declval<Nth<F,0,P>>(),
                                       declval<Nth<G,1,P>>()) );

    template< class P/*air*/ >
    constexpr Result<P> operator() ( const P& p ) {
        return make_pair( f(get<0>(p)), g(get<1>(p)) );
    }
};

template< class F, class G, class P = PairCompose<F,G> > 
constexpr P pair_compose( F f, G g ) {
    return P( move(f), move(g) );
}
/* Default category: function. */
template< class F > struct Category<F> {
    static constexpr Id id = Id();

    template< class _F, class ..._G > static
    constexpr decltype( compose(declval<_F>(),declval<_G>()...) )
    comp( _F&& f, _G&& ...g ) {
        return compose( forward<_F>(f), forward<_G>(g)... );
    }
};

template< class ... > struct Arrow;

/* Haskell defines <<< as an alias for (.). */

/*
 * fcomp (forward compose): the reverse of comp.
 * Haskell's >>>.
 */
template< class F, class G >
constexpr auto fcomp( F&& f, G&& g ) 
    -> decltype( comp(declval<G>(), declval<F>()) ) 
{
    return comp( forward<G>(g), forward<F>(f) ); 
}

template< class F, class G, class H, class ...I>
constexpr decltype( comp( fcomp(declval<G>(),declval<H>(),declval<I>()...), 
                          declval<F>() ) )
fcomp( F&& f, G&& g, H&& h, I&& ...i ) {
    return comp( fcomp( forward<G>(g), forward<H>(h), forward<I>(i)... ),
                 forward<F>(f) );
}

template< class A, class F, class Arr = Arrow<A> >
decltype( Arr::arr( declval<F>() ) )
arr( F&& f ) { return Arr::arr( forward<F>(f) ); }

/* (f *** g) (x,y) = (f x, g y) */
template< class F, class G, class A = Arrow<F> >
decltype( A::split(declval<F>(), declval<G>()) )
split( F&& f, G&& g ) {
    return A::split( forward<F>(f), forward<G>(g) );
}

/* (f &&& g) x = (f x, g x) */
template< class F, class G, class A = Arrow<F> >
decltype( A::fan(declval<F>(),declval<G>()) )
fan( F&& f, G&& g ) {
    return A::fan( forward<F>(f), forward<G>(g) );
}

/* (first f) (x,y) = (f x, y) */
template< class F, class A = Arrow<F> >
decltype( A::first(declval<F>()) )
first( F&& f ) {
    return A::first( forward<F>(f) );
}

/* (second f) (x,y) = (x, f y) */
template< class F, class A = Arrow<F> >
decltype( A::second(declval<F>()) )
second( F&& f ) {
    return A::second( forward<F>(f) );
}

template< class Func > struct Arrow<Func> {
    static constexpr Id arr = Id();

    // Same decltype expression used many times. Save typing.
    template< class F, class G >
    using Split = decltype( pair_compose(declval<F>(), declval<G>()) );

    /*
     * Note: It would seem that an easier way to define this class might be to
     * have it define only split, and define first, second, and fan in terms
     * of that. This goes against Haskell's version, but I don't see a reason,
     * currently, why it would not be just as generic and more convenient.
     */

    template< class F, class G > static 
    constexpr Split<F,G> split ( F&& f, G&& g ) 
    {
        return pair_compose( forward<F>(f), forward<G>(g) );
    }

    template< class F > static 
    constexpr Split<F,Id> first( F&& f ) 
    { 
        return pair_compose( forward<F>(f), Id() );
    }

    template< class F > static 
    constexpr Split<Id,F> second( F&& f ) 
    {
        return pair_compose( Id(), forward<F>(f) );
    }

    struct Splitter {
        template< class X, class P = pair<X,X> > 
            constexpr P operator() ( const X& x ) {
                return P( x, x );
            }
    };

    template< class F, class G > static 
    constexpr auto fan( F&& f, G&& g ) 
        -> decltype( compose(declval< Split<F,G> >(), Splitter()) )
    {
        return compose( pair_compose( forward<F>(f), forward<G>(g) ), 
                        Splitter() );
    }
};

template< class X > constexpr X inc( X x ) { return ++x; }
template< class X > constexpr X dec( X x ) { return --x; }

struct Add {
    template< class X, class Y >
    constexpr CommonType<X,Y> operator() ( X&& x, Y&& y ) {
        return forward<X>(x) + forward<Y>(y);
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

template< class S >
constexpr auto length( S&& s ) -> decltype( declval<S>().size() )
{
    return forward<S>(s).size();
}

template< class S >
constexpr size_t length( const S& s ) {
    return distance( begin(s), end(s) );
} 

template< class X, unsigned N >
constexpr size_t length( X (&)[N] ) {
    return N;
}

template< class S >
constexpr bool null( const S& s ) {
    return begin(s) == end(s);
}

template< class S >
constexpr bool not_null( const S& s ) {
    return begin(s) != end(s);
}

template< class S >
using SeqRef = typename cata::sequence_traits<S>::reference;
template< class S >
using SeqVal = typename cata::sequence_traits<S>::value_type;
template< class S >
using SeqIter = typename cata::sequence_traits<S>::iterator;

template< class S >
constexpr SeqRef<S> head( S&& s, size_t n = 0 ) {
    return *next( begin(forward<S>(s)), n );
}

template< class X > 
constexpr X head( const initializer_list<X>& l, size_t n = 0 ) {
    return *next( begin(l), n );
}

template< class S >
constexpr SeqRef<S> last( S&& s, size_t n = 0 ) {
    return *prev( end(forward<S>(s)), n+1 );
}

template< class X >
constexpr X last( const initializer_list<X>& l, size_t n = 0 ) {
    return *prev( end(l), n+1 );
}

struct Last {
    size_t N;

    constexpr Last( size_t N=0 ) : N(N) { }

    template< class X >
    constexpr auto operator() ( X&& x ) 
        -> decltype( last(declval<X>()) )
    {
        return last( forward<X>(x), N );
    }
};

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
    return R( prev( end(forward<S>(s)), n ),
              begin( forward<S>(s) ) );
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
          class R = Range<S,reverse_iterator<I>> >
constexpr R reverse_wrap( S&& s ) {
    return R( end(forward<S>(s)), begin(forward<S>(s)) );
}

template< class R, class S >
R _dup( S&& s ) {
    R r;
    copy( begin(forward<S>(s)), end(forward<S>(s)),
          back_inserter(r) );
    return r;
}

template< class R, class S >
R _dup( S&& s, size_t n, size_t off = 0 ) {
    R r;
    copy_n( next( begin(s), off ), 
            std::min( n+1, length(s) ) - 1,
            back_inserter(r) );
    return r;
}

template< class S >
S dup( const S& s ) {
    return _dup<S>( s );
}

template< class S > using Dup = decltype( dup(declval<S>()) );

template< class S, class I, class R = Range<S,I>, 
          class RS = typename R::sequence_type >
Dup<RS> dup( const Range<S,I>& r ) {
    return _dup<Dup<RS>>( r );
}

template< class X, class V = vector<X> >
V dup( const initializer_list<X>& l ) {
    return _dup<V>( l );
}

template< class S, class D = Dup<S> >
D dup( S&& s, size_t n ) {
    return _dup<D>( forward<S>(s), n );
}

template< class S, class D = Dup<S> >
D dup( S&& s, size_t start, size_t end ) {
    return _dup<D>( forward<S>(s), end-start, end );
}

template< class S > 
Dup<S> tail( const S& s ) {
    return dup( tail_wrap(s) );
}

template< class S > 
Dup<S> init( const S& s ) {
    return dup( init_wrap(s) );
}

template< class S >
Dup<S> reverse( S&& s ) {
    return dup( reverse_wrap(forward<S>(s)) );
}

template< class F, class RI, class XS >
void _map( F&& f, RI&& ri, const XS& xs ) {
    transform( begin(xs), end(xs), 
               forward<RI>(ri), forward<F>(f) );
}

template< class F, class RI, class XS, class YS, class ...ZS >
void _map( F&& f, RI&& ri, 
               const XS& xs, const YS& ys, const ZS& ...zs ) 
{
    for( const auto& x : xs )
        _map( closure(forward<F>(f),x), forward<RI>(ri), 
                  ys, zs... );
}

template< class A, class B, class R >
using ESame = typename std::enable_if< is_same<A,B>::value, R >::type;
template< class A, class B, class R >
using XSame = typename std::enable_if< !is_same<A,B>::value, R >::type;

/* map f {1,2,3} -> { f(1), f(2), f(3) } */
template< template<class...> class S, class X, class ..._X, 
          class XS = S<X,_X...>,
          class F, class FX = Result<F,X>, class R = Decay<S<FX,_X...>> > 
auto map( F&& f, S<X,_X...> xs ) -> XSame< FX, X, R >
{
    R r;
    _map( forward<F>(f), back_inserter(r), xs );
    return r;
}

// When mapping from X to X, we can optimize by not returning a new sequence.
template< template<class...> class S, class X, class ...XS, class F,
          class R = S<X,XS...> >
auto map( F&& f, S<X,XS...> xs ) -> ESame< X, Result<F,X>, R >
{
    _map( forward<F>(f), begin(xs), xs );
    return xs;
}

template< class F, class X, class V = vector< Result<F,X> > >
V map( F&& f, const initializer_list<X>& l ) {
    V v; 
    v.reserve( length(l) );
    _map( forward<F>(f), begin(v), l );
    return v;
}

/* map_to<R> v = R( map(f,v) ) */
template< class R, class F, class S >
R map_to( F&& f, S&& s ) {
    R r;
    _map( forward<F>(f), back_inserter(r), forward<S>(s) );
    return r;
}

/*
 * Variadic map.
 * In Haskell, one might write with Control.Applicative...
 *      (+) <$> [1,2] <*> [3,4]
 * but if we make map variadic to begin with, we can duplicate this behaviour
 * without Control.Applicative and just write
 *      map( (+), [1,2], [3,4] )
 *
 * map f xs ys = concatMap (\x -> map (f x) ys) xs
 * map is, however, more efficient because it does not construct a new sequence
 * for every x.
 */
template< template<class...> class S, class ..._S,
          class X, class Y, class ...Z,
          class F, class FXYZ = Result<F,X,Y,Z...>,
          class R = S<FXYZ,_S...> >
R map( F&& f, const S<X,_S...>& xs, 
       const S<Y,_S...>& ys, const S<Z,_S...>& ...zs )
{
   R r;
   _map( forward<F>(f), back_inserter(r), xs, ys, zs... );
   return r;
}

// TODO: requires further specialization.
template< class X, size_t N, class F, class A = array<Result<F,X>,N> >
A map( F&& f, const array< X, N >& xs ) {
    A r;
    _map( forward<F>(f), begin(r), xs );
    return r;
}

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
        r.push_back( forward<F>(f)( it ) );
    return r;
}

template< class F, class X, class V = vector< Result<F,X> > >
V map_iter( F&& f, const initializer_list<X>& l ) {
    V v; 
    v.reserve( length(l) );
    _map( forward<F>(f), begin(v), l );
    return v;
}


/* each f a b c... = all f [a,b,c...] */
template< class F, class X >
bool each( F&& f, X&& x ) {
    return forward<F>(f)( forward<X>(x) );
}

template< class F, class X, class Y, class ...Z >
bool each( F&& f, X&& x, Y&& y, Z&& ...z ) {
    return each( forward<F>(f), forward<X>(x) ) and 
        each( forward<F>(f), forward<Y>(y), forward<Z>(z)... );
}

struct NotNull {
    template< class X >
    bool operator() ( X&& x ) {
        return not_null( forward<X>(x) );
    }
};

/* accuml -- foldl implementaton */
template< class F, class X, class ...XS >
constexpr Decay<X> accuml( F&& f, X&& x, 
                           const XS& ...xs ) {
    return each( NotNull(), xs... ) ?
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
    return std::accumulate( begin(s), end(s), 
                            forward<X>(x), forward<F>(f) );
}

template< class F, class X, class XS, class YS >
X accuml( F&& f, X x, XS&& xs, YS&& ys ) {
    auto xi = begin( forward<XS>(xs) );
    auto yi = begin( forward<YS>(ys) );

    for( ; xi != end(xs) and yi != end(ys); xi++, yi++ )
        x = forward<F>(f)( x, *xi, *yi );
    return x;
}

/* foldl f x {1,2,3} -> f(f(f(x,1),2),3) */
template< class F, class X, class ...S >
constexpr Decay<X> foldl( F&& f, X&& x, S&& ...s ) {
    return accuml( forward<F>(f), forward<X>(x), forward<S>(s)... );
}

template< class F, class X, class Y >
constexpr Decay<X> foldl( F&& f, X&& x, initializer_list<Y> s ) {
    return accuml( forward<F>(f), forward<X>(x), move(s) );
}

template< class F, class S, 
          class X = typename cata::sequence_traits<S>::value_type >
constexpr X foldl( F&& f, S&& s ) {
    return foldl( forward<F>(f), 
                  head(forward<S>(s)), tail_wrap(forward<S>(s)) );
}

template< class F, class X >
constexpr X foldl( F&& f, initializer_list<X> s ) {
    return foldl( forward<F>(f), head(s), tail_wrap(move(s)) );
}

/* foldr f x {1,2,3} -> f(1,f(2,f(3,x))) */
template< class F, class X, class ...S >
constexpr Decay<X> foldr( F&& f, X&& x, S&& ...s ) {
    return foldl( flip(forward<F>(f)), forward<X>(x), 
                  reverse_wrap(forward<S>(s))... );
}

template< class F, class X, class Y >
constexpr Decay<X> foldr( F&& f, X&& x, initializer_list<Y> s ) {
    return foldl( flip(forward<F>(f)), forward<X>(x), reverse_wrap(move(s)) );
}

template< class F, class S, 
          class X = typename cata::sequence_traits<S>::value_type >
constexpr X foldr( F&& f, S&& s ) {
    return foldr( forward<F>(f), 
                  last(forward<S>(s)), init_wrap(forward<S>(s)) );
}

template< class F, class Y >
constexpr Y foldr( F&& f, initializer_list<Y> s ) {
    return foldr( forward<F>(f), head(s), tail_wrap(move(s)) );
}

/* 
 * append({1},{2,3},{4}) -> {1,2,3,4} 
 * Similar to [1]++[2,3]++[4]
 */
template< typename A, typename B = A >
A append( A a, const B& b ) {
    copy( begin(b), end(b), back_inserter(a) );
    return a;
}

template< typename A, typename B, typename C, typename ...D >
A append( A a, const B& b, const C& c, const D& ... d ) {
    return append( append(move(a), b), c, d... );
}

struct Append {
    template< class R, class ...S >
    constexpr R operator() ( R r, S&& ...s ) {
        return append( move(r), forward<S>(s)... );
    }
};

template< class XS, class X >
XS cons( XS xs, X&& x ) {
    xs.push_back( forward<X>(x) );
    return xs;
}

template< class XS, class X, class Y, class ...Z >
XS cons( XS xs, X&& x, Y&& y, Z&& ...z ) {
    return cons (
        cons( move(xs), forward<X>(x) ),
        forward<Y>(y), forward<Z>(z)...
    );
}

template< class XS, class X >
XS rcons( XS xs, X&& x ) {
    xs.push_front( forward<X>(x) );
    return xs;
}

template< class XS, class X, class Y, class ...Z >
XS rcons( XS xs, X&& x, Y&& y, Z&& ...z ) {
    return rcons (
        rcons( move(xs), forward<X>(x) ),
        forward<Y>(y), forward<Z>(z)...
    );
}

struct Cons {
    template< class S, class ...X >
    S operator() ( S s, X&& ...x ) const {
        return cons( move(s), forward<X>(x)... );
    }
};

struct RCons {
    template< class S, class ...X >
    S operator() ( S s, X&& ...x ) const {
        return rcons( move(s), forward<X>(x)... );
    }
};

template< class F, class X, class S,
          class V = vector<Decay<X>> >
V scanl( F&& f, X&& x, const S& s ) {
    V v{ forward<X>(x) };
    for( const auto& y : s )
        v.emplace_back( 
            forward<F>(f)( last(v), y )
        );
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
    using container  = vector<Decay<X>>;
    using reference  = typename container::reference;
    using value_type = typename container::value_type;
    using citerator  = typename container::iterator;

    // Unfortunately, as a requirement for declaring the Fnct::operator() as a
    // constexper convention, this must define its () as const. This feels
    // hackish, but the standard doesn't provide a way of saying a constexpr
    // function is non-const when its arguments aren't constexprs. We can
    // either remove the constexper convention for generality, or just define
    // these as mutable.
    mutable F f;
    mutable container c;

    struct iterator 
        : std::iterator<bidirectional_iterator_tag,value_type> 
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

        reference operator* () {
            grow();
            return *it;
        }

        ptrdiff_t operator- ( const iterator& i ) {
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
constexpr size_t length( const Remember<F,X>& ) {
    return std::numeric_limits<size_t>::max();
}

template< class S, class I, class D = Dup<S> >
D dup_range( I b, I e ) {
    return D( b, e );
}

template< class F, class X, class I = Remember<F,X> >
vector<X> dup( const Remember<F,X>& c ) {
    return c;
}

template< class F, class X, class I = Remember<F,X> >
vector<X> dup( Remember<F,X> c, size_t n ) {
    return _dup<vector<X>>( move(c), n );
}

template< class F, class X, class IC = Remember<F,X>,
          class I = typename IC::iterator >
Dup<IC> dup_range( I b, I e ) {
    return dup_range( b.it, e.it );
}

template< class F, class X >
using Iterate = Remember< Composition<F,Last>, X >;

template< class F, class X, class I = Iterate<F,X> >
constexpr I iterate( F f, X x ) {
    return I ( 
        compose( move(f), Last() ), 
        move(x) 
    );
}

template< class X, class I = Iterate<Id,X> >
constexpr I repeat( X x ) {
    return iterate( Id(), move(x) );
}

template< class F, class X, 
          class I = Remember< BComposition<F,Last,Last>, X > >
constexpr I bi_iterate( F f, X a, X b ) {
    return I (
        bcompose( move(f), Last(1), Last() ),
        move(a), move(b)
    );
}

/* enumerate 1 5 = [1,2,3,4,5] */
template< class X, class Y, class F = decltype(inc<X>)* >
constexpr vector<Decay<X>> enumerate( X&& from, Y&& to, F f = inc<X> ) {
    return take_while (
        less_than(forward<Y>(to)),
        iterate( move(f), forward<X>(from) )
    );
}

/* enumerate 1 = [1..] */
template< class X = unsigned int, class F = decltype(inc<X>)* >
constexpr auto enumerate( X&& x = 1, F f = inc<X> ) 
    // Calling enumerate(1,2) creates an ambiguity on whether you want this or
    // the above version. Just disable this version when the second argument
    // isn't callable.
    -> Decay<decltype( f(declval<X>()), declval<Iterate<F,X>>() )>
{
    return iterate( move(f), forward<X>(x) );
}

template< class X >
vector<Decay<X>> replicate( size_t n, X&& x ) {
    return take( n, repeat(forward<X>(x)) );
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
P middle_closure( F&& f, Y&& y ) {
    return P( forward<F>(f), forward<Y>(y) );
}

/* every_other x [a,b,c...] = [x,a,x,b,x,c...] */
template< class X, class S, class R >
constexpr R _every_other( const X& x, const S& s, R r ) {
    return foldl( middle_closure(Cons(),x), move(r), s );
}

template< class X, class S >
constexpr S every_other( const X& x, const S& s ) {
    return foldl( middle_closure(Cons(),x), S(), s );
}

/* intersparse x [a,b,c] = [a,x,b,x,c] */
template< class X, class S >
constexpr S intersparse( const X& x, const S& s ) {
    return length(s) > 1 ?
        _every_other( x, tail_wrap(s), S{head(s)} ) : s;
}

template< class S, class SS, class R >
constexpr R _every_other_append( const S& s, const SS& ss, R r ) {
    using F = R (*)( R, const S&, SeqRef<SS> );
    return foldl( middle_closure((F)append,s), move(r), ss );
}

template< class S, class SS >
constexpr S every_other_append( const S& s, const SS& ss ) {
    return _every_other_append( s, ss, S() );
}

/* intercalcate "--" {"ab","cd","ef"} */
template< class S, class SS >
S intercalcate( const S& s, const SS& ss ) {
    return not null(ss) ? 
        _every_other_append( s, tail_wrap(ss), S{head(ss)} )
        : S();
}

/* concat {{1},{2,3},{4}} = {1,2,3,4} */
template< class SS, class S = typename SS::value_type >
S concat( const SS& ss ) {
    return foldl( append<S,S>, S(), ss );
}

template< class S >
S sort( S s ) {
    std::sort( begin(s), end(s) );
    return s;
}

template< typename Container >
bool ordered( const Container& c )
{
    return length(c) <= 1
        or mismatch ( 
            begin(c), end(c)-1, 
            begin(c)+1, 
            less_equal<int>() 
        ).second == end(c);
}

/* filter f C -> { x for x in C such that f(x) is true. } */
template< typename Container, typename F >
Container filter( F&& f, Container cont )
{
    cont.erase ( 
        remove_if( begin(cont), end(cont), 
                   fnot( forward<F>(f) ) ),
        end( cont )
    );
    return cont;
}

template< class X, class F, class V = vector<X> >
V filter( F&& f, const initializer_list<X>& cont ) {
    V v;
    v.reserve( length(cont) );
    copy_if( begin(cont), end(cont), back_inserter(v), forward<F>(f) );
    return v;
}

// TODO: Perhaps this could be implemented with a filterFold...
/* filtrate f pred xs = filter pred (map f xs) */
template< class F, class P, class S, 
          class R = decltype( map(declval<F>(),declval<S>()) ) >
R filtrate( F&& f, P&& p, S&& s ) {
    R r;
    for( auto& x : forward<S>(s) ) {
        auto y = forward<F>(f)( x );
        if( forward<P>(p)(y) )
            r.push_back( move(y) );
    }
    return r;
}

/* find pred xs -> Maybe x */
template< class F, class S,
          class Val = typename cata::sequence_traits<S>::value_type >
const Val* find( F&& f, const S& s ) {
    const auto& e = end(s);
    const auto it = find_if( begin(s), e, forward<F>(f) );
    return it != e ? &(*it) : nullptr; 
}

using MaybeIndex = unique_ptr<size_t>;

MaybeIndex JustIndex( size_t x ) { return MaybeIndex( new size_t(x) ); }
MaybeIndex NothingIndex() { return nullptr; }

/* Construct MaybeIndex from an iterator. */
template< class I, class S >
MaybeIndex PossibleIndex( const I& i, const S& s ) {
    return i != end(s) ? JustIndex( distance(begin(s),i) ) : NothingIndex();
}

template< class X, class S >
const X* find_first( const X& x, const S& s ) {
    auto it = std::find( begin(s), end(s), x );
    return it != end(s) ? &(*it) : nullptr;
}

/* cfind x C -> C::iterator */
template< typename S, typename T >
constexpr auto cfind( T&& x, S&& s )
    -> decltype( begin(declval<S>()) )
{
    return find( begin(forward<S>(s)), end(forward<S>(s)),
                 forward<T>(x) );
}

template< typename S, typename F >
constexpr auto cfind_if( F&& f, S&& s )
    -> decltype( begin(declval<S>()) )
{
    return std::find_if( begin(forward<S>(s)), end(forward<S>(s)),
                         forward<F>(f) );
}

template< class S, class T, class F = equal_to<T> >
constexpr auto cfind_not( const T& x, S&& s, F&& f = F() )
    -> decltype( begin(declval<S>()) )
{
    return cfind_if (
        fnot( closure(forward<F>(f),x) ),
        forward<S>(s)
    );
}

template< class F, class S >
MaybeIndex find_index( F&& f, const S& s ) {
    return PossibleIndex( cfind_if(forward<F>(f),s), s );
}

template< class F, class S, class V = vector<size_t> >
V find_indecies( F&& f, const S& s ) {
    return PossibleIndex( cfind_if(forward<F>(f),s), s );
}

template< class S, class D = Dup<S> >
D take( size_t n, S&& s ) {
    return dup( forward<S>(s), n );
}

template< class P, class S, class _S = Dup<S> >
_S take_while( P&& p, S&& s ) {
    auto it = begin(forward<S>(s)); 
    while( it != end(s) and forward<P>(p)(*it) )
         it++ ;

    // TODO: This is a hack and won't work on non-random iterators, but
    // required to work with Remember. I should implement Dup as a class that
    // can be specialized similarly to Functor and Monad.
    return take (
        it - begin(s),
        forward<S>(s)
    );
}

template< class S >
Dup<S> drop( size_t n, S&& s ) {
    return dup( tail_wrap( max( (size_t)length(forward<S>(s))-n, (size_t)0 ),
                           forward<S>(s) ) );
}

template< class P, class S, class _S = Decay<S> >
_S drop_while( P&& p, S&& s ) {
    return _S ( 
        cfind_if( fnot(forward<P>(p)), forward<S>(s) ),
        end( forward<S>(s) )
    );
}

template< class Pred, class S >
S drop_while_end( Pred p, S s ) {
    s.erase( cfind_if(p,s), end(s) );
    return s;
}

template< class X, class R > 
using XInt = typename enable_if< !is_integral<X>::value, R >::type;

template< class I, class S, class _S = Decay<S>, 
          class P = pair<_S,_S> >
constexpr XInt<I,P> split_at( I it, S&& s ) 
{
    return { { begin(forward<S>(s)), it },
             {  it,  end(forward<S>(s)) } };
}

template< class S, class _S = Decay<S>, 
          class P = pair<_S,_S> >
constexpr P split_at( size_t n, S&& s ) {
    return split_at (
        next( begin(forward<S>(s)), 
              min(length(s), n) ), // Don't split passed the end!
        forward<S>(s)
    );
}

template< class P,
          class S, class _S = Decay<S>, 
          class R = pair<_S,_S> >
R span( P&& p, S&& s ) {
    R r;
    for( auto& x : s )
        (forward<P>(p)( x ) ? r.second:r.first).push_back( x );
    return r;
}

template< class P, class S, class _S = Decay<S> >
pair<_S,_S> sbreak( P&& p, S&& s ) {
    return split_at( cfind_if(forward<P>(p),forward<S>(s)), forward<S>(s) );
}

template< class XS, class YS >
bool is_prefix( const XS& xs, const YS& ys ) {
    return std::equal( begin(xs), end(xs), begin(ys) );
}

template< class X, class S >
bool is_prefix( const initializer_list<X>& l, const S& s ) {
    return std::equal( begin(l), end(l), begin(s) );
}

template< class XS, class YS >
bool is_suffix( const XS& xs, const YS& ys ) {
    return is_prefix( reverse_wrap(xs), reverse_wrap(ys) );
}

template< class X, class S >
bool is_suffix( const initializer_list<X>& l, const S& s ) {
    return is_prefix( reverse_wrap(l), reverse_wrap(s) );
}

template< class XS, class YS >
bool is_infix( const XS& xs, const YS& ys ) {
    return std::includes( begin(ys), end(ys), begin(xs), end(xs) );
}

template< class X, class S >
bool is_infix( const initializer_list<X>& l, const S& s ) {
    return std::includes( begin(s), end(s), begin(l), end(l) );
}

template< class XS, class YS >
bool equal( const XS& xs, const YS& ys ) {
    return length(xs) == length(ys) and is_prefix( xs, ys );
}

template< class X, class S >
bool elem( const X& x, const S& s ) {
    return find_first( x, s );
}

template< class X, class S >
bool not_elem( const X& x, const S& s ) {
    return not elem( x, s );
}

template< class X, class S >
MaybeIndex elem_index( const X& x, const S& s ) {
    return PossibleIndex( cfind(x,s), s );
}

template< class X, class S, class V = vector<size_t> >
V elem_indecies( const X& x, const S& s ) {
    V v;
    auto it = begin(s);
    while(true) {
        it = cfind( x, range<S>(it,end(s)) );
        if( it != end(s) )
            v.push_back( distance(begin(s),it++) );
        else 
            break;
    }
    return v;
}

/* insert 3 [1,2,4] = [1,2,3,4] */
template< class X, class S >
S insert( X&& x, S s ) {
    s.insert( 
        upper_bound( begin(s), end(s), forward<X>(x) ),
        forward<X>(x)
    );
    return s;
}

/* insert p y xs -- inserts into the first place where (p x) == True. */
template< class P, class X, class S >
S insert( P&& p, X&& x, S s ) {
    s.insert( 
        upper_bound( begin(s), end(s), forward<X>(x), forward<P>(p) ),
        forward<X>(x)
    );
    return s;
}

template< class X, class S >
S erase( const X& x, S s ) {
    auto it = cfind( x, s );
    if( it != end(s) )
        s.erase( it );
    return s;
}

template< class F, class X, class S >
S erase( F&& f, const X& x, S s ) {
    auto it = cfind_if( closure(forward<F>(f),x), s );
    if( it != end(s) )
        s.erase( it );
    return s;
}

template< class P, class XS, class YS >
XS erase_first( P&& p, XS xs, YS&& ys ) {
    using F = XS(*)( const P&, SeqRef<YS>, XS );
    return foldl (
        // \s x -> erase p x s
        flip( closure( (F)erase, forward<P>(p) ) ), 
        move(xs), forward<YS>(ys) 
    ); 
}

template< class P, class XS, class Y >
XS erase_first( P&& p, XS xs, initializer_list<Y> l ) {
    using F = XS(*)( const P&, const Y&, XS );
    return foldl( flip(closure((F)erase,forward<P>(p))), 
                  xs, move(l) ); 
}

template< class S, class X >
S cons_set( S s, X&& x ) {
    return not_elem(x,s) ?  cons( move(s), forward<X>(x) ) : s;
}

template< class DS, class S, class X >
S cons_difference( const DS& ds, S s, X&& x ) {
    return not_elem(x,ds) ?  cons( move(s), forward<X>(x) ) : s;
}

template< class DS, class S, class X >
S cons_intersection( const DS& ds, S s, X&& x ) {
    return elem(x,ds) ?  cons( move(s), forward<X>(x) ) : s;
}

template< class S, class X >
S cons_when( bool b, S s, X&& x ) {
    return b ? cons( move(s), forward<X>(x) ) : s;
}

template< class S >
S nub( const S& s ) {
    using F = S(*)( S, SeqRef<S> );
    return foldl( (F)cons_set, S(), s );
}

template< class XS, class YS, class R = Decay<XS> >
R difference( XS&& xs, const YS& ys ) {
    using F = R(*)( const YS&, XS, SeqRef<XS> );
    return foldl ( 
        closure( (F)cons_difference, ys ),
        R(), forward<XS>(xs)
    );
}
template< class XS, class YS >
XS sunion( XS xs, YS&& ys ) {
    using F = XS(*)( XS, SeqRef<YS> );
    return foldl( (F)cons_set, move(xs), forward<YS>(ys) );
}

template< class XS, class YS, class R = Decay<XS> >
R intersect( XS&& xs, const YS& ys ) {
    using F = R(*)( const YS&, XS, SeqRef<XS> );
    return foldl (
        closure( (F)cons_intersection, ys ),
        R(), forward<XS>(xs)
    );
}

template< class S >
SeqVal<S> sum( const S& s ) {
    return std::accumulate( begin(s), end(s), 0 );
}

template< class S >
SeqVal<S> product( const S& s ) {
    return std::accumulate( begin(s), end(s), 1, Mult() );
}

template< class S >
SeqRef<S> maximum( S&& s ) {
    return *std::max_element( begin(forward<S>(s)), end(forward<S>(s)) );
}

template< class F, class S >
bool all( F&& f, const S& s ) {
    return std::all_of( begin(s), end(s), forward<F>(f) );
}

template< class F, class S >
bool any( F&& f, const S& s ) {
    return any_of( begin(s), end(s), forward<F>(f) );
}

template< class F, class S >
bool none( F&& f, const S& s ) {
    return std::none_of( begin(s), end(s), forward<F>(f) );
}

template< class F, class XS, class YS >
XS intersect_if( F&& f, const XS& xs, const YS& ys ) {
    XS r;
    for( auto& x : xs )
        if( any( closure(forward<F>(f),x), ys ) )
            r.push_back( x );
    return r;
}

template< class XS, class YS, class U = unique_ptr<YS> >
U strip_prefix( const XS& xs, const YS& ys ) {
    return not is_prefix( xs, ys ) ? nullptr :
        U( new YS( next(begin(ys),length(xs)), end(ys) ) );
}

template< class M > struct Return;

template< class XS, class YS >
constexpr XS maybe_cons_range( XS xs, YS&& ys ) {
    return not null(ys) ? cons( move(xs), forward<YS>(ys) )
        : xs;
}

template< class S, class P = std::equal_to<SeqRef<S>>,
          class _S = Decay<S>, class V = vector<_S> >
V group( S&& s, P&& p = P() ) {
    V v;

    auto it = begin( forward<S>(s) ), next = it;
    const auto e = end( forward<S>(s) );

    for( ; it != e; it = next) {
        auto adj = adjacent_find( it, e, forward<P>(p) );
        v = maybe_cons_range ( 
            // First, cons all the non-adjacent members.
            foldl (
                // \v s -> cons v (return s)
                flip( compose(flip(Cons()), Return<_S>()) ),
                move(v), range<S>( it, adj ) 
            ),
            // Then cons the adjacent members.
            _S( adj, 
                next = cfind_not( *adj, range<S>(adj,e),
                                  forward<P>(p) ) ) 
        );
    }

    return v;
}

template< class S, class V = vector<S> >
V _inits( const S& s ) {
    V v;
    for( auto it = begin(s); it != end(s); it++ )
        v.emplace_back( S(begin(s),it) );
    return v;
}

template< class S, class V = vector<S> >
V inits( const S& s ) {
    return not null(s) ? _inits(s) : V();
}

template< class S, class V = vector<S> >
V _tails( const S& s ) {
    V v{ S() };
    for( auto it = end(s); it != begin(s); )
        v.emplace_back( S(--it,end(s)) );
    return v;
}

template< class S, class V = vector<S> >
V tails( const S& s ) {
    return not null(s) ? _tails(s) : V();
}

template< class S >
bool _next_p_ref( S& s ) {
    return std::next_permutation( begin(s), end(s) );
}

template< class S >
S _next_p( S s ) {
    _next_p_ref( s );
    return s;
}

template< class S >
using SeqSeq = vector<Decay<S>>;

template< class S, class V = SeqSeq<S> >
V sorted_permutations( S&& original ) {
    V r{ forward<S>(original) };
    Dup<S> next;
    while( next = last(r), _next_p_ref(next) )
        r.emplace_back( move(next) );
    return r;
}

template< class S, class V = SeqSeq<S> >
V _permutations( S&& original ) {
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
V permutations( S&& original ) {
    return ordered(original) ? sorted_permutations( forward<S>(original) )
        : _permutations( forward<S>(original) );
}

/* 
 * concatMap f s = concat (map f s)
 *      where f is a function: a -> [b]
 *            map f s produces a type: [[b]]
 * The operation "map then concat" may be inefficient compared to an inlined
 * loop, which would not require allocating a sequence of sequences to be
 * concatenated. This is an optimization that should be equivalent to the
 * inlined loop.
 */
template< class F, class ...S,
          class R = Result<F,SeqRef<S>...> >
R concatMap( F&& f, S&& ...xs ) {
    return foldl (
        // \xs x -> append xs (f x)
        flip( compose( flip(Append()), forward<F>(f) ) ), 
        R(), forward<S>(xs)...
    );
}

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
 * Just  x -> Maybe (Just x) | with a definite value.
 * Nothing -> Maybe Nothing  | with definitely no value.
 */
template< class T, class M = std::unique_ptr<T> > 
constexpr M Just( T t ) {
    return M( new T(move(t)) );
}

struct ReturnJust {
    template< class X >
    unique_ptr<Decay<X>> operator() ( X&& x ) {
        return Just( forward<X>(x) );
    }
};

template< class T, class M = std::unique_ptr<T> > 
constexpr M Nothing() {
    return nullptr;
}

/* maybe b (a->b) (Maybe a) -> b */
template< class R, class F, class P >
constexpr R maybe( R&& nothingVal, F&& f, P&& m ) {
    return m ? forward<F>(f)( *forward<P>(m) )
             : forward<R>( nothingVal );
}

/* 
 * Just f  * Just x  = Just (f x)
 * _       * _       = Nothing
 * Just x  | _       = Just x
 * Nothing | Just x  = Just x
 * Nothing | Nothing = Nothing
 */
template< class F, class P, 
          class Ret = decltype( Just( (*declval<F>())(*declval<P>()) ) ) >
Ret operator* ( F&& a, P&& b ) {
    return a and b ? Just( (*forward<F>(a))(*forward<P>(b)) ) 
                   : nullptr;
}

template< class P > 
constexpr P operator|| ( P&& a, P&& b ) {
    return a ? forward<P>(a) : forward<P>(b); 
}

template< class P, class R = decltype( Just(*declval<P>()) ) > 
constexpr R operator|| ( const P& a, const P& b ) {
    return a ? Just(*a) : b ? Just(*b) : nullptr;
}

/* Either a b : Left a | Right b */
template< class L, class R >
struct Either
{
    typedef L left_type;
    typedef R right_type;

    unique_ptr<left_type> left;
    unique_ptr<right_type> right;

    // TODO: Make these type constructors, not types.
    struct Left { 
        left_type value;
        constexpr Left( left_type value ) : value(move(value)) { }
    };

    struct Right { 
        right_type value;
        constexpr Right( right_type value ) : value(move(value)) { }
    };

    template< class X >
    explicit constexpr Either( X&& x )  
        : left(  new left_type (move(x.value)) ) { }
    explicit constexpr Either( Right x ) 
        : right( new right_type(move(x.value)) ) { }
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
    -> decltype( declval<F>()(*e.right) )
{
    return e.right ? forward<F>(f)(*e.right) : forward<G>(g)(*e.left);
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

template< class F, class G, class ...H,
          class Fn = Functor< Cat<G> > >
constexpr auto fmap( F&& f, G&& g, H&& ...h )
    -> decltype( Fn::fmap(declval<F>(),declval<G>(),declval<H>()...) ) 
{
    return Fn::fmap( forward<F>(f), forward<G>(g), forward<H>(h)... );
}

struct FMap {
    template< class F, class ...X >
    constexpr auto operator() ( F&& f, X&& ...x ) 
        -> decltype( fmap(declval<F>(),declval<X>()...) )
    {
        return fmap( forward<F>(f), forward<X>(x)... );
    }
};

template< class F > 
constexpr Closure<FMap,F> fmap( F&& f ) {
    return closure( FMap(), forward<F>(f) );
}

/* fmap f g = compose( f, g ) */
template< class Function >
struct Functor<Function> {
    template< class F, class G >
    static auto fmap( F&& f, G&& g ) 
        -> decltype( compose(declval<F>(),declval<G>()) )
    {
        return compose( forward<F>(f), forward<G>(g) );
    }
};

/* fmap f Pair(x,y) = Pair( f x, f y ) */
template< class X, class Y >
struct Functor< pair<X,Y> > {
    template< unsigned N, class P >
    using Nth = decltype( get<N>( declval<P>() ) );

    template< class F, class ...P >
    using PX = decltype( declval<F>()( declval<Nth<0,P>>()... ) );
    template< class F, class ...P >
    using PY = decltype( declval<F>()( declval<Nth<1,P>>()... ) );

    template< class F, class ...P >
    static constexpr auto fmap( F&& f, P&& ...p ) 
        -> decltype (
            make_pair( declval<PX<F,P...>>(), declval<PY<F,P...>>() )
        )
    {
        return make_pair( forward<F>(f)(forward<P>(p).first...),
                  forward<F>(f)(forward<P>(p).second...) );
    }
};

template<>
struct Functor< cata::maybe > {
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
        -> decltype( Just(declval<F>()( *declval<M>()... )) )
    {
        return each( forward<M>(m)... ) ? 
            Just( forward<F>(f)( *forward<M>(m)... ) ) : nullptr;
    }
};

template<>
struct Functor< cata::sequence > {
    /* f <$> [x0,x1,...] = [f x0, f x1, ...] */
    template< class F, class ...S >
    static constexpr decltype( map(declval<F>(),declval<S>()...) )
    fmap( F&& f, S&& ...s ) {
        return map( forward<F>(f), forward<S>(s)... );
    }
};

template< class L, class R >
struct Functor< Either<L,R> > {
    template< class F, class FR = decltype( declval<F>()(declval<R>()) ) >
    static Either<L,FR>  fmap( F&& f, const Either<L,R>& e ) {
        return e.right ? Right<L>( forward<F>(f)(*e.right) ) 
                       : Left<FR>( *e.left );
    }
};

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
decltype( Mo::mempty() ) mempty() { return Mo::mempty(); }

template< class M1, class M2, 
          class Mo = Monoid<Cat<M1>> >
auto mappend( M1&& a, M2&& b ) 
    -> decltype( Mo::mappend(declval<M1>(),declval<M2>()) )
{
    return Mo::mappend( forward<M1>(a), forward<M2>(b) );
}

/*
 * mappend const& 
 * The above && version will always be preferred, except in the case of a
 * const&. In other words, this version will NEVER be preferred otherwise.
 */

template< class S, class V = SeqVal<S>, class M = Monoid<Cat<V>> >
auto mconcat( S&& s ) -> decltype( M::mconcat(declval<S>()) ) {
    return M::mconcat( forward<S>(s) );
}


// When we call mappend from within Monoid<M>, lookup deduction will see its
// own implementation of mappend. This ensures a recursive mappend gets
// properly forwarded.
template< class XS, class YS > 
decltype( mappend(declval<XS>(),declval<YS>()) )
fwd_mappend( XS&& xs, YS&& ys ) {
    return mappend( forward<XS>(xs), forward<YS>(ys) );
}

template< class M > 
decltype( mempty<M>() ) fwd_mempty() { return mempty<M>(); }

template<> struct Monoid< cata::sequence > {
    template< class S >
    static S mempty() { return S{}; }

    template< class XS, class YS >
    static XS mappend( XS xs, YS&& ys ) {
        return append( move(xs), forward<YS>(ys) );
    }

    template< class SS >
    static auto mconcat( SS&& ss ) -> decltype( concat(declval<SS>()) )
    {
        return concat( forward<SS>(ss) ); 
    }
};

/* Monoid (Maybe X) -- where X is a monoid. */
template<> struct Monoid< cata::maybe > {
    template< class M >
    static constexpr M mempty() { return nullptr; }

    template< class M >
    static M dup( const M& m ) {
        return maybe( mempty<M>(), ReturnJust(), m );
    }

    template< class X >
    using IsRVal = is_rvalue_reference<X>;
    template< class X >
    using ERVal = typename enable_if< IsRVal<X>::value, Decay<X> >::type;

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
        return x and y ? Just( fwd_mappend(*forward<M>(x), *forward<M>(y)) )
            : dup(forward<M>(x) || forward<M>(y));
    }

    /* mconcat [Just x, Just y, Nothing] = Just (x <> y <> Nothing)*/
    template< class S, class M = SeqRef<S>, class R = Decay<M> > 
    static R mconcat( S&& s ) {
        using F = R (*) ( const M&, const M& );
        return foldl( (F)mappend, mempty<R>(), forward<S>(s) );
    }
};

/* Monoid (Pair X X) */
template< class X, class Y > struct Monoid< pair<X,Y> > {
    typedef pair<X,Y> P;

    static P mempty() { return P( fwd_mempty<X>(), fwd_mempty<Y>() ); }

    static P mappend( const P& a, const P& b ) {
        return P( fwd_mappend(a.first,b.first), 
                  fwd_mappend(a.second,b.second) );
    }

    template< class S > static P mconcat( const S& s ) {
        return foldr( mappend, mempty(), s );
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
template< class A, class B,
          class Mo = Monad<Cat<A>> >
decltype( Mo::mdo(declval<A>(),declval<B>()) ) 
mdo( A&& a, B&& b ) {
    return Mo::mdo( forward<A>(a), forward<B>(b) ); 
}

/* m >>= k */
template< class M, class F,
          class Mo = Monad<Cat<M>> >
auto mbind( M&& m, F&& f ) 
    -> decltype( Mo::mbind(declval<M>(),declval<F>()) ) 
{
    return Mo::mbind( forward<M>(m), forward<F>(f) ); 
}

/* return<M> x = M x */
template< class M, class X > 
M mreturn( X&& x ) {
    return Monad< Cat<M> >::template mreturn<M>( forward<X>(x) );
}

/* mreturn () = (\x -> return x) */
template< class M, class Mo = Monad<Cat<M>> > 
constexpr auto mreturn() -> decltype( Mo::mreturn() ) { 
    return Mo::mreturn();
}

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
decltype( mbind(declval<X>(),declval<Y>()) ) 
operator >>= ( X&& x, Y&& y ) {
    return mbind( forward<X>(x), forward<Y>(y) );
}

template< class X, class Y >
decltype( mdo(declval<X>(),declval<Y>()) ) 
operator >> ( X&& x, Y&& y ) {
    return mdo( forward<X>(x), forward<Y>(y) );
}

template<> struct Monad< cata::sequence > {
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
        auto size = length( a );
        while( size-- )
            c = append( move(c), b );
        return c;
    }

    /* m >>= k -- where m is a sequence. */
    template< class S, class F >
    static decltype( concatMap(declval<F>(),declval<S>()) )
    mbind( S&& xs, F&& f ) { 
        // xs >>= f = foldr g [] xs 
        //     where g acc x = acc ++ f(x)
        //           ++ = append
        return concatMap( f, xs );
    }
};

template< class P > struct IsPointerImpl { 
    using reference = decltype( *declval<P>() );
    using bool_type = decltype( (bool)declval<P>() );
};

template<> struct Monad< cata::maybe > {
    template< class M > using traits = cata::maybe_traits<M>;
    template< class M > using value_type = typename traits<M>::value_type;
    template< class M > using smart_ptr  = typename traits<M>::smart_ptr;

    template< class M, class X, class P = smart_ptr<M> >
    static P mreturn( X&& x ) { 
        return P( new X(forward<X>(x)) ); 
    }

    template< class M >
    using Return = smart_ptr<M> (*) ( value_type<M> );

    static constexpr ReturnJust mreturn() { return ReturnJust(); }

    template< class M >
    static constexpr smart_ptr<M> mfail(const char*) { return nullptr; }

    template< class M, class PZ >
    static constexpr PZ mdo( const M& x, PZ&& z ) {
        return x ? forward<PZ>(z) : nullptr;
    }

    template< class M, class F >
    using Result = Result< F, value_type<M> >;

    template< class M, class F >
    static constexpr Result<M,F> mbind( M&& x, F&& f ) {
        return x ? forward<F>(f)( *forward<M>(x) ) : nullptr;
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

template< class M1, class M2, 
          class Mo = MonadPlus<Cat<M1>> >
auto mplus( M1&& a, M2&& b ) 
    -> decltype( Mo::mplus(declval<M1>(),declval<M2>()) )
{
    return Mo::mplus( forward<M1>(a), forward<M2>(b) );
}

template<> struct MonadPlus< cata::sequence > {
    template< class S >
    static S mzero() { return S(); }

    template< class SX, class SY >
    static auto mplus( SX&& sx, SY&& sy ) 
        -> decltype( append(declval<SX>(),declval<SY>()) )
    {
        return append( forward<SX>(sx), forward<SY>(sy) );
    }
};

template<> struct MonadPlus< cata::maybe > {
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
constexpr Squash<F> squash( F f )
{
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

/* zip_with f A B -> { f(a,b) for a in A and b in B } */
    template< typename Container, typename F >
Container zip_with( F&& f, const Container& a, Container b ) {
    transform( begin(a), end(a), begin(b), 
               begin(b), forward<F>(f) );
    return b;
}

template< class F, class R, class ...S >
R _zip_with( F&& f, R r, const S& ...s ) {
return each( NotNull(), s... ) ?
    _zip_with( forward<F>(f),
               cons( move(r), forward<F>(f)( head(s)... ) ),
               tail_wrap( s )... )
    : r;
}

template< class F, class XS, class ...YS, class R = Dup<XS> >
R zip_with( F&& f, const XS& xs, const YS& ...ys ) {
    return _zip_with( forward<F>(f), R(), xs, ys... );
}


/*
 * cleave x f g h -> { f(x), g(x), h(x) }
 * Inspired by Factor, the stack-based language.
 */
template< typename X, typename F, typename ... Fs,
          typename R = decltype( declval<F>()(declval<X>()) ) >
constexpr array<R,sizeof...(Fs)+1> cleave( X&& x, F&& f, Fs&& ... fs ) 
{
    return {{ forward<F> (f )( forward<X>(x) ), 
              forward<Fs>(fs)( forward<X>(x) )... }};
}

/* cleave_with f x y z =  { f(x), f(y), f(z) } */
template< class F, class A, class ...B >
constexpr auto cleave_with( F&& f, A&& a, B&& ...b )
    -> array< decltype( declval<F>()(declval<A>()) ), sizeof...(B)+1 > 
{
    return {{ f(forward<A>(a)), f(forward<B>(b))... }};
}

template< class T, unsigned int N, class F >
array<T,N> generate( F&& f ) {
    array<T,N> cont;
    generate( begin(cont), end(cont), forward<F>(f) );
    return cont;
}

template< class T, class F >
vector<T> generate( F&& f, unsigned int n ) {
    vector<T> c; 
    c.reserve(n);
    while( n-- )
        c.push_back( forward<F>(f)() );
    return c;
}

template< class Cmp, class S, class R = decltype( begin(declval<Cmp>()) ) >
constexpr R max( Cmp&& cmp, const S& cont ) {
return max_element( begin(cont), end(cont), forward<Cmp>(cmp) );
}

template< class S >
constexpr auto max( const S& cont ) -> decltype( begin(cont) ) {
return max_element( begin(cont), end(cont) );
}

template< class Cmp, class S >
constexpr auto min( Cmp&& cmp, const S& cont ) -> decltype( begin(cont) ) {
    return min_element( begin(cont), end(cont), forward<Cmp>(cmp) );
}

template< class Container >
constexpr auto min( const Container& cont ) -> decltype( begin(cont) ) {
    return min_element( begin(cont), end(cont) );
}

} // namespace pure

