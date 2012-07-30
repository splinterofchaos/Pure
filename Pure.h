
#pragma once

#include <algorithm>
#include <numeric>
#include <functional>
#include <iterator>
#include <array>
#include <vector>
#include <utility>
#include <tuple>

namespace pure {

using namespace std;

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

    constexpr PartialApplication( F&& f, Arg&& arg )
        : f(forward<F>(f)), arg(forward<Arg>(arg))
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
    constexpr PartialApplication( F&& f, Arg1&& arg1, Args&& ...args )
        : PartialApplication< PartialApplication<F,Arg1>, Args... > (
            PartialApplication<F,Arg1>( forward<F>(f), forward<Arg1>(arg1) ),
            forward<Args>(args)...
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

    constexpr Composition( F&& f, G&& g ) 
        : f(forward<F>(f)), g(forward<G>(g)) 
    {
    }

    template< class ...Args >
    constexpr auto operator() ( Args&& ...args )
        -> decltype( f(g(declval<Args>()...)) )
    {
        return g( f(forward<Args>(args)...) );
    }
};

template< class F, class G, class ...H >
struct Composition<F,G,H...> : Composition<F,Composition<G,H...>>
{
    typedef Composition<G,H...> Comp;

    constexpr Composition( F&& f, G&& g, H&& ...h )
        : Composition<F,Composition<G,H...>> ( 
            forward<F>(f), 
            Comp( forward<G>(g), forward<H>(h)... )
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

/* map f {1,2,3} -> { f(1), f(2), f(3) } */
template< typename Container, typename F >
Container map( F&& f, Container cont ) 
{
    transform( begin(cont), end(cont), begin(cont), 
                    forward<F>(f) );
    return cont;
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

/* find x in C -> iterator to x or end(C) */
template< typename Container, typename T >
constexpr auto find( const T& value, Container&& cont )
    -> decltype( begin(cont) )
{
    return find( begin(cont), end(cont), value );
}

template< typename Container, typename F >
constexpr auto find_if( const F& f, Container&& cont )
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

