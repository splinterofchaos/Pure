
#include "List.h"

#include <algorithm>
#include <set>

namespace pure {

namespace set {

/* 
 * Set theory!
 *
 * A small set of operator overloads to simplify working with sets, or
 * generically, sequences.
 *
 * S(x,y,z...) -- create a set from x, y, z...
 * !s          -- s is null.
 * +s          -- the length of s (Or: |s|.)
 * x > s | s < x -- s contains x
 * s >> x      -- s without x
 * s << x      -- s with x
 * xs | ys     -- The union of xs and ys.
 * xs / ys     -- The difference of xs and ys.
 * xs % ys     -- The intersection of xs and ys.
 * xs * xs     -- The Cartesian product of xs and xs.
 *
 * The source has been split into two sections: generic, which implements
 * versions of these functions that should work on all containers, and ordered,
 * which takes advantage of ordering for optimization. The common namespace
 * implements functions that are the same in both.
 *
 * To use this, fully import the namespace with one of the fallowing:
 *      using namespace pure::set; // for generic
 *      using namespace pure::set::ordered;
 */

namespace common {

/* Create a set. */
template< class X, class ...Y >
std::set<Decay<X>> S( X&& x, Y&& ...y ) {
    return std::set<Decay<X>>({ std::forward<X>(x), 
                                std::forward<Y>(y)... });
}

/* s is null */
template< class S >
bool operator ! ( const S& s ) {
    return pure::list::null(s);
}

/* |s| -- the magnitude of s. (The closest thing we have to an abs sign.) */
template< class S >
unsigned long long operator + ( const S& s ) {
    return pure::list::length( s );
}

/* The Cartesian product of xs and ys. */
template< class XS, class YS, 
          class X = list::SeqVal<XS>, class Y = list::SeqVal<YS>,
          class P = std::pair<X,Y>,
          class R = list::Remap<XS,P> >
R operator * ( const XS& xs, const YS& ys ) {
    return list::map (
        []( const X& x, const Y& y ) { return P{x,y}; },
        xs, ys
    );
}

} // namespace common

namespace generic {

using namespace common;

// These operations are guaranteed to work even if the arguments are unsorted
// or contain duplicates.


/* x is an element of s */
template< class X, class S >
bool operator < ( const X& x, const S& s ) {
    return pure::list::elem( x, s );
}

/* s contains x */
template< class S, class X >
bool operator > ( const S& s, const X& x ) {
    return pure::list::elem( x, s );
}

/* xs is a subset of ys */
template< class XS, class YS >
bool operator <= ( XS xs, const YS& ys ) {
    for( const auto& y : ys ) {
        xs <<= y;
        // If everything in xs has been found in ys, xs is a subset of ys.
        if( ! xs )
            return true;
    }
    return false;
}

/* s without x */
template< class S, class X >
Decay<S> operator >> ( S&& s, const X& x ) {
    return pure::list::erase( x, std::forward<S>(s) );
}

/* (reference version) */
template< class S, class X >
S& operator >>= ( S& s, const X& x ) {
    s = std::move(s) >> x;
    return s;
}

/* s appended with x */
template< class S, class X >
Decay<S> operator << ( S&& s, X&& x ) {
    return pure::list::cons( std::forward<S>(s), std::forward<X>(x) );
}

/* (reference version) */
template< class S, class X >
S& operator <<= ( S& s, X&& x ) {
    s = std::move(s) << std::forward<X>(x);
    return s;
}

/* The union of xs and ys (with no duplicates). */
template< class XS, class YS >
XS operator | ( XS xs, YS&& ys ) {
    for( auto y : std::forward<YS>(ys) ) 
        if( not (y<xs) )
            xs <<= y;
    return xs;
}

/* (reference version) */
template< class XS, class YS >
XS& operator |= ( XS& xs, YS&& ys ) {
    xs = std::move(xs) | std::forward<YS>(ys);
    return xs;
}   

/* Every x from xs such that there is no y from ys where x = y. */
template< class XS, class YS >
XS operator / ( XS xs, const YS& ys ) {
    for( const auto& y : ys )
        xs >>= y;
    return xs;
}

/* (reference version) */
template< class XS, class YS >
XS& operator /= ( XS& xs, const YS& ys ) {
    xs = std::move(xs) / ys;
    return xs;
}

/* The intersection of xs and ys. (Or: The remainder of xs/ys.) */
template< class XS, class YS >
XS operator % ( const XS& xs, const YS& ys ) {
    XS r;
    for( const auto& y : ys )
        if( y < xs )
            r <<= y;
    return r;
}

/* (reference version) */
template< class XS, class YS >
XS& operator %= ( XS& xs, YS&& ys ) {
    xs = std::move(xs) % std::forward<YS>(ys);
    return xs;
}   

} // namespace generic

namespace ordered {

using namespace common;

// These versions take advantage of knowing the container is ordered.

/* x is an element of s */
template< class X, class S >
bool operator < ( const X& x, const S& s ) {
    return std::binary_search( begin(s), end(s), x );
}

/* s contains x */
template< class S, class X >
bool operator > ( const S& s, const X& x ) {
    return std::binary_search( begin(s), end(s), x );
}

/* xs is a subset of ys */
template< class XS, class YS >
bool operator <= ( const XS& xs, const YS& ys ) {
    return std::includes( begin(ys), end(ys), begin(xs), end(xs) );
}

/* s without x */
template< class S, class X >
Decay<S> operator >> ( S&& s, const X& x ) {
    return pure::list::erase( x, std::forward<S>(s) );
}

/* (reference version) */
template< class S, class X >
S& operator >>= ( S& s, const X& x ) {
    s = std::move(s) >> x;
    return s;
}

/* s appended with x */
template< class S, class X >
Decay<S> operator << ( S&& s, X&& x ) {
    return pure::list::insert( std::forward<X>(x), std::forward<S>(s) );
}

/* (reference version) */
template< class S, class X >
S& operator <<= ( S& s, X&& x ) {
    s = std::move(s) << std::forward<X>(x);
    return s;
}

/* The union of xs and ys (with no duplicates). */
template< class XS, class YS >
XS operator | ( const XS& xs, const YS& ys ) {
    XS r;
    std::merge( begin(xs), end(xs), begin(ys), end(ys), 
                       list::tailInserter(r) );
    return r;
}

/* (reference version) */
template< class XS, class YS >
XS& operator |= ( XS& xs, YS&& ys ) {
    xs = std::move(xs) | std::forward<YS>(ys);
    return xs;
}   

/* Every x from xs such that there is no y from ys where x = y. */
template< class XS, class YS >
XS operator / ( const XS& xs, const YS& ys ) {
    XS r;
    std::set_difference( begin(xs), end(xs), begin(ys), end(ys),
                         list::tailInserter(r) );
    return r;
}

/* (reference version) */
template< class XS, class YS >
XS& operator /= ( XS& xs, const YS& ys ) {
    xs = std::move(xs) / ys;
    return xs;
}

/* The intersection of xs and ys. (Or: The remainder of xs/ys.) */
template< class XS, class YS >
XS operator % ( const XS& xs, const YS& ys ) {
    XS r;
    std::set_intersection( begin(xs), end(xs), begin(ys), end(ys),
                           list::tailInserter(r) );
    return r;
}

/* (reference version) */
template< class XS, class YS >
XS& operator %= ( XS& xs, YS&& ys ) {
    xs = std::move(xs) % std::forward<YS>(ys);
    return xs;
}   

} // namespace ordered

using namespace generic;

} // namespace set

}
