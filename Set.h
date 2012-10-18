
#include "List.h"

#include <algorithm>
#include <set>

namespace pure {

namespace set {

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

}

}
