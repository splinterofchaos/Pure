
#include "Pure.h"

#include <cstdio>
#include <cmath>
#include <vector>
#include <array>
#include <list>

#include <iostream>

using namespace std;
using namespace pure;

template< typename Container >
void print( const char* const msg, const Container& v )
{
    cout << msg << "\n";
    copy( v.begin(), v.end(), ostream_iterator<float>(cout, " ") );
    cout << endl;
}

typedef array<int,2> Vec;

Vec operator + ( Vec&& a, Vec&& b )
{ 
    return zip_with( plus<int>(), forward<Vec>(a), forward<Vec>(b) ); 
}

Vec operator * ( Vec&& a, int x )
{
    return map( partial(multiplies<int>(),x), forward<Vec>(a) ); 
}
Vec operator * ( int x, Vec&& a )      { return forward<Vec>(a) * x; }
Vec operator * ( const Vec& a, int x ) { return Vec(a) * x; }
Vec operator * ( int x, const Vec& a ) { return Vec(a) * x; }

/* Complete the quadratic equation with a pre-calculated square root. */
constexpr float _qroot( float a, float b, bool negate, float root )
{
    return (-b + (negate ? -root:root)) / (2 * a);
}

/* 
 * quadratic root(a,b,c) = x or y.
 * pure:: functions prefer to work with sequences, so this returns an array
 * containing just the two possible values of x.
 */
constexpr std::array<float,2> quadratic_root( float a, float b, float c )
{
    return cleave ( 
        sqrt(b*b - 4*a*c),
        partial( _qroot, a, b, false ),
        partial( _qroot, a, b, true  )
    );
}

constexpr float _qroot2( float a, float b, float root )
{
    return (-b + root) / (2 * a);
}

constexpr float _qroot_root( float a, float b, float c )
{
    return sqrt( b*b - 4*a*c );
}

constexpr std::array<float,2> quadratic_root2( float a, float b, float c )
{
    return cleave_with (
        partial( _qroot2, a, b ),
        _qroot_root(a,b,c),
        - _qroot_root(a,b,c)
    );
}

int five() { return 5; }
int times_two(int x) { return x * 2; }
int plus_two(int x) { return x + 2; }
int times(int x,int y) { return x*y; }
int square( int x ) { return times(x,x); }
float to_float( int x ) { return x; }

string show( int x ) {
    char digits[20];
    sprintf( digits, "%d", x );
    return digits;
}

string show( string s ) {
    return "\"" + s + "\"";
}

template< class X > string showJust( X x ) {
    return "Just " + show( x );
}

template< class T > string show( const Maybe<T>& m ) {
    typedef typename Maybe<T>::value_type V;
    return maybe( string("Nothing"), showJust<V>, m );
}

template< class R > string showRight( const R& r ) 
{ return "Right " + show( r ); }
template< class L > string showLeft( const L& l ) 
{ return "Left "  + show( l ); }

template< class L, class R > 
string show( const Either<L,R>& e ) {
    typedef typename Either<L,R>::right_type RT;
    typedef typename Either<L,R>::left_type LT;
    return either( showRight<RT>, showLeft<LT>, e );
}

int main()
{
    printf (
        "sum of (1,2,3,4) = %d\n", // = 10
        foldl<int>( std::plus<int>(), vector<int>{1,2,3,4} )
    );
    printf (
        "sum of (4,3,2,1) = %d\n", // = 10
        foldr<int>( std::plus<int>(), vector<int>{1,2,3,4} )
    );

    auto sevens = Vec{{2,5}} + Vec{{5,2}};
    printf( "<5,2> + <2,5> = <%d,%d>\n", sevens[0], sevens[1] );
    auto fourteens = sevens * 2;
    printf( "<7,7> * 2 = <%d,%d>\n", fourteens[0], fourteens[1] );

    auto roots = quadratic_root( 1, 3, -4 );
    printf (
        "quadratic root of x^2 + 3x - 4 = 0 : %f or %f\n",
        roots[0], roots[1]
    );
    roots = quadratic_root2( 1, 3, -4 );
    printf (
        "quadratic root of x^2 + 3x - 4 = 0 : %f or %f\n",
        roots[0], roots[1]
    );

    constexpr auto sqrDoublePlus2 = compose( plus_two, times_two, square );
    printf( "3^2 * 2 + 2 = 9 * 2 + 2 = %d\n", sqrDoublePlus2(3) );

    printf( "5 * 2 = %d\n", partial(times,5)(2) );
    printf( "5 * 2 = %d\n", partial(times,5,2)() );

    printf( "fmap (+2) (pure 1) = %d\n", fmap(plus_two,pure::pure(1))() );
    printf( "fmap (+2) (+2) 1   = %d\n", fmap(plus_two,plus_two)(1) );

    auto pair = fmap( plus_two, std::make_pair(1,2) );
    printf( "fmap (+2) (Pair 1 2) = Pair %d %d\n", 
            pair.first, pair.second );

    auto oneTwo = fmap( plus_two, std::list<int>{1,2} );
    printf( "fmap (+2) [1,2] = [%d,%d]\n", oneTwo.front(), oneTwo.back() );

    printf( "fmap (+2) (Just 2)  = %s\n", 
            show( fmap(plus_two, Just(2)) ).c_str() );
    printf( "fmap (+2) (Nothing) = %s\n", 
            show( fmap(plus_two, Nothing<int>()) ).c_str() );

    printf( "fmap (+2) (Left \"yawn\") = %s\n", 
            show( fmap(plus_two,Left<int>("yawn")) ).c_str() );
    printf( "fmap (+2) (Right 5)     = %s\n",
            show( fmap(plus_two,Right<string>(5)) ).c_str() );

    vector<int> N = {1,2,3,4,5,6,7,8};
    int n = 5;
    auto equalsN = partial( equal_to<int>(), n );
    printf( "find (==5) [1,2,3,4,5,6,7,8] = %s\n", 
            show( find(equalsN, N) ).c_str() );
    n = 9;
    printf( "find (==9) [1,2,3,4,5,6,7,8] = %s\n", 
            show( find(equalsN, N) ).c_str() );

    printf( "Just (+2) <*> Just 2  = %s\n",
            show( Just(plus_two) * Just(2) ).c_str() );
    printf( "Just (+2) <*> Nothing = %s\n",
            show( Just(plus_two) * Nothing<int>() ).c_str() );

    auto rp2 = Right<int>( plus_two );
    printf( "Right (+2) <*> Right 1 = %s\n",
            show( Right<int>(plus_two) * Right<int>(1) ).c_str() );
    printf( "Right (+2) <*> Left  1 = %s\n",
            show( Right<int>(plus_two) * Left<int>(1)  ).c_str() );

    printf( "Nothing <|> Just 2   = %s\n", 
            show( Nothing<int>() | Just(2) ).c_str() );
    printf( "Nothing <|> Nothing  = %s\n", 
            show( Nothing<int>() | Nothing<int>() ).c_str() );
}
