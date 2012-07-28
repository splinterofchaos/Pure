
#include "Pure.h"

#include <cstdio>
#include <cmath>
#include <vector>
#include <array>

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
        partial( _qroot, a, b, true ),
        partial( _qroot, a, b, false )
    );
}

int five() { return 5; }
int times_two(int x) { return x * 2; }
int times(int x,int y) { return x*y; }
int square( int x ) { return times(x,x); }

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

    constexpr auto fourthPower = compose( square, square );
    printf( "3^4 = 9^2 = %d\n", fourthPower(3) );

}
