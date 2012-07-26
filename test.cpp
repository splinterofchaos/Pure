
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

template< class T >
T identity( T x )
{
    return x;
}

std::array<float,2> quadratic_root( float a, float b, float c )
{
    return map (
        [&](float root){ return (-b + root) / (2*a); },
        cleave( sqrt(b*b - 4*a*c), identity<float>, negate<float>() )
    );
}

int five() { return 5; }
int times_two(int x) { return x * 2; }

int main()
{
    // Fold left and right work.
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

    auto roots = quadratic_root( 1, 3, -4 );
    printf (
        "quadratic root of x^2 + 3x - 4 = 0 : %f or %f\n",
        roots[0], roots[1]
    );

}
