
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

constexpr int get_x( const Vec& v ) { return v[0]; } 
constexpr int get_y( const Vec& v ) { return v[1]; }

Vec operator + ( const Vec& a, Vec&& b )
{ 
    return zip_with( plus<int>(), a, move(b) ); 
}

Vec operator + ( Vec&& a, const Vec& b )      { return b + move(a); } 
Vec operator + ( const Vec& a, const Vec& b ) { return a + Vec(b); }


Vec operator * ( Vec&& a, int x )
{
    return map( partial(multiplies<int>(),x), move(a) ); 
}
Vec operator * ( int x, Vec&& a )      { return move(a) * x; }
Vec operator * ( const Vec& a, int x ) { return Vec(a) * x; }
Vec operator * ( int x, const Vec& a ) { return Vec(a) * x; }

/* Complete the quadratic equation with a pre-calculated square root. */
constexpr float _qroot( float a, float b, float root )
{
    return (-b + root) / (2 * a);
}

constexpr pair<float,float> _split_qroot( float a, float b,float root )
{
    return make_pair( _qroot(a,b,root), _qroot(a,b,-root) );
}

/* 
 * quadratic root(a,b,c) = Maybe [x,y]
 * pure:: functions prefer to work with sequences, so this returns an array
 * containing just the two possible values of x.
 */
Maybe<pair<float,float>> quadratic_root( float a, float b, float c )
{
    // Return split(a,b,root) 
    return Just( partial(_split_qroot, a, b) ) * 
        // IFF root>=0; else Nothing.
        (b*b >= 4*a*c ? Just( sqrt(b*b-4*a*c) ) : Nothing<float>());
}

int five() { return 5; }
constexpr int times(int x,int y) { return x*y; }
auto times_two = partial( times, 2 );
constexpr int plus_two(int x) { return x + 2; }

// squash(f) returns a functor that duplicates its first argument.
constexpr auto square = squash( times );
// square(x) = times(x,x)

float to_float( int x ) { return x; }

string show( int x ) {
    char digits[20];
    sprintf( digits, "%d", x );
    return digits;
}

string show( char c ) {
    return string( 1, c );
}

string show( float x ) {
    char digits[20];
    sprintf( digits, "%.1f", x );
    return digits;
}

string show( string s ) {
    return "\"" + s + "\"";
}

template< class S > typename ESeq<S,string>::type show( S s ) {
    string str = "[";
    for( const auto& x : s ) {
        str += show( x ) + " ";
    }
    if( str.size() > 1 )
        str.back() = ']';
    else 
        str.push_back( ']' );

    return str;
}

template< class X, class Y > string show( const pair<X,Y>& p ) {
    return "Pair (" + show(p.first) + ") (" + show(p.second) + ")";
}

template< class X > string showJust( const X& x );
template< class T > string show( const Maybe<T>& m ) {
    typedef typename Maybe<T>::value_type V;
    return maybe( string("Nothing"), showJust<V>, m );
}

template< class X > string showJust( const X& x ) {
    return "Just (" + show( x ) + ")";
}

template< class R > string showRight( const R& r ) 
{ return "Right (" + show( r ) + ")"; }
template< class L > string showLeft( const L& l ) 
{ return "Left  (" + show( l ) + ")"; }

template< class L, class R > 
string show( const Either<L,R>& e ) {
    typedef typename Either<L,R>::right_type RT;
    typedef typename Either<L,R>::left_type LT;
    return either( showRight<RT>, showLeft<LT>, e );
}

int main()
{
    printf( "3^2 = %d\n", square(3) );

    printf (
        "sum of (1,2,3,4) = %d\n", // = 10
        foldl<int>( std::plus<int>(), vector<int>{1,2,3,4} )
    );
    printf(
        "sum of (4,3,2,1) = %d\n", // = 10
        foldr<int>( std::plus<int>(), vector<int>{1,2,3,4} )
    );

    Vec fiveTwo = {{5,2}}, twoFive = {{2,5}};

    // Join lets us adapt an N-ary function to an (N-1) one.
    printf( "\t5 * 2 = %d\n", join(times, get_x, get_y)(fiveTwo) );
    // is the same as times(get_x(fiveTwo),get_y(fiveTwo)).

    auto sevens = fiveTwo + twoFive;
    printf( "<5,2> + <2,5> = <%d,%d>\n", sevens[0], sevens[1] );
    auto fourteens = sevens * 2;
    printf( "<7,7> * 2 = <%d,%d>\n", fourteens[0], fourteens[1] );

    auto roots = quadratic_root( 1, 3, -4 );
    printf( "quadratic root of x^2 + 3x - 4 = 0 : %s\n",
            show( quadratic_root(1,3,-4) ).c_str() );
    printf( "quadratic root of x^2 + 4 = 0 : %s\n",
            show( quadratic_root(1,0,4) ).c_str() );

    constexpr auto sqrDoublePlus2 = compose( plus_two, times_two, square );
    printf( "3^2 * 2 + 2 = 9 * 2 + 2 = %d\n", sqrDoublePlus2(3) );

    printf( "5 * 2 = %d\n", partial(times,5)(2) );
    printf( "5 * 2 = %d\n", partial(times,5,2)() );

    printf( "fmap (+2) (pure 1) = %d\n", fmap(plus_two,pure::pure(1))() );
    printf( "fmap (+2) (+2) 1   = %d\n", fmap(plus_two,plus_two)(1) );

    printf( "fmap (+2) (Pair 1 2) = %s\n", 
            show( fmap(plus_two, std::make_pair(1,2)) ).c_str() );

    printf( "fmap (+2) [1,2] = %s\n", 
            show( fmap(plus_two, std::list<int>{1,2}) ).c_str() );

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

    printf( "Just 1 >> Just \"hya!\" = %s\n",
            show( Just(1) >> Just("hya!") ).c_str() );
    printf( "Just 1 >> Nothing = %s\n",
            show( Just(1) >> Nothing<int>() >>= mreturn<Maybe,int> ).c_str() );

    auto qr = partial( quadratic_root, 1, 3 );
    typedef pair<float,float> QR;
    printf( "Just -4  >>= (quadraticRoot 1 3) = %s\n",
            show( Just(-4) >>= qr ).c_str() );
    printf( "Just 400 >>= (quadraticRoot 1 3) = %s\n",
            show( Just(400) >>= qr ).c_str() );
    printf( "return 1 :: Maybe Int = %s\n",
            show( mreturn<Maybe>(1) ).c_str() );
    printf( "Just 1 >> fail 'oops' = %s\n",
            show( Just(1) >> mfail<Maybe,int>("oops") ).c_str() );
}
