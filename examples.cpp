
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

typedef array<float,2> Vec;

constexpr int get_x( const Vec& v ) { return v[0]; } 
constexpr int get_y( const Vec& v ) { return v[1]; }

Vec operator + ( const Vec& a, Vec&& b )
{ 
    return zip_with( plus<float>(), a, move(b) ); 
}

Vec operator + ( Vec&& a, const Vec& b )      { return b + move(a); } 
Vec operator + ( const Vec& a, const Vec& b ) { return a + Vec(b); }


Vec operator * ( Vec&& a, float x )
{
    return map( closure(multiplies<float>(),x), move(a) ); 
}
Vec operator * ( float x, Vec&& a )      { return move(a) * x; }
Vec operator * ( const Vec& a, float x ) { return Vec(a) * x; }
Vec operator * ( float x, const Vec& a ) { return Vec(a) * x; }

Vec operator / ( const Vec& v, float x ) { return v * (1/x); }

unique_ptr<float> sqroot( float x ) {
    return x >= 0 ? Just(sqrt(x)) : Nothing<float>(); 
}

/* x +- y = [x+y,x-y] */
constexpr Vec plus_minus( float x, float y ) { return {{x+y,x-y}}; }

Vec qroot_impl( float a, float b, float root ) {
    return plus_minus( -b, root ) / (2*a);
}

/* quadratic root(a,b,c) = Maybe [x,y] */
unique_ptr<Vec> quadratic_root( float a, float b, float c )
{
    return closet(qroot_impl,a,b) ^ sqroot(b*b - 4*a*c);
}

int five() { return 5; }
constexpr int times(int x,int y) { return x*y; }
auto times_two = closet( times, 2 );
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
    return "'" + string( 1, c ) + "'"; 
}

string show( float x ) {
    char digits[20];
    sprintf( digits, "%.1f", x );
    return digits;
}

string show( string s ) {
    return "\"" + s + "\"";
}

string show( const char* str ) {
    return show( string(str) );
}

template< class S > typename ESeq<S,string>::type show( S s ) {
    string str = "[";
    for( const auto& x : s ) {
        str += show( x ) + ",";
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
template< class X > string show( const unique_ptr<X>& m ) {
    return maybe( string("Nothing"), showJust<X>, m );
}
template< class X > string show( X* m ) {
    return maybe( string("Nothing"), showJust<X>, m );
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

// This is a hard type to deduce, so explicitly define show for it. (Used for
// mconcat example.)
string show( const vector<unique_ptr<vector<int>>>& vmv ) {
    string str = "{";
    for( const auto& mv : vmv )
        str = str + show( mv ) + " ";
    str.back() = '}';
    return str;
}

vector<int> pos_neg( int x ) { return { x, -x }; }

bool even( int x ) { return x % 2 == 0; }

// The typical monadic Maybe example.
unique_ptr<int> addM( const unique_ptr<int>& a, const unique_ptr<int>& b ) {
    return a >>= [&](int x){ 
        return b >>= [x](int y){ 
            return Just(x+y); 
        }; 
    };
};

unique_ptr<int> addM2( const unique_ptr<int>& a, const unique_ptr<int>& b ) {
    return fmap( std::plus<int>(), a, b );
};

void print_xyz( int x, int y, int z ) {
    printf( "%d %d %d\n", x, y, z );
}

int main()
{
    rcloset( print_xyz, 2, 3 )( 1 );

    vector<int> evens = filter( even, vector<int>{1,2,3,4,5,6,7,8} );
    printf( "evens = %s\n", show(evens).c_str() );

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
    printf( "\t5 * 2 = %d\n", bcompose(times, get_x, get_y)(fiveTwo) );
    //         is the same as times(get_x(fiveTwo), get_y(fiveTwo)).

    auto sevens = fiveTwo + twoFive;
    printf( "<5,2> + <2,5> = <%f,%f>\n", sevens[0], sevens[1] );
    auto fourteens = sevens * 2;
    printf( "<7,7> * 2 = <%f,%f>\n", fourteens[0], fourteens[1] );

    printf( "quadratic root of x^2 + 3x - 4 = 0 : %s\n",
            show( quadratic_root(1,3,-4) ).c_str() );
    printf( "quadratic root of x^2 + 4 = 0 : %s\n",
            show( quadratic_root(1,0,4) ).c_str() );

    auto sqrDoublePlus2 = comp( plus_two, times_two, square );
    auto rsqrDoublePlus2 = fcomp( square, times_two, plus_two );
    printf( "3^2 * 2 + 2 = 9 * 2 + 2 = %d\n", sqrDoublePlus2(3) );
    printf( "3^2 * 2 + 2 = 9 * 2 + 2 = %d\n", rsqrDoublePlus2(3) );

    puts( "\naddM a b = do\n\tx <- a\n\ty <- b\n\tx + y" );
    printf( "addM 2 4 = %s\n", 
            show( addM(Just(2), Just(4)) ).c_str() );
    puts( "addM2 a b = (+) <$> a <*> b" );
    printf( "addM2 2 4 = %s\n\n",
            show( addM2(Just(2), Just(4)) ).c_str() );

    printf( "5 * 2 = %d\n", closet(times,5)(2) );
    printf( "5 * 2 = %d\n", closet(times,5,2)() );

    printf( "(+2) <$> (pure 1) (100) = %d\n", fmap(plus_two, pure::pure(1))(100) );
    printf( "\tccomp (+2) (pure 1) $ () = %d\n", ccompose(plus_two,pure::pure(1))() );
    printf( "(+2) <$> (+2) 1 = %d\n", fmap(plus_two, plus_two)(1) );

    printf( "(+2) <$> (Pair 1 2) = %s\n", 
            show( plus_two ^ std::make_pair(1,2) ).c_str() );

    printf( "(+2) <$> [1,2] = %s\n", 
            show( plus_two ^ std::list<int>{1,2} ).c_str() );

    printf( "(+2) <$> (Just 2)  = %s\n", 
            show( plus_two ^ Just(2) ).c_str() );
    printf( "(+2) <$> (Nothing) = %s\n", 
            show( plus_two ^ Nothing<int>() ).c_str() );

    printf( "(+2) <$> (Left \"yawn\") = %s\n", 
            show( plus_two ^ Left<int>("yawn") ).c_str() );
    printf( "(+2) <$> (Right 5)     = %s\n",
            show( plus_two ^ Right<string>(5) ).c_str() );

    puts("");
    printf( "(+) <$> [1,2] <*> [3,4] <*> [5,6] = %s\n",
            show ( 
                fmap( [](int x, int y, int z){return x+y+z;}, 
                      vector<int>{1,2}, vector<int>{3,4}, vector<int>{5,6}) 
            ).c_str() );
    printf( "(+) <$> Pair 1 2 <*> Pair 3 4 = %s\n", 
            show( fmap(std::plus<int>(), std::make_pair(1,2),
                                         std::make_pair(3,4)) ).c_str() );
    puts("");

    vector<int> N = {1,2,3,4,5,6,7,8};
    int n = 5;
    auto equalsN = closure( equal_to<int>(), n );
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
            show( Nothing<int>() || Just(2) ).c_str() );
    printf( "Nothing <|> Nothing  = %s\n", 
            show( Nothing<int>() || Nothing<int>() ).c_str() );

    printf( "Just 1 >> Just \"hya!\" = %s\n",
            show( Just(1) >> Just("hya!") ).c_str() );

    typedef unique_ptr<int>(*mret)(int&&);
    printf( "Just 1 >> Nothing >>= return = %s\n",
            show( Just(1) >> Nothing<int>() >>= mreturn<unique_ptr<int>>() ).c_str() );

    auto qr = closet( quadratic_root, 1, 3 );
    typedef pair<float,float> QR;
    printf( "Just -4  >>= (quadraticRoot 1 3) = %s\n",
            show( Just(-4) >>= qr ).c_str() );
    printf( "Just 400 >>= (quadraticRoot 1 3) = %s\n",
            show( Just(400) >>= qr ).c_str() );
    printf( "return 1 :: Maybe Int = %s\n",
            show( mreturn<unique_ptr<int>>(1) ).c_str() );
    printf( "Just 1 >> fail 'oops' = %s\n",
            show( Just(1) >> mfail<unique_ptr<int>>("oops") ).c_str() );

    printf( "[1,2,3] >> [4,5] = %s\n",
            show( vector<int>{1,2,3} >> vector<int>{4,5} ).c_str() );
    printf( "[] >> [3,4] = %s\n",
            show( vector<int>{} >> vector<int>{3,4} ).c_str() );

    printf( "[1,2,3] >>= (\\x->[x,-x]) = %s\n",
            show( vector<int>{1,2,3} >>= pos_neg ).c_str() );

    printf( "Just [1,2] <> Just [3,4] = %s\n",
            show( mappend(Just(vector<int>{1,2}),Just(vector<int>{3,4})) ).c_str() );
    const auto PSX = make_pair( vector<int>{1}, vector<int>{2} );
    const auto PSY = make_pair( vector<int>{3}, vector<int>{4} );
    printf( "Pair [1] [2] <> Pair [3] [4] = %s\n",
            show( mappend(PSX,PSY) ).c_str() );

    vector<unique_ptr<vector<int>>> vmv;
    vmv.emplace_back( Just(vector<int>{1,2}) ); // Don't use an initializer list
    vmv.emplace_back( Just(vector<int>{3,4}) ); // since Maybe is non-copyable.
    printf( "mconcat %s = %s\n",
            show( vmv ).c_str(), 
            show( mconcat(vmv) ).c_str() );

    printf( "Just 1 `mplus` Just 2 = %s\n",
            show( mplus(Just(1),Just(2)) ).c_str() );
    printf( "[1] `mplus` [2] = %s\n",
            show( mplus(vector<int>{1},vector<int>{2}) ).c_str() );

    pair<int,int> p( 1, 2 );
    using Show = string(int);
    auto showInt = [](int x){ return show(x); };
    printf( "first show >>> second (+2) $ (1,2) = %s\n", 
            show( comp(first(showInt), second(plus_two))( p ) ).c_str() );
    printf( "show *** (+2) $ (1,2) = %s\n",
            show( split( showInt, plus_two )( p ) ).c_str() );
}
