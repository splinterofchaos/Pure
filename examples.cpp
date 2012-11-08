
#include "Pure.h"
#include "Fold.h"
#include "Arrow.h"
#include "State.h"
#include "Applicative.h"
#include "Set.h"

#include <cstdio>
#include <cmath>
#include <vector>
#include <array>
#include <list>
#include <set>
#include <cctype>

#include <iostream>
#include <fstream>

using namespace std;
using namespace pure;
using namespace pure::list;

template< typename Container >
void print( const char* const msg, const Container& v )
{
    cout << msg << "\n";
    copy( begin(v), end(v), ostream_iterator<float>(cout, " ") );
    cout << endl;
}

typedef array<float,2> Vec;

constexpr int get_x( const Vec& v ) { return v[0]; } 
constexpr int get_y( const Vec& v ) { return v[1]; }

Vec operator + ( const Vec& a, const Vec& b )
{ 
    return zipWith( Add(), a, b ); 
}


Vec operator * ( Vec a, float x )
{
    return map( closure(Mult(),x), a ); 
}
Vec operator * ( float x, Vec a ) { return move(a) * x; }

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
constexpr auto times_two = mult(2);
constexpr int plus_two(int x) { return x + 2; }

// squash(f) returns a functor that duplicates its first argument.
constexpr auto square = squash( Mult() );
// square(x) = x * x

float to_float( int x ) { return x; }

string show( bool );
string show( int  );
string show( unsigned int );
string show( unsigned long int );
string show( unsigned long long );
string show( char );
string show( const char* );
string show( string );
string show( float );
template< class X, class Y > string show( const pair<X,Y>& p );
template< class S > auto show( const S& s ) -> decltype( begin(s), string() );
template< class X > string showJust( const X& );
template< class X > string show( const unique_ptr<X>& );
template< class X > string show( X* );

string show( bool b ) {
    return b ? "True" : "False";
}

string show( int x ) {
    char digits[20];
    sprintf( digits, "%d", x );
    return digits;
}

string show( unsigned int x ) {
    char digits[20];
    sprintf( digits, "%u", x );
    return digits;
}

string show( unsigned long int x ){
    char digits[20];
    sprintf( digits, "%lu", x );
    return digits;
}

string show( unsigned long long x ) {
    char digits[20];
    sprintf( digits, "%llu", x );
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

template< class S > 
auto show( const S& s ) -> decltype( begin(s), string() )
{
    string str = "[";
    for( const auto& x : s ) {
        str += show( x ) + ",";
    }
    if( length(str) > 1 )
        str.back() = ']';
    else 
        str.push_back( ']' );

    return str;
}

template< class X, class Y > string show( const pair<X,Y>& p ) {
    return "(Pair: " + show(p.first) + ", " + show(p.second) + ")";
}

template< class X > string showJust( const X& x );
template< class M > string showMaybe( const M& m ) {
    using X = decltype( *declval<M>() );
    return maybe( string("Nothing"), showJust<X>, m );
}

template< class X > string show( const unique_ptr<X>& m ) {
    return showMaybe( m );
}
template< class X > string show( X* m ) {
    return showMaybe( m );
}

template< class X > string showJust( const X& x ) {
    return "(Just " + show( x ) + ")";
}

template< class R > string showRight( const R& r ) 
{ return "(Right " + show( r ) + ")"; }
template< class L > string showLeft( const L& l ) 
{ return "(Left  " + show( l ) + ")"; }

template< class L, class R > 
string show( const Either<L,R>& e ) {
    typedef typename Either<L,R>::right_type RT;
    typedef typename Either<L,R>::left_type LT;
    return either( showRight<RT>, showLeft<LT>, e );
}

// This is a hard type to deduce, so explicitly define show for it. (Used for
// mconcat example.)
template< class X >
string show( const vector<X>& vmv ) {
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
    using namespace pure::monad; // Import >>= overload.
    return a >>= [&](int x){ 
        return b >>= [x](int y){ 
            return Just(x+y); 
        }; 
    };
};

unique_ptr<int> addM2( const unique_ptr<int>& a, const unique_ptr<int>& b ) {
    return fmap( add, a, b );
};

void print_xyz( int x, int y, int z ) {
    printf( "%d %d %d\n", x, y, z );
}

int main()
{
    rcloset( print_xyz, 2, 3 )( 1 );

    auto evens = ( DupTo<std::set>() ^ filter(even) )( enumerate(1,12) );
    printf( "es = filter even [1..12] = %s\n", show(evens).c_str() );
    printf( "\tnull es = %s\n", show( null(evens) ).c_str() );
    printf( "\tlength es = %lu\n", length(evens) );
    printf( "\thead es = %d\n\tlast es = %d\n", head(evens), last(evens) );
    printf( "\ttail es = %s\n\tinit es = %s\n",
            show( tail(evens) ).c_str(), show( init(evens) ).c_str() );
    printf( "\tinits es = %s\n", show( inits(evens) ).c_str() );
    printf( "\treverse es = %s\n", show( reverse(evens) ).c_str() );
    printf( "\tisPrefixOf [2,4] es = %s\n", show( prefix({2,4},evens) ).c_str() );
    printf( "\telem 2 es = %s\n",   show( elem(  2,evens) ).c_str() );
    printf( "\tdelete 4 es = %s\n", show( erase( 4,evens) ).c_str() );
    printf( "\tinsert 5 es = %s\n", show( insert(5,evens) ).c_str() );
    printf( "\tdeleteFirstBy (==) es [2,6] = %s\n", 
            show( eraseFirst( equal_to<int>(),evens,vector<int>{1,2,6} ) ).c_str() );
    printf( "\tpermutations (take 3 es) = %s\n", 
            show ( 
                ( permutations ^ DupTo<std::vector>() ^ take(3) )( evens ) 
            ).c_str() );
    printf( "\tscanl (+) %s = %s\n", 
            show( evens ).c_str(), show( scanl( Add(), evens ) ).c_str() );
    printf( "\tscanr (+) %s = %s\n", 
            show( evens ).c_str(), show( scanr( Add(), evens ) ).c_str() );
    {
        using namespace pure::set::ordered;
        printf( "\npure::set :\n"
                "es contains 2 = %s\n", show(2<evens).c_str() );
        printf( "The length of es = %s\n", show(+evens).c_str() );
        printf( "[6,2] is a subset of es: %s\n", 
                show( S(6,2) <= evens ).c_str() );
        printf( "union of es and [3,7,10] = %s\n",
                show( S(3,7,10) | evens ).c_str() );
        printf( "intersection of es and [4,7,10] = %s\n",
                show( S(4,7,10) % evens ).c_str() );
        printf( "es without [2,8,10] = %s\n",
                show( evens / S(2,8,10) ).c_str() );

        printf( "['a','b'] * evens = %s\n",
                show( S('a','b') * S(1,2) ).c_str() );
    }
    puts("");

    printf( "intersparse ',' \"abcd\" = %s\n",
            intersparse( ',', string("abcd") ).c_str() );
    printf( "intercalcate \"--\" {\"ab\",\"cd\",\"ef\"} = %s\n",
             intercalcate ( 
                 string("--"),
                 vector<string>{"ab","cd","ef"}
             ).c_str() );

    puts("");
    printf( "take 10 $ iterate (+2) 1 = %s\n",
            show( take( 10, iterate(plus_two,1) ) ).c_str() );
    printf( "replicate 10 1 = %s\n",
            show( replicate(10, 1) ).c_str() );
    puts("");

    printf( "break even [1..8] = %s\n",
            show( sbreak(even,vector<int>{1,2,3,4,5,6,7,8}) ).c_str() );
    printf( "goup \"footoonopor\" = %s\n",
            show( group(string("footoonopor")) ).c_str() );
    printf( "elemIndecies 'o' \"footoonopor\" = %s\n",
            show( elemIndecies('o',string("footoonopor")) ).c_str() );
    printf( "nub \"footoonopor\" = %s\n",
            show( nub(string("footoonopor")) ).c_str() );
    printf( "\"footo\" `union` \"onopor\" = %s\n",
            show( sunion(string("footo"),string("onopor")) ).c_str() );
    printf( "\"footo\" // \"onopor\" = %s\n",
            show( difference(string("footo"),string("onopor")) ).c_str() );
    printf( "\"footo\" `intersect` \"onopor\" = %s\n",
            show( intersect(string("footo"),string("onopor")) ).c_str() );
    printf( "intersectBy (<) \"footo\" \"onopor\" = %s\n",
            show( intersectIf(std::greater<char>(),string("footo"),string("onopor")) ).c_str() );

    // isspace is a macro, so we need to wrap it to pass it.
    auto is_space = [](char c){ return isspace(c); };
    printf( "dropWhile isspace \" \\tfoo\" = \"%s\"\n",
            dropWhile( is_space, string(" \tfoo") ).c_str() );
    printf( "dropWhileEnd isspace \"foo\\n\" = %s\n",
            show( dropWhileEnd(is_space, string("foo\n")) ).c_str() );

    puts("");

    printf( "3^2 = %d\n", square(3) );

    printf (
        "sum of (1,2,3,4) = %d\n", // = 10
        foldl( Add(), {1,2,3,4} )
    );
    printf(
        "sum of (4,3,2,1) = %d\n", // = 10
        foldr( Add(), {1,2,3,4} )
    );

    auto accumF = []( int x, int y ) { return std::make_pair(x+y,x*y); };
    printf( "mapAccumL (\\x y -> (x+y,x*y)) 0 [2,8,10] = %s\n",
            show( mapAccumL(accumF,0,std::vector<int>{2,8,10}) ).c_str() );
    printf( "mapAccumR (\\x y -> (x+y,x*y)) 0 [2,8,10] = %s\n",
            show( mapAccumR(accumF,0,std::vector<int>{2,8,10}) ).c_str() );

    Vec fiveTwo = {{5,2}}, twoFive = {{2,5}};

    // Join lets us adapt an N-ary function to an (N-1) one.
    printf( "\t5 * 2 = %d\n", bcompose(Mult(), get_x, get_y)(fiveTwo) );
    //         is the same as times(get_x(fiveTwo), get_y(fiveTwo)).

    auto sevens = fiveTwo + twoFive;
    printf( "<5,2> + <2,5> = <%f,%f>\n", sevens[0], sevens[1] );
    auto fourteens = sevens * 2;
    printf( "<7,7> * 2 = <%f,%f>\n", fourteens[0], fourteens[1] );

    printf( "quadratic root of x^2 + 3x - 4 = 0 : %s\n",
            show( quadratic_root(1,3,-4) ).c_str() );
    printf( "quadratic root of x^2 + 4 = 0 : %s\n",
            show( quadratic_root(1,0,4) ).c_str() );

    auto sqrDoublePlus2 = arrow::comp( plus_two, times_two, square );
    auto rsqrDoublePlus2 = arrow::fcomp( square, times_two, plus_two );
    printf( "3^2 * 2 + 2 = 9 * 2 + 2 = %d\n", sqrDoublePlus2(3) );
    printf( "3^2 * 2 + 2 = 9 * 2 + 2 = %d\n", rsqrDoublePlus2(3) );

    puts( "\naddM a b = do\n\tx <- a\n\ty <- b\n\tx + y" );
    printf( "addM (Just 2) (Just 4) = %s\n", 
            show( addM(Just(2), Just(4)) ).c_str() );
    puts( "addM2 a b = (+) <$> a <*> b" );
    printf( "addM2 (Just 2) (Just 4) = %s\n\n",
            show( addM2(Just(2), Just(4)) ).c_str() );

    printf( "5 * 2 = %d\n", closet(Mult(),5)(2) );
    printf( "5 * 2 = %d\n", closet(Mult(),5,2)() );

    puts("");
    puts("p2M = fmap (+2)");
    auto plus_twoM = fmap( plus_two );
    printf( "\tp2M (1,2) = %s\n\tp2M [1,2] = %s\n",
            show( plus_twoM(make_pair(1,2)) ).c_str(),
            show( plus_twoM(std::list<int>{1,2}) ).c_str() );
    puts("");

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
    printf( "\t(+) <$> Pair 1 2 <*> Pair 3 4 = %s\n", 
            show( fmap(add, std::make_pair(1,2),
                            std::make_pair(3,4)) ).c_str() );

    puts("");
    puts("add3 x y z = x + y + z");
    puts("add3M a b c = add3 <$> a <*> b <*> c");
    auto add3 = [](int x, int y, int z){ return x+y+z; };
    auto add3M = fmap( add3 );
    printf( "\tadd3M (Just 1) (Just 2) (Just 3) = %s\n",
            show( add3M( Just(1), Just(2), Just(3) ) ).c_str() );
    printf( "\tadd3M [1,2] [3,4] [5,6] = %s\n",
            show( add3M(vector<int>{1,2}, vector<int>{3,4}, 
                       vector<int>{5,6}) ).c_str() );
    using V = vector<int>;
    printf( "\tzip_with add3 [1,2] [3,4] [5,6] = %s\n",
            show( zipWith(add3,V{1,2},V{3,4},V{5,6}) ).c_str() );
    printf( "\tfold add3 10 [1,2] [3,4] = %s\n",
            show( foldl(add3,10,V{1,2},V{3,4}) ).c_str() );
    puts("");

    vector<int> N = {1,2,3,4,5,6,7,8};
    int n = 5;
    auto equalsN = closure( equal_to<int>(), n );
    printf( "find (==5) [1,2,3,4,5,6,7,8] = %s\n", 
            showMaybe( find(equalsN, N) ).c_str() );
    n = 9;
    printf( "find (==9) [1,2,3,4,5,6,7,8] = %s\n", 
            showMaybe( find(equalsN, N) ).c_str() );

    {
        puts("");

        // Bring in the operator overloads * (ap) and || (alt).
        using namespace pure::ap;

        puts("");
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

        std::vector<Part<Add,int>> fs = { add(1), add(5), add(3) };
        puts( "fs = [(+1),(+5),(+3)]" );
        printf( "\tfs <*> pure 1 = %s\n",
                show( fs*apure<std::vector>(1) ).c_str() );
        printf( "[1,2,3] <|> [4] <|> empty = %s\n",
                show( std::vector<int>{1,2,3} || std::vector<int>{4}
                      || empty<std::vector<int>>() ).c_str() );
        printf( "pure 5 :: [] = %s\n",
                show( pure::ap::pure<std::vector>(5) ).c_str() );
        printf( "([1,2],(+2)) <*> ([3,4],5) = %s\n",
                show( std::make_pair(std::vector<int>{1,2}, plus_two)
                      * std::make_pair(std::vector<int>{3,4},5) ).c_str() );
    }

    puts("");
    {
        using namespace pure::monad;

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
                show( mreturn<unique_ptr>(1) ).c_str() );
        printf( "Just 1 >> fail 'oops' = %s\n",
                show( Just(1) >> mfail<unique_ptr<int>>("oops") ).c_str() );

        printf( "[1,2,3] >> [4,5] = %s\n",
                show( vector<int>{1,2,3} >> vector<int>{4,5} ).c_str() );
        printf( "[] >> [3,4] = %s\n",
                show( vector<int>{} >> vector<int>{3,4} ).c_str() );

        printf( "[1,2,3] >>= (\\x->[x,-x]) = %s\n",
                show( vector<int>{1,2,3} >>= pos_neg ).c_str() );


        std::vector<std::unique_ptr<int>> v;
        v.emplace_back(Just(1));
        v.emplace_back(Just(2));
        v.emplace_back(Just(3));
        printf( "sequence [Just 1, Just 2, Just 3] = %s\n",
                show( sequence(v) ).c_str() );

        std::vector<std::vector<int>> vv = { {1,2,3}, {4,5}, {6,7} };
        printf( "sequence [[1,2,3],[4,5],[6,7]] = %s\n",
                        show( sequence(vv) ).c_str() );

    }

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

    {
        using namespace pure::arrow;

        pair<int,int> p( 1, 2 );
        using Show = string(int);
        auto showInt = [](int x){ return show(x); };
        printf( "first show >>> second (+2) $ (1,2) = %s\n", 
                show( (first(showInt) > second(plus_two))( p ) ).c_str() );
        printf( "show *** (+2) $ (1,2) = %s\n",
                show( (showInt * plus_two)( p ) ).c_str() );

        printf( "show &&& (+2) $ 5 = %s\n",
                show( (showInt && plus_two)( 5 ) ).c_str() );

        constexpr auto plusTwoK = arr<Kleisli<std::unique_ptr>>(
            plus_two
        );

        auto subTwoK = arr<Kleisli<std::unique_ptr>>(
            []( int x ) { return x - 2; }
        );

        using namespace category;
        constexpr auto plusFourK = plusTwoK > plusTwoK;

        printf( "plusFourK 10 = %s\n",
                show( plusFourK(10) ).c_str() );
        printf( "(plusTwoK &&& subTwoK) 0 = %s\n",
                show( (plusTwoK && subTwoK)(0) ).c_str() );

    }

    puts("");
    {
        using namespace pure::monad;
        using namespace pure::state;

        auto s = state::returnState<int>(10);
        puts("let s = return 10 :: State Int Int");
        printf( "runState  s 5 = %s\n", show( run( s,5).get() ).c_str() );
        printf( "evalState s 5 = %s\n", show( eval(s,5).get() ).c_str() );
        printf( "execState s 5 = %s\n", show( exec(s,5).get() ).c_str() );
        printf( "runState (fmap (\\x->x*2) s) 10 = %s\n",
                show( fmap(times_two, s).runState(10).get() ).c_str() );

        printf( "runState (s >>= (\\x->return x)) 10 = %s\n",
                show( (s >>= state::Return<int>()).runState(10).get() ).c_str() );

        auto tick = get<int>() >>= []( int x ){
            return put( x+1 ) >>= state::Return<int>();
        };
        puts("tick = do\n\tn <- get\n\tput (n+1)\n\treturn n");
        printf( "execState tick 5 = %s\n",
                show( exec( tick, 5 ).get() ).c_str() );

        puts("tick2 = modify (+2) >> get");
        auto tick2 = modify<int>( plus_two ) >> state::get<int>();
        printf( "execState tick2 5 = %s\n",
                show( exec( tick2, 5 ).get() ).c_str() );

        printf( "runState (gets (+2)) 5 = %s\n",
                show( gets<int>(plus_two).runState(5).get() ).c_str() );
    }
}

