
#include "../Pure.h"

using namespace pure;

#include <iostream>

using std::cout;
using std::endl;
using std::flush;

template< class X >
std::ostream& operator<< ( std::ostream& os, const std::vector<X>& v ) {
    std::copy( begin(v), end(v), std::ostream_iterator<X>(os," ") );
    return os;
}

vector<int> multiples_less_than_1000( int x ) {
    return take (
        999 / x, // x goes into 1000 (999/x) times.
        iterate( pure::plus(x), x )
    );
}

void problem1() {
    cout << "The sum of every multiple of 3 or 5 between 1 and 1000: "
         << flush <<
         sum( sunion( multiples_less_than_1000(3), 
                      multiples_less_than_1000(5) ) )
         << endl;
}

constexpr auto even = fnot( rcloset( Mod(), 2 ) );

void problem2() {
    cout << "The sum of every even Fibonacci number below 4-million: "
         << flush << 
         sum ( 
             filter ( 
                 even,
                 take_while (
                     less_than( 4000000 ),
                     bi_iterate( Add(), 1, 2 )
                 )
             )
         ) << endl;
}

using PrimeType = unsigned long long int;

PrimeType next_prime( const vector<PrimeType>& past ) {
    int x = last( past );

    do x += 2;
    while( any( divisor_of(x), tail_wrap(past) ) );
    return x;
}

auto primes = memorize( next_prime, 2ull, 3ull );

void problem3() {
    const long int START = 600851475143;
    long int x = START;

    cout << "The largest prime divisor of " << START << flush;

    auto p = begin( primes );

    while( *p < std::sqrt(x) )
        if( x % *p++ == 0 )
            x /= *prev(p);

    cout << " is " << x << endl;
}

#include <cmath>

vector<int> digits( int x ) {
    // TODO: Perhaps this would be a good test case for implementing mapAccum.
    vector<int> ds;
    for( ; x > 0; x = x / 10 )
        ds.push_back( x % 10 );
    return ds;
}

bool _palindrome( const vector<int>& v ) {
    return equal( v, reverse_wrap(v) );
}

bool palindrome( int x ) {
    return _palindrome( digits(x) );
}

auto N = enumerate(0);

void problem4() {
    cout << "The largest palindrome product of three digit numbers :"
         << flush;
    cout << maximum ( 
        map ( 
            []( const IRange& r ) -> unsigned int {
                auto ps = filter( palindrome, 
                                  map_to<vector<unsigned int>>(times(last(r)), 
                                                      init(r)) );
                return not_null(ps) ? maximum(ps) : 0;
            },
            drop (
                // We remove the first three values: {} {100}, and {100,101}.
                3,
                inits( enumerate(100,999) )
            )
        )
    ) << endl;
}

int gcm( int x, int y ) {
    while(true) {
        if( not x ) return y;
        y %= x;
        if( not y ) return x;
        x %= y;
    }
}

int lcm( int x, int y ) {
    return x * y / gcm(x,y);
}

void problem5() {
    cout << foldl( lcm, enumerate(2,19) ) 
         << " is divisible by all numbers 1 thought 20." << endl;
}

void problem6() {
    cout << "The difference between the sum squared and squared sum "
            "of each number between 1 and 100: " << flush;

    constexpr auto N = enumerate( 1, 100 );

    // A sum on an XRange (enumerate's return type) is a constexpr!
    constexpr auto sqrOfSum = pow( sum(N), 2.f );

    using P = float(*)(float,float);
    cout << int ( 
        sqrOfSum - sum ( 
            map_to<vector<int>>( rclosure(P(pow), 2), N ) 
        ) 
    ) << endl;
}

void problem7() {
    cout << "The 1001'st prime number: " << flush;
    cout << *next ( 
        begin( primes ), 
        10000 
    ) << endl;
}

int from_sym( char sym ) { return sym - '0'; }

void problem8() {
    cout << "The largest sum of five numbers in the given sequence: " << flush;

    const string nsStr = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450";

    cout << maximum (
        map ( 
            []( const vector<int>& v ) { return product( take(5,v) ); },
            tails( map_to<vector<int>>(from_sym,nsStr) )
        ) 
    ) << endl;
}

constexpr bool triplet( int a, int b, int c ) {
    return a < b and b < c and a*a + b*b == c*c;
}

void problem9() {
    cout << "The triplet, (" << flush;

    unsigned int a = 4, b = 5;
    auto c = [&]() { return std::hypot(a,b); };

    while( a+b+c() != 1000 ) {
        a++;
        for( b = a+1; a + b + c() < 1000; b++ ) { 
        }
    }

    cout << a << ',' << b << ',' << c()
         << "), equals " << a+b+c() << " when added and " 
         << int(a*b*c()) << " when multiplied. " << endl;
}

void problem10() {
    // TODO: This is too slow, too brute force, and WRONG!
    cout << "The sum of all primes below 4 million is: " << flush;
    constexpr auto s = sum( enumerate(5,10) );
    cout << s;
    //cout << sum ( 
    //    take_while( less_than(4000000ull), memorize(next_prime,2ull,3ull) )
    //) << endl;
    
    //unsigned long long sum = 0;
    //auto primes = memorize( next_prime, 2ull, 3ull );
    //auto p = begin( primes );
    //while( *p < PrimeType(4000000ull) ) 
    //    sum += *p++;
    //cout << sum << endl;

    cout << endl;
}

int main() {
    problem1();
    problem2();
    problem3();
    problem4();
    problem5();
    problem6();
    problem7();
    problem8();
    problem9();
    problem10(); 
}
