
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

template< class F, class X > struct LastAccum {
    mutable F f;
    mutable X x;

    LastAccum( F f, X x ) : f(move(f)), x(move(x)) { }

    X operator()( X y ) const {
        std::swap( x, y );
        x = f( y, x );
        return y;
    }
};

template< class F, class X > 
LastAccum<F,X> last_accum( F f, X x ) {
    return LastAccum<F,X>( move(f), move(x) );
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

struct Primes {
    vector<long int> primes;

    Primes() : primes{ 1, 2, 3, 5, 7 } { }

    const vector<long int>& so_far() const {
        return primes; 
    }

    bool prime( long int x ) {
        return none( divisor_of(x), tail_wrap(primes) );
    }

    void add_next_prime() {
        int x = last(so_far()) + 2; // Start at the next odd number.

        while( not prime(x) ) 
            x += 2;

        primes.push_back( x );
    }
} primes;

long int last_prime() {
    return last( primes.primes );
}

int next_prime( const vector<int>& past ) {
    int x = last( past );

    do x += 2;
    while( any( divisor_of(x), tail_wrap(past) ) );
    return x;
}

bool prime( int x ) { return primes.prime(x); }

void problem3() {
    const long int START = 600851475143;
    long int x = START;

    cout << "The largest prime divisor of " << START << flush;

    auto primes = memorize( next_prime, 2, 3 );
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
        concatMap ( 
            []( vector<int> r ) -> vector<int> {
                return filtrate( times(last(r)), palindrome, init(move(r)) );
            },
            drop (
                // We remove the first three values: {} {100}, and {100,101}.
                3,
                inits( enumerate(100,1000) )
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
    cout << foldl( lcm, enumerate(1,20) ) 
         << " is divisible by all numbers 1 thought 20." << endl;
}

void problem6() {
    cout << "The difference between the sum squared and squared sum "
            "of each number between 1 and 100: " << flush;

    vector<int> N = enumerate( 1, 101 );

    using P = float(*)(float,float);
    cout << int( pow( sum(N), 2) - sum( map_to<vector<int>>(rclosure(P(pow),2), N) ) ) 
         << endl;
}

void problem7() {
    cout << "The 1001'st prime number: " << flush;
    cout << *next ( 
        begin( memorize(next_prime,1,2,3) ), 
        10001 - 3 
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


int main() {
    problem1();
    problem2();
    problem3();
    problem4();
    problem5();
    problem6();
    problem7();
    problem8();
}
