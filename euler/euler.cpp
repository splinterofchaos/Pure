
#include "../Pure.h"

using namespace pure;

#include <iostream>

using std::cout;
using std::endl;
using std::flush;

std::ostream& operator<< ( std::ostream& os, const std::vector<int>& v ) {
    std::copy( begin(v), end(v), std::ostream_iterator<int>(os," ") );
    return os;
}

vector<int> multiples_less_than_1000( int x ) {
    return take (
        999 / x, // x goes into 1000 (999/x) times.
        iterate( closet(Add(),x), x )
    );
}

void problem1() {
    cout << "The sum of every multiple of 3 or 5 between 1 and 1000: "
         << sum( sunion( multiples_less_than_1000(3), 
                         multiples_less_than_1000(5) ) )
         << endl;
}

template< class F, class X > struct LastAccum {
    F f;
    X x;

    constexpr LastAccum( F f, X x ) : f(move(f)), x(move(x)) { }

    X operator()( X y ) {
        std::swap( x, y );
        x = f( y, x );
        return y;
    }
};

template< class F, class X > 
constexpr LastAccum<F,X> last_accum( F f, X x ) {
    return LastAccum<F,X>( move(f), move(x) );
}

constexpr auto even = fnot( rcloset( Mod(), 2 ) );

void problem2() {
    cout << "The sum of every even Fibonacci number below 4-million: "
         << sum ( 
             filter ( 
                 even,
                 take_while (
                     less_than( 4000000 ),
                     // iterate and last_accum, together, 
                     // give us the two-value accumulator we need.
                     iterate( last_accum(Add(),2), 1 ) 
                 )
             )
         ) << endl;
}

struct Primes {
    vector<long int> primes;

    Primes() : primes{ 2, 3, 5, 7 } { }

    const vector<long int>& so_far() const {
        return primes; 
    }

    bool prime( long int x ) {
        return none( divisor_of(x), primes );
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

bool prime( int x ) { return primes.prime(x); }

void problem3() {
    const long int START = 600851475143;
    long int x = 600851475143;

    cout << "The largest prime divisor of " << START << flush;
    while( last_prime() < std::sqrt(x) ) {
        primes.add_next_prime();
        if( x % last_prime() == 0 )
            x /= last_prime();
    }
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

bool palindrome( const vector<int>& v ) {
    return equal( v, reverse_wrap(v) );
}

bool palindrome( int x ) {
    return palindrome( digits(x) );
}
    
auto three_digits = take_while( less_than(1000), iterate(inc<int>,100) );

using ThreeDS = decltype(three_digits);

void problem4() {
    vector<int> maxes;
    for( auto r = reverse_wrap( three_digits ); 
         not_null(r); r = tail_wrap(r) )
    {
        using ItoB = bool(*)(int);
        auto t = tail(r); // TODO: If this line is not separate, gcc fails to
                          //       compile. Why?
        maxes = append( move(maxes), 
                        filter( (ItoB)palindrome, 
                                map(times(head(r)), t) ) );
    }
    cout << "The largest product of three digit numbers "
            "making a palindrome : " << maximum(maxes) << endl;
}

int main() {
    problem1();
    problem2();
    problem3();
    problem4();
}