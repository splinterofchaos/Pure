
#include "../Pure.h"

using namespace pure;

#include <iostream>

using std::cout;
using std::endl;

bool less_equal_1000( int x ) {
    return x <= 10;
}

vector<int> multiples_less_than_1000( int x ) {
    // TODO: This causes an infinite loop. Why?
    //return take_while (
    //    less_equal_1000,
    //    iterate( closet(std::plus<int>(),x), x )
    //);
    return take (
        999 / x, // x goes into 1000 (999/x) times.
        iterate( closet(std::plus<int>(),x), x )
    );
}

void problem1() {
    cout << "The sum of every multiple of 3 or 5 between 1 and 1000: "
         << sum ( sunion( multiples_less_than_1000(3), 
                          multiples_less_than_1000(5) ) )
         << endl;
}

template< class F, class X > struct LastAccum {
    F f;
    X x;

    constexpr LastAccum( F f, X x ) : f(move(f)), x(move(x)) { }

    X operator()( X y ) {
        X tmp = x;
        X r = f( x, y );
        x = r;
        return tmp;
    }
};

template< class F, class X > 
constexpr LastAccum<F,X> last_accum( F f, X x ) {
    return LastAccum<F,X>( move(f), move(x) );
}

constexpr auto even = fnot( rcloset( std::modulus<int>(), 2 ) );

void problem2() {
    cout << "The sum of every even Fibonacci number below 4-million: "
         << sum ( 
             filter ( 
                 even,
                 take_while (
                     rclosure( std::less<int>(), 4000000 ),
                     // iterate and last_accum, together, 
                     // give us the two-value accumulator we need.
                     iterate( last_accum(std::plus<int>(),2), 1 ) 
                 )
             )
         ) << endl;
}

void problem3() {

}

int main() {
    problem1();
    problem2();
    problem3();
}
