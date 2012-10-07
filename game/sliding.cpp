
#include "../Pure.h"
#include "../List.h"

#include <array>
#include <algorithm>
#include <utility>
#include <cmath>
#include <iostream>

using Row = std::array<unsigned int,3>;
using Board = std::array< Row, 3>;

unsigned int& at( size_t x, size_t y, Board& b ) {
    return b[y][x];
}

const unsigned int& at( size_t x, size_t y, const Board& b ) {
    return b[y][x];
}

struct Vec { int x, y; };

unsigned int& at( const Vec& v, Board& b ) {
    return at( v.x, v.y, b );
}

const unsigned int& at( const Vec& v, const Board& b ) {
    return at( v.x, v.y, b );
}

std::ostream& operator << ( std::ostream& os, const Board& b ) {
    os << "+---+\n";
    pure::list::vmap (
        [&]( const Row& r ) {
            os << "|" << r[0] << r[1] << r[2] << "|\n";
        }, b
    );
    os << "+---+";
    return os;
}

Board GOAL = {{ {{0, 1, 2}},
                {{3, 4, 5}},
                {{6, 7, 8}} }};

bool goalState( const Board& b ) {
    return b == GOAL;
}


constexpr Vec operator + ( const Vec& a, const Vec& b ) {
    return { a.x + b.x, a.y + b.y };
}
constexpr Vec operator - ( const Vec& a, const Vec& b ) {
    return { a.x - b.x, a.y - b.y };
}

constexpr unsigned int manhattan( const Vec& a, const Vec& b ) {
    return std::abs(a.x-b.x) + std::abs(a.y-b.y);
}

Vec dirs[] = { {0,1}, {1,0}, {0,-1}, {-1,0} };

bool inBounds( int x ) { return x >= 0 and x < 3; }

bool inBounds( const Vec& v ) {
    return inBounds(v.x) and inBounds(v.y);
}

Vec findZero( const Board& b ) { 
    using pure::list::enumerateTo;
    for( auto x : enumerateTo(3) )
        for( auto y : enumerateTo(3) )
            if( at(x,y,b) == 0 )
                return {x,y};
    throw "WTF";
}

Board successor( Vec toMove, Board b ) {
    Vec zeroPos = findZero( b );

    if( manhattan(toMove,zeroPos) != 1 )
        throw "Bad move!";

    std::swap( at(zeroPos,b), at(toMove,b) );
    return b;
}

using Successors = std::vector<Board>;

Successors successors( const Board& b ) {
    using pure::list::enumerate;
    Successors ss;
    auto z = findZero( b );
    for( auto d : dirs ) {
        auto p = z + d;
        if( inBounds(p) )
            ss.emplace_back( successor(p,b) );
    }
    return ss;
}

Board randomSwapState( unsigned int n=5 ) {
    static std::uniform_int_distribution<int> d(0,2);
    static std::mt19937 engine; 
    static auto rnd = pure::closure( d, engine );

    Board s = GOAL;

    while( n-- ) 
        std::swap (
            at( rnd(), rnd(), s ),
            at( rnd(), rnd(), s )
        );

    return s;
}

using Actions = std::vector< Vec >;

Board play( const Actions& as, Board b ) {
    for( auto a : as ) 
        b = successor( a, move(b) );
    return b;
}

struct Path {
    Actions actions;
    Board s;
};
    
int main() {
    Board s = randomSwapState();

    using std::cout;
    using std::endl;

    cout << "State = \n" << s << endl;

    auto ss = successors( s );

    for( auto x : ss ) 
        cout << "successor :\n" << x << endl;

    cout << "Game won? " << goalState(s) << endl;
}
