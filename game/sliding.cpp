
#include "../Pure.h"
#include "../List.h"

#include <array>
#include <list>
#include <algorithm>
#include <utility>
#include <cmath>
#include <ctime>
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
                return {(int)x,(int)y};
    throw "WTF";
}

struct Successor {
    Board b;
    Vec   action;
    unsigned int cost = 1;

    Successor( Vec toMove, Board b )
        : b( move(b) ), action( toMove )
    {
        Vec zeroPos = findZero( b );

        if( manhattan(toMove,zeroPos) != 1 )
            throw "Bad move!";

        std::swap( at(zeroPos,this->b), at(toMove,this->b) );
    }
};

using Action  = Vec;
using Actions = std::vector< Action >;

Board succeed( Action a, Board b ) {
    return Successor( a, move(b) ).b;
}

using Successors = std::vector<Successor>;

std::pair<Successors,Board> successors( const Board& b ) {
    Successors ss;
    auto z = findZero( b );
    for( auto d : dirs ) {
        auto p = z + d;
        if( inBounds(p) )
            ss.emplace_back( p, b );
    }
    return { move(ss), b };
}

auto succState = pure::state<Board,Successors>( successors );

std::mt19937 engine( std::time(0) ); 

Board randomSwapState( unsigned int n=5 ) {
    static std::uniform_int_distribution<int> d(0,2);
    static auto rnd = pure::closure( d, engine );

    Board s = GOAL;

    while( n-- ) 
        std::swap (
            at( rnd(), rnd(), s ),
            at( rnd(), rnd(), s )
        );

    return s;
}

Board randomOffState( unsigned int n=3 ) {
    static std::uniform_int_distribution<int> d(0,1);
    static auto rnd = pure::closure( d, engine );

    auto toAdd = [&]{ return rnd() ? 1 : -1; };

    Board b = GOAL;
    while( n-- ) {
        auto z = findZero( b );
        if( rnd() ) 
            z.x += toAdd();
        else
            z.y += toAdd();

        if( inBounds(z) )
            b = succeed( z, move(b) );
        else
            n++;
    }

    return b;
}


//auto tick = sget<int>() >>= []( int x ){
//    return sput( x+1 ) >>= ReturnState<int>();
//};

//auto play = sget<Board,Actions>() >>= 

Board play( const Actions& as, Board b ) {
    for( auto a : as ) 
        b = succeed( a, move(b) );
    return b;
}

Actions breadthFirst( const Board& b ) {
    std::list< std::pair<Actions,Board> > fringe{ {{},move(b)} };
    std::vector< Board > past;
    while( fringe.size() ) {
        std::pair<Actions,Board> s = fringe.back();
        fringe.pop_back();

        if( pure::list::elem(s.second,past) )
            continue;

        past.push_back( s.second );

        if( goalState(s.second) )
            return s.first;

        pure::list::vmap (
            [&]( Successor suc ) {
                auto path = s.first;
                path.push_back( suc.action );
                fringe.emplace_front( move(path), move(suc.b) );
            }, successors( s.second ).first
        );
    }
    throw "No solution";
}

Vec place( unsigned int n ) {
    return { (int)n%3, (int)n/3 };
}

unsigned int slideManhattanHeuristic( const Board& b ) {
    using pure::list::enumerateTo;

    unsigned int sum;
    for( auto x : enumerateTo(2) )
        for( auto y : enumerateTo(2) )
            sum += manhattan( place(at(x,y,b)), {(int)x,(int)y} );
    return sum;
}

unsigned int forwardCost( const Actions& as ) {
    return as.size();
}

Actions astar( const Board& b ) {
    using State = std::pair<Actions,Board>;
    std::list< State > fringe{ {{},move(b)} };
    std::vector< Board > past;
    while( fringe.size() ) {
        std::pair<Actions,Board> s = fringe.front();
        fringe.pop_front();

        if( pure::list::elem(s.second,past) )
            continue;

        past.push_back( s.second );

        if( goalState(s.second) )
            return s.first;

        for( auto suc : successors( s.second ).first ) {
            auto path = s.first;
            path.push_back( suc.action );
            fringe = pure::list::insert (
                [&]( const State& a, const State& b ) {
                return forwardCost(a.first) + slideManhattanHeuristic(a.second) 
                     < forwardCost(b.first) + slideManhattanHeuristic(b.second);
                },
                State{ move(path), move(suc.b) },
                move( fringe )
            );
        }
    }
    throw "No solution";
}
    
int main() {
    //Board s = randomSwapState();
    Board s = randomOffState(40);


    using std::cout;
    using std::endl;

    cout << "State = \n" << s << endl;
//    auto ss = succState.runState( s ).get().first;
////    auto ss = successors( s );
//
//    for( auto x : ss ) 
//        cout << "successor :\n" << x.b << endl;
//
//    cout << "Game won? " << goalState(s) << endl;

    auto solution = astar( s );

    for( auto a : solution ) 
        cout << "Move: (" << a.x << ',' << a.y << ")\n" << (s=succeed(a,s)) << endl;

    cout << "Moves : " << solution.size() << endl;

}
