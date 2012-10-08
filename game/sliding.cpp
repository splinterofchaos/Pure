
#include "../Pure.h"
#include "../List.h"

#include <array>
#include <list>
#include <algorithm>
#include <utility>
#include <cmath>
#include <ctime>
#include <iostream>
#include <chrono>

using Row = std::array<unsigned int,3>;
using Board = std::array< Row, 3>;

using std::move;

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

template< class Model, class Action >
struct Succession {
    using model_type  = Model;
    using action_type = Action;

    model_type  m;
    action_type a;
    unsigned int cost = 1;

    constexpr Succession( model_type m, action_type a, unsigned int cost=1 )
        : m(move(m)), a(move(a)), cost(cost)
    {
    }
};

template< class M, class A, class S = Succession<M,A> >
S succession( M m, A a, unsigned int cost=1 ) {
    return S( move(m), move(a), cost );
}

Succession<Board,Vec> slideSuccession( Vec toMove, Board b ) {
        Vec zeroPos = findZero( b );

        if( manhattan(toMove,zeroPos) != 1 )
            throw "Bad move!";

        std::swap( at(zeroPos,b), at(toMove,b) );

        return succession( b, toMove );
}

using Action  = Vec;
using Path = std::vector< Action >;

Board slideSucceed( Vec a, Board b ) {
    return slideSuccession( a, move(b) ).m;
}

template< class M, class A >
using SuccessionList = std::vector<Succession<M,A>>;

using SlideSuccessors = SuccessionList<Board,Vec>;

SlideSuccessors slideSuccessors( const Board& b ) {
    SlideSuccessors ss;

    auto z = findZero( b );
    for( auto d : dirs ) {
        auto p = z + d;
        if( inBounds(p) )
            ss.emplace_back( slideSuccession(p,b) );
    }
    return ss;
}

std::mt19937 engine( std::time(0) ); 

Vec place( unsigned int n ) {
    return { (int)n%3, (int)n/3 };
}

unsigned int manhattanPriority( const Board& b ) {
    using pure::list::enumerateTo;

    unsigned int sum;
    for( auto x : enumerateTo(2) )
        for( auto y : enumerateTo(2) )
            sum += manhattan( place(at(x,y,b)), {(int)x,(int)y} );
    return sum;
}

unsigned int hammingPriority( const Board& b ) {
    using pure::list::enumerateTo;

    unsigned int sum;
    for( auto x : enumerateTo(2) )
        for( auto y : enumerateTo(2) )
            sum += at(x,y,b) == (x + 3*y);
    return sum;
}

template< class Path >
unsigned int forwardCost( const Path& as ) {
    return as.size();
}

unsigned long long astarExpanded = 0;

struct {
    template< class Heuristic, class Path, class Model >
    unsigned int operator () ( Heuristic h, const std::pair<Path,Model>& s ) {
        return forwardCost(s.first) + h(s.second);
    }
} astarCost{};

#include <memory>
#include <utility>
#include <algorithm>

template< class Model, class Succeed, class Heuristic,
          class A = typename pure::Decay <
              decltype (
                  std::declval<Succeed>()(std::declval<Model>())[0]
              ) 
          > :: action_type,
          class Path = std::vector<A>,
          class MaybePath = std::unique_ptr<Path> >
MaybePath astar( const Model& b, Succeed succeed, Heuristic h ) {
    using State = std::pair<Path,Model>;
    std::list< State > fringe{ {{},move(b)} };
    std::vector< Model > past;

    astarExpanded = 1;

    auto cost = pure::closure(astarCost,h);
    auto compare = [&]( const State& a, const State& b ) {
        return cost(a) < cost(b);
    };

    while( fringe.size() ) {
        Path ps;
        Model m;

        std::tie(ps,m) = fringe.front();
        fringe.pop_front();

        if( pure::list::elem(m,past) )
            continue;

        past.push_back( m );

        if( goalState(m) )
            return MaybePath( new Path(move(ps)) );

        using pure::list::cons;
        using pure::list::insert;
        for( auto suc : succeed(m) ) {
            astarExpanded++;

            auto path = cons( ps, suc.a );
            fringe = insert (
                compare,
                State{ move(path), move(suc.m) },
                move( fringe )
            );
        }
    }
    return nullptr;
}

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
            b = slideSucceed( z, move(b) );
        else
            n++;
    }

    return b;
}



template< class H >
void results( const Board& b, H h, const char* const name ) {
    using std::cout;
    using std::endl;
    using Clock = std::chrono::high_resolution_clock;
    using ms    = std::chrono::milliseconds;

    auto start = Clock::now();
    auto solutionPtr = astar( b, slideSuccessors, h );

    cout << endl << name << endl;

    if( not solutionPtr ) {
        cout << "No solution\n";
        return;
    }

    auto solution = *solutionPtr;

    cout << "Moves : " << solution.size() << endl;
    cout << "Expanded " << astarExpanded << " nodes.\n";
    cout << "Time : " << std::chrono::duration_cast<ms>( Clock::now() - start ).count() << endl;
}
    
int main() {
    //Board s = randomSwapState();
    Board s = randomOffState(40);


    using std::cout;
    using std::endl;

    cout << "State = \n" << s << endl;

    results( s, manhattanPriority, "Manhattan" );
    results( s, hammingPriority,   "Hamming"   );
    results( s, pure::pure(0),   "null heuristic"   );
}