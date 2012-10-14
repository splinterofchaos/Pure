
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

bool operator == ( const Vec& a, const Vec& b ) {
    return a.x == b.x and a.y == b.y;
}

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

constexpr Board GOAL = {{ {{0, 1, 2}},
                          {{3, 4, 5}},
                          {{6, 7, 8}} }};

bool slideGoal( const Board& b ) {
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

const std::vector<Vec> dirs = { {0,1}, {1,0}, {0,-1}, {-1,0} };

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

constexpr struct {
    template< class Path >
    unsigned int operator () ( const Path& as ) const {
        return as.size();
    }
} uniformCost{};

unsigned long long astarExpanded = 0;

#include <memory>
#include <utility>
#include <algorithm>

template< class SucceedState, class Model >
using SuccessionType = pure::list::SeqVal<pure::Result<SucceedState,Model>>;

template< class SucceedState, class Model >
using ActionType = 
    typename SuccessionType<SucceedState,Model>::first_type;

template< class SucceedState, class Model >
using Path = std::vector< ActionType<SucceedState,Model> >; 

template< class Succeed, class Model >
using MaybePath = std::unique_ptr< Path<Succeed,Model> >;


#include <deque>
template< class Model, class SucceedST, class Goal,
          class ForwardCost, class Heuristic, 
          class A = ActionType<SucceedST,Model>,
          class P = Path<SucceedST,Model>,
          class MPath = MaybePath<SucceedST,Model> >
MPath astar( Model b, const SucceedST& succeed, const Goal& goal, 
             const ForwardCost& f, const Heuristic& h ) 
{
    // Maintain a fringe ordered from cheapest to most expensive states based
    // on the backward cost (number of steps required) and heuristic (forward)
    // cost.
    using State = std::pair<P,Model>;
    std::deque< State > fringe{ {{},move(b)} };

    std::vector< Model > past;

    astarExpanded = 1;

    const auto cost = [&]( const State& s ) { 
        return f(s.first) + h(s.second); 
    };

    // Compare states by the sum of their forwards and backwards costs.
    const auto compare = pure::on( pure::Less(), cost );

    using pure::list::elem;
    while( fringe.size() ) {
        P ps;
        Model m;

        // Pick the lowest hanging fruit.
        std::tie(ps,m) = fringe.front();
        fringe.pop_front();

        // If this state is in the past, we already expanded on a quicker way
        // to get here, so skip it.
        if( elem(m,past) )
            continue;

        if( goal(m) )
            return pure::Just(move(ps));

        auto successions = succeed( m );
        past.emplace_back( move(m) );

        // We don't want to expand backwards, so it makes sense to check if a
        // state is in the past before inserting to the fringe. Unfortunately,
        // that is very slow as the past grows. Instead, compare against the
        // most recent past states.
        auto recent = pure::list::tail_wrap(
            std::max( 0ul, past.size() - successions.size()*3 - 1 ),
            past
        );

        for( auto suc : move(successions) ) {
            if( elem(suc.second, recent) )
                continue;

            astarExpanded++;

            using pure::list::cons;
            using pure::first;

            // Keep order on insertion.
            fringe = pure::list::insert (
                compare,
                // nextState = { cons(ps,suc.first), suc.second }
                first( cons(ps) )( suc ),
                move( fringe )
            );
        }
    }
    return nullptr;
}

template< class M, class S, class G, class F >
MaybePath<S,M> uniformCostSearch( M&& m, S&& s, G&& g, F&& f ) 
{
    return astar( std::forward<M>(m), std::forward<S>(s), std::forward<G>(g),
                  std::forward<F>(f), pure::pure(0) );
}

template< class M, class S, class G >
MaybePath<S,M> breadthFirstSearch( M&& m, S&& s, G&& g ) 
{
    return uniformCostSearch( std::forward<M>(m), std::forward<S>(s), 
                              std::forward<G>(g), pure::pure(0) );
}

using pure::Decay;
constexpr struct SuccessionPair {
    template< class Suc, class Model, class Action,
              class P = std::pair<Decay<Action>,Decay<Model>> >
    constexpr P operator () ( Suc&& s, Model&& m, Action a ) {
        return P( a, 
                  std::forward<Suc>(s)( a, std::forward<Model>(m) ) );
    }
} successionPair{};

constexpr struct Successions {
    template< class Suc, class Moves, class Model, 
              class Action = pure::list::SeqVal< pure::Result<Moves,Model> > >
    std::vector<std::pair<Action,Model>> operator () ( Suc&& s, Moves&& moves, 
                                                       Model m ) const
    {
        auto ms = moves( m );
        return pure::list::mapTo< std::vector > (
            pure::closure( successionPair, 
                           std::forward<Suc>(s), std::move(m) ),
            ms
        );
    }
} successions{};


constexpr struct SuccessionsFunction {
    template< class Suc, class Moves >
    using Result = pure::Closet< Successions, Suc, Moves >;

    template< class Suc, class Moves >
    constexpr auto operator () ( Suc s, Moves m ) -> Result<Suc,Moves> {
        return pure::closet( successions, move(s), move(m) );
    }
} successionsFunction{};

Board slideSucceed( Vec a, Board b ) {
    Vec zeroPos = findZero( b );

    if( manhattan(a,zeroPos) != 1 )
        throw "Bad move!";

    std::swap( at(zeroPos,b), at(a,b) );
    return b;
}

std::vector<Vec> slideMoves( const Board& b ) {
    using Pred = bool(*)(const Vec&);
    constexpr Pred pred = inBounds;

    return pure::list::filtrate( pure::plus( findZero(b) ),
                                 pred, dirs );
}

auto slideStateSuccessors = successionsFunction( slideSucceed, slideMoves );

std::mt19937 engine( std::time(0) ); 

Vec place( unsigned int n ) {
    return { (int)n%3, (int)n/3 };
}

unsigned int slideHeuristic( const Board& b ) {
    using pure::list::enumerateTo;

    unsigned int sum = 0;
    for( auto x : enumerateTo(2) )
        for( auto y : enumerateTo(2) )
            sum += manhattan( place(at(x,y,b)), {(int)x,(int)y} );
    return sum;
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

using Maze = std::vector<std::string>;

Maze maze = { 
    "###########",
    "# #       #",
    "#   #######",
    "##       ##",
    "# # ####  #",
    "#     #  ##",
    "###########"
}; // NOTE: Takes 9 moves to get from (1,1) to (1,4).

const Vec MAZE_BEGIN = { 1, 1 };
const Vec MAZE_END   = { 1, 4 };

std::vector<Vec> mazeMoves( Vec pos ) {
    std::vector<Vec> moves;

    for( auto d : dirs ) {
        auto p = d + pos;
        if( maze[p.y][p.x] == ' ' )
            moves.push_back( d );
    }

    return moves;
}

auto mazeState = successionsFunction( pure::Add(), mazeMoves );

unsigned int pathHeuristic( Vec pos ) {
    return manhattan( pos, MAZE_END );
}

bool pathGoal( Vec p ) {
    return pathHeuristic(p) == 0;
}

enum Search {
    BFS,
    UCS,
    ASTAR
};

const char* const searchName[] = {
    "Breadth-First Search",
    "Uniform Cost Search",
    "A*"
};

template< class Model, class Suc, class Goal, class H >
void results( Search sType, Model m, Suc s, Goal g, H h ) {
    using std::cout;
    using std::endl;
    using Clock = std::chrono::high_resolution_clock;
    using ms    = std::chrono::milliseconds;

    auto start = Clock::now();

    cout << searchName[sType] << " search." << endl;

    decltype(breadthFirstSearch(m,s,g)) solutionPtr;

    switch( sType ) {
      case BFS: solutionPtr = breadthFirstSearch( m, s, g );
                break;
      case UCS: solutionPtr = uniformCostSearch( m, s, g, uniformCost );
                break;
      case ASTAR: solutionPtr = astar( m, s, g, uniformCost, h );
    }

    if( not solutionPtr ) {
        cout << "No solution\n";
        return;
    }

    auto solution = *solutionPtr;

    cout << "Moves : " << solution.size() << endl;
    cout << "Expanded " << astarExpanded << " nodes.\n";
    cout << "Time : " << 
        std::chrono::duration_cast<ms>( Clock::now() - start ).count()/1000.f 
        << " seconds "<< endl << endl;
}
    
int main() {
    using std::cout;
    using std::endl;

    results( ASTAR, Vec{1,1}, mazeState, pathGoal, pathHeuristic );
    results( UCS,   Vec{1,1}, mazeState, pathGoal, pathHeuristic );
    results( BFS,   Vec{1,1}, mazeState, pathGoal, pathHeuristic );
   
    //Board s = randomSwapState();
    Board s = randomOffState(999);
    cout << "\nSlide puzzle\n" << s << endl << endl;
    results( ASTAR, s, slideStateSuccessors, slideGoal, slideHeuristic );
    results( UCS,   s, slideStateSuccessors, slideGoal, slideHeuristic );
}
