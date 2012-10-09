
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

Board GOAL = {{ {{0, 1, 2}},
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

template< class Model, class Action, class F >
using SearchState = pure::StateT<Model,Action,std::vector,F>;

template< class M, class A, class F >
SearchState<M,A,F> stateSuccession( F f ) {
    return { move(f) };
}

template< class M, class A, class S = Succession<M,A> >
constexpr S succession( M m, A a, unsigned int cost=1 ) {
    return S( move(m), move(a), cost );
}

constexpr struct {
    template< class Path >
    unsigned int operator () ( const Path& as ) {
        return as.size();
    }
} uniformCost;

unsigned long long astarExpanded = 0;

#include <memory>
#include <utility>
#include <algorithm>

template< class SucceedState >
using Path = std::vector< typename SucceedState::accumulator_type >; 

template< class Succeed >
using MaybePath = std::unique_ptr< Path<pure::Decay<Succeed>> >;

template< class Model, class SucceedST, class Goal,
          class ForwardCost, class Heuristic, 
          class A = typename SucceedST::accumulator_type,
          class Path  = std::vector<A>,
          class MPath = std::unique_ptr<Path> >
MPath astar( Model b, SucceedST succeed, Goal goal, ForwardCost f, Heuristic h ) 
{
    // Maintain a fringe ordered from cheapest to most expensive states based
    // on the backward cost (number of steps required) and heuristic (forward)
    // cost.
    using State = std::pair<Path,Model>;
    std::list< State > fringe{ {{},move(b)} };

    std::vector< Model > past;

    astarExpanded = 1;

    // Compare states by the sum of their forwards and backwards costs.
    auto compare = pure::on ( 
        pure::Less(),
        [&]( const State& s ) { return f(s.first) + h(s.second); }
    );

    while( fringe.size() ) {
        Path ps;
        Model m;

        // Pick the lowest hanging fruit.
        std::tie(ps,m) = fringe.front();
        fringe.pop_front();

        if( pure::list::elem(m,past) )
            continue;

        past.push_back( m );

        if( goal(m) )
            return pure::Just(move(ps));

        using pure::list::cons;
        using pure::list::insert;
        for( auto suc : succeed.runState(m) ) {
            A a;
            Model next;
            std::tie(a,next) = suc;

            astarExpanded++;

            auto path = cons( ps, a );

            // Keep order on insertion.
            fringe = insert (
                compare,
                State{ move(path), move(next) },
                move( fringe )
            );
        }
    }
    return nullptr;
}

template< class M, class S, class G, class F >
MaybePath<S> uniformCostSearch( M&& m, S&& s, G&& g, F&& f ) 
{
    return astar( std::forward<M>(m), std::forward<S>(s), std::forward<G>(g),
                  std::forward<F>(f), pure::pure(0) );
}

template< class M, class S, class G >
MaybePath<S> breadthFirstSearch( M&& m, S&& s, G&& g ) 
{
    return uniformCostSearch( std::forward<M>(m), std::forward<S>(s), 
                              std::forward<G>(g), pure::pure(0) );
}

constexpr struct SuccessionPair {
    template< class Suc, class Model, class Action >
    constexpr std::pair< Action, Model > operator () ( Suc s, Model m,
                                                       Action a ) 
    {
        return { a, s( a, m ) };
    }
} successionPair{};

constexpr struct Successions {
    template< class Suc, class Moves, class Model, 
              class Action = pure::list::SeqVal < 
                  decltype(std::declval<Moves>()(std::declval<Model>())) 
            > >
    std::vector<std::pair<Action,Model>> operator () ( Suc s, Moves moves, Model m ) const
    {
        return pure::list::mapTo< std::vector > (
            pure::closure( successionPair, s, m ),
            moves( m )
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

template< class M, class A, class S, class Moves >
using SuccessionState = pure::StateT < 
    M, A, std::vector, 
    typename SuccessionsFunction::Result<S,Moves>
>;

template< class Model, class Action, class Suc, class Moves >
auto successionsState( Suc s, Moves m ) 
    -> SuccessionState<Model,Action,Suc,Moves>
{
    return { successionsFunction( move(s), move(m) ) };
}

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

auto slideStateSuccessors = successionsState<Board,Vec>( slideSucceed, slideMoves );

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


template< class H >
void slideResults( const Board& b, H h, const char* const name ) {
    using std::cout;
    using std::endl;
    using Clock = std::chrono::high_resolution_clock;
    using ms    = std::chrono::milliseconds;

    auto start = Clock::now();
    //auto solutionPtr = astar( b, slideSuccessors, slideGoal, uniformCost, h );
    auto solutionPtr = astar( b, slideStateSuccessors, slideGoal, uniformCost, h );

    cout << endl << name << endl;

    if( not solutionPtr ) {
        cout << "No solution\n";
        return;
    }

    auto solution = *solutionPtr;

    cout << "Moves : " << solution.size() << endl;
    cout << "Expanded " << astarExpanded << " nodes.\n";
    cout << "Time : " << 
        std::chrono::duration_cast<ms>( Clock::now() - start ).count()/1000.f 
        << " seconds "<< endl;
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

auto mazeState = successionsState<Vec,Vec>( pure::Add(), mazeMoves );

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

void mazeResults( Search s, Vec p ) {
    using std::cout;
    using std::endl;
    using Clock = std::chrono::high_resolution_clock;
    using ms    = std::chrono::milliseconds;

    auto start = Clock::now();

    cout << searchName[s] << " search." << endl;

    MaybePath< decltype(mazeState) > solutionPtr;

    switch( s ) {
      case BFS: solutionPtr = breadthFirstSearch( p, mazeState, pathGoal );
                break;
      case UCS: solutionPtr = uniformCostSearch( p, mazeState, 
                                                 pathGoal, uniformCost );
                break;
      case ASTAR: solutionPtr = astar( p, mazeState, pathGoal, 
                                        uniformCost, pathHeuristic );
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


    mazeResults( ASTAR, {1,1} );
    mazeResults( UCS, {1,1} );
    mazeResults( BFS, {1,1} );
   
    //Board s = randomSwapState();
    Board s = randomOffState(999);
    cout << "\nSlide puzzle\n" << s << endl;

    slideResults( s, slideHeuristic, "Manhattan" );
    //slideResults( s, pure::pure(0), "null heuristic" );
}
