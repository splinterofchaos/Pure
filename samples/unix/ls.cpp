
#include <iostream>
#include <boost/filesystem.hpp>

#include "../../Pure.h"
#include "../../Free.h"

namespace fs = boost::filesystem;

using std::cout;
using std::endl;

using pure::operator>>=;

constexpr struct Vec {
    template< class X, class ...Y >
    std::vector<pure::Decay<X>> operator () ( X&& x, Y&& ...y ) const {
        return { std::forward<X>(x), std::forward<Y>(y)... };
    }
} vec{};

using PathPred = std::function<bool(const fs::path&)>;

PathPred is_dir = pure::free::F<const fs::path&>(fs::is_directory);

PathPred is_executable = []( const fs::path& p ) {
    return fs::status(p).permissions() & 010;
};

PathPred isnt_dotfile = []( const fs::path& p ) {
    return p.filename().c_str()[0] != '.';
};

int main(int argc, char* argv[])
{
    if (argc < 2)
    {
        std::cout << "Usage: tut1 path\n";
        return 1;
    }

    static int argi = 1;
    auto next_path = [&] { return argv[argi++]; };
    auto more_paths = [&] { return argi < argc; };

    while( more_paths() ) {
        fs::path file = next_path();

        if( not fs::exists(file) ) {
            cout << "The file " << file << " does not exist." << endl;
        }

        if( fs::is_directory(file) ) {
            cout << file.c_str() << " : " << endl;

            using pure::list::dupTo;
            using pure::list::range;
            using pure::list::sort;


            using I = fs::directory_iterator;

            using PS = std::vector<fs::path>;
            PS paths = sort( PS(I(file),I()) );

            using V = std::vector<std::string>;
            V dirs, executables, files;

            auto send_when_impl = []( std::function<bool(const fs::path&)> pred, 
                                      V& dst, fs::path p ) 
            {
                if( pred(p) ) {
                    dst.emplace_back( p.filename().string() );
                    return PS{};
                }
                return PS{ std::move(p) };
            };

            auto send_when = [&]( std::function<bool(const fs::path&)> pred, V& dst) {
                return pure::closure( send_when_impl, std::move(pred), dst );
            };

            // Filter out all the prinable elements.
            // Spread the paths to the appropriate places.
            paths =  ((( paths >>= send_when( is_dir,        dirs ) )
                               >>= send_when( is_executable, executables ) )
                               >>= send_when( isnt_dotfile,  files ) );

            //paths = std::move(paths) >>= [&]( fs::path p ) {
            //    auto name = p.filename().string();

            //    if( fs::is_directory(p) ) {
            //        dirs.push_back(name);
            //        return PS();
            //    } else if( fs::status(p).permissions() & 010 ) {
            //        executables.push_back(name);
            //        return PS();
            //    } else if( name[0] != '.' ) {
            //        files.push_back(name);;
            //        return PS();
            //    }
            //    return PS{p};
            //};
                
            for( auto& d : dirs )
                cout << d << "/ ";
            if( dirs.size() )
                cout << '\n';

            for( auto& e : executables )
                cout << '*' << e << ' ';
            if( executables.size() )
                cout << '\n';

            for( auto& f : files )
                cout << f << ' ';
            if( files.size() )
                cout << '\n';
        } else {
            cout << file.c_str() << endl;
        }
    }

    return 0;
}
