
#include "../tpl.h"

#include <string>
#include <iostream>

#include <typeinfo>

#include <cxxabi.h>

using namespace pure::tpl;

std::string demangle(const char* name) {

    int status = -4;

    char* res = abi::__cxa_demangle(name, NULL, NULL, &status);

    std::string ret = status ? name : res;
    free(res);

    return ret;
}

namespace std {

string to_string( const char* const str ) {
    return str;
}

string to_string( string str ) {
    return move(str);
}
template< class ...X >
string to_string( const std::tuple<X...>& t );

}

constexpr struct Serialize {
    template< class X >
    std::string operator () ( const X& x ) const {
        return demangle( typeid(x).name() ) + ":" + std::to_string(x);
    }
} serialize{};

namespace std {

template< class ...X >
string to_string( const std::tuple<X...>& t )
{
    using namespace pure;
    return "<" + foldl (
        compose( add, add.with(", ") ),
        zipWith( serialize, t )
    ) + ">";
}

} // namespace std

int main() {
    auto t = tuple(1,2,"hi",5.2);
    std::cout << std::to_string(t) << std::endl;
}
