

#pragma once

#include "List.h"

#include <iostream>
#include <iterator>

namespace pure {
namespace io {

template< class X, class Stream > struct Contents {
    // GCC seems to have not implemented a movable io stream yet, so we'll just
    // use a reference for now.
    Stream& stream;

    using iterator = std::istream_iterator<X>;

    Contents( Stream& s ) : stream(s) { }

    iterator begin() const { return iterator(stream); }
    iterator end()   const { return iterator();       }
};

template< class X, class Stream, class C = Contents<X,Stream> >
C fileContents( Stream& s ) {
    return C( s );
}

} // namespace io
} // namespace pure

