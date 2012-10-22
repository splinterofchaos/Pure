
namespace pure {

namespace free {

/*
 * Free: Utility functions for dealing with free functions.
 *
 * Free functions overloaded by their argument types can be difficult to pass
 * into higher order functions. For example:
 *
 *      map( std::pow, someSequence );
 *
 * The compiler cannot infer whether std::pow refers to the float or double
 * version. To rectify this, one might just specify its type.
 *
 *      using Fn = double(*)(double,double);
 *      Fn p = std::pow;
 *      map( p, someSequence );
 *      map( Fn(std::pow), someSequence ); // Also OK.
 *
 * Instead, we can define a function, F, of some explicit type, X, which tells
 * the compiler what types the function takes. It can then figure out which we
 * want.
 */

/* F<X...>(ptr) = ptr */
template< class ...X, class R >
constexpr auto F( R(*ptr)(X...) ) -> decltype(ptr) {
    return ptr;
}

/* Binary F */
template< class X, class R >
constexpr auto B( R(*ptr)(X,X) ) -> decltype(ptr) {
    return ptr;
}

// Note: B is not strictly necessary as knowing only the first argument type,
// the compiler may be able to deduce the correct function. However, this may
// not always be the case.

}

}
