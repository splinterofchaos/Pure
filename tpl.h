
#pragma once

#include <utility>
#include <string>

#include "Functional.h"

namespace pure {

template< size_t i, class T > struct AssertInTupleRange {
    enum { size = std::tuple_size<T>::value };
    static_assert( i < size, "Index larger than tuple size." );
};

template< size_t i, class T, class Assert = AssertInTupleRange<i,Decay<T>> >
constexpr auto get( T&& t )
    -> decltype( Assert(), std::get<i>(declval<T>()) )
{
    return std::get<i>( forward<T>(t) );
}

template< size_t i, class T, size_t S = std::tuple_size<Decay<T>>::value - 1,
          size_t j = S - i,
          class Assert = AssertInTupleRange<j,Decay<T>> >
constexpr auto rget( T&& t )
    -> decltype( Assert(), get<j>(declval<T>()) )
{
    return get<j>( forward<T>(t) );
}

template< size_t i > struct Get {
    template< class T >
    constexpr auto operator () ( T&& t )
        -> decltype( get<i>(declval<T>()) )
    {
        return get<i>( forward<T>(t) );
    }
};

template< size_t i > struct RGet {
    template< class T >
    constexpr auto operator () ( T&& t )
        -> decltype( rget<i>(declval<T>()) )
    {
        return rget<i>( forward<T>(t) );
    }
};

namespace tpl {

/*
 * IFromType::value = the index of the first type for which P<X> is true.
 * P is assumed to also use the ::value idiom.
 */
template< template<class...> class P, size_t i, class X, class ...Y >
struct IFromType {
    // Don't use enum here; GCC gives a hideous warning
    // for mixing enum and non-enum type expressions.
    static constexpr bool done = P<X>::value;
    static constexpr size_t value = done ? i
        : IFromType< P, i+1, Y... >::value;
};

template< template<class...> class P, size_t i, class X >
struct IFromType<P,i,X> {
    static constexpr size_t value = P<X>::value ? i : -1;
};

template< template<class...> class P, class ...X >
struct IFromTuple;

template< template<class...> class P, class ...X >
struct IFromTuple< P, std::tuple<X...> > {
    enum { value = IFromType<P,0,X...>::value };
};

template< class X > struct Type {
    template< class Y > struct Same    : std::is_same<X,Y> { };
    template< class Y > struct Similar : std::is_convertible<X,Y> { };
    template< class Y > struct Base    : std::is_base_of<X,Y> { };
};

template< class X > struct IOfType {
    template< class T >
    using type = IFromTuple< Type<X>::template Same, T >;

    template< class T >
    constexpr size_t get() {
        return type< Decay<T> >::value;
    }
};

template< class X, class T >
constexpr size_t iOfType() {
    return IOfType<X>().get<T>();
}

template< class X > struct IOfSimilar {
    template< class T >
    using type = IFromTuple< Type<X>::template Similar, T >;

    template< class T >
    constexpr size_t get() {
        return type< Decay<T> >::value;
    }
};

template< class X, class T >
constexpr size_t iOfSimilar() {
    return IOfSimilar<X>().get<T>();
}

/*
 * Find the index, i, of the type for which P<i> is true,
 * and forward the tuple to F<i>
 */
template< template<class...> class P/*redicate*/,
          template<size_t> class F >
struct SelectI {
    template< class T, size_t i = IFromTuple<P,Decay<T>>::value >
    constexpr auto operator () ( T&& t )
        -> decltype( F<i>()( forward<T>(t) ) )
    {
        return F<i>()( forward<T>(t) );
    }
};

/* gett - Get the first element of type X. */
template< class X > struct GetT {
    template< class T, class G = Get<iOfType<X,T>()> >
    constexpr auto operator () ( T&& t )
        -> decltype( G()( forward<T>(t) ) )
    {
        return G()( forward<T>(t) );
    }
};

template< class X, class T >
constexpr auto gett( T&& t )
    -> decltype( GetT<X>()(forward<T>(t)) )
{
    return GetT<X>()( forward<T>(t) );
}

/* gets - Get the first element similar to X. */
template< class X > struct GetS {
    template< class T, class Find = SelectI<Type<X>::template Similar,Get> >
    constexpr auto operator () ( T&& t )
        -> decltype( Find()( forward<T>(t) ) )
    {
        return Find()( forward<T>(t) );
    }
};

template< class X, class T >
constexpr auto gets( T&& t )
    -> decltype( GetS<X>()(forward<T>(t)) )
{
    return GetS<X>()( forward<T>(t) );
}



/* CONSTRUCTION AND TRAITS */

constexpr auto pair  = MakeBinaryT<std::pair>();
constexpr auto tuple = MakeT<std::tuple>();

// Function objects for std::forward_as_tuple and std::tie.
constexpr auto forwardTuple = ForwardT<std::tuple>();
constexpr auto tie          = TieT<std::tuple>();

template< class ...X > using Tuple = std::tuple< Decay<X>... >;
template< class ...X > using Tie   = decltype( tie(declval<X>()...) );
template< class ...X > using ForwardTuple = std::tuple< X... >;

template< class X > struct TTraits { // Tuple Traits.
    enum { isTuple = false, isPair = false };
};

template< class ...X > struct TTraits<std::tuple<X...>> {
    using type = std::tuple<X...>;

    template< class ... Y >
    using remap = std::tuple<Y...>;

    static constexpr auto ctor = tuple;

    enum { isTuple = true, isPair = false };
};

template< class X, class Y > struct TTraits<std::pair<X,Y>> {
    using type = std::pair<X,Y>;

    template< class A, class B >
    using remap = std::pair<A,B>;

    static constexpr auto ctor = pair;

    enum { isTuple = false, isPair = true };
};

template< class T, class ...X >
using remap = typename TTraits< Decay<T> >::template remap<X...>;

template< class T > constexpr bool isTuple() {
    return TTraits< Decay<T> >::isTuple;
}

template< class P > constexpr bool isPair() {
    return TTraits< Decay<P> >::isPair;
}

/* If T is a tuple, returns a tuple, if a pair, returns a pair. */
template< class T, class ...X, class TT = TTraits<Decay<T>> >
constexpr auto makeSimilar( X&& ...x )
    -> decltype( TT::ctor( forward<X>(x)... ) )
{
    return TT::ctor( forward<X>(x)... );
}

template< class T > constexpr size_t size() {
    return std::tuple_size< Decay<T> >::value;
}

template< class T > constexpr size_t size( const T& ) {
    return size<T>();
}

template< size_t i, class T >
using Elem = decltype( get<i>(declval<T>()) );

template< class X > struct TupleOrPair {
    enum { value = isTuple<X>() or isPair<X>() };
};

template< class X >
static constexpr bool tupleOrPair() {
    return TupleOrPair<X>::value;
}

template< class T, class ...U > struct AnyIsTuple {
    enum { value = isTuple<T>() or AnyIsTuple<U...>::value };
};

template< class T > struct AnyIsTuple<T> {
    enum { value = isTuple<T>() };
};

template< class ...X > struct RecursiveTuple;

template< class ...X > struct RecursiveTuple< std::tuple<X...> > {
    enum { value = AnyIsTuple<X...>::value };
};

template< class T > constexpr bool recursiveTuple() {
    return RecursiveTuple< Decay<T> >::value;
}


/* INDEXING */

// The last index in the parameter pack.
template< size_t ... > struct LastIndex;

template< size_t X > struct LastIndex<X> {
    static constexpr size_t I = X;
};

template< size_t X, size_t ...Y > struct LastIndex<X,Y...> {
    static constexpr size_t I = LastIndex<Y...>::I;
};

/*
 * Index List
 * The list of indecies. Ex: IndexList<1,5,2>().
 *
 * Typical usage:
 * An IndexList will usually be sent to an implementation function which
 * captures the indicies in a parameter pack.
 *
 * Ex:
 * template< size_t ...I, class T >
 * auto f( T t ) { g( std::get<I>(t)... ); }
 *
 * If I = {1,5,2}, then those are the parameters to g.
 */
// Used for sequential access of tuple members.
template< size_t ...i > struct IndexList {
    static constexpr size_t I = LastIndex<i...>::I;
    using Next = IndexList< i..., I+1 >;
};

template<> struct IndexList<> {
    using Next = IndexList<>;
};

/*
 * Build an IndexList of N elements, [0,N).
 *
 * To build the IndexList for an entire tuple:
 *     IListBuilder< std::tuple_size<T>::value >::type
 */
template< size_t n > struct IListBuilder {
    using type = typename IListBuilder< n-1 >::type::Next;
};

// Base case: one element.
template<> struct IListBuilder<1> {
    using type = IndexList<0>;
};

// No elements: empty list.
template<> struct IListBuilder<0> {
    using type = IndexList<>;
};

/* Build an IndexList of N elements, [0,N). */
template< size_t N >
using BuildList = typename IListBuilder<N>::type;

/* Build the entire IndexList for T. */
template< class T >
using TupleIndicies = BuildList< std::tuple_size<Decay<T>>::value >;


/* APPLICATION */

// Helper: Ignore I, return x.
template< size_t I, class X > const X& xOfI( const X& x ) { return x; }
template< size_t I, class X >       X& xOfI(       X& x ) { return x; }

template< size_t i > struct XOfI : Id {
    constexpr XOfI() : Id() { }
};

template< template<size_t> class Fi, size_t i, class F, class ...T >
constexpr auto applyRowBy( const F& f, T&& ...t )
    -> decltype( f( Fi<i>()(forward<T>(t))... ) )
{
    return f( Fi<i>()(forward<T>(t))... );
}

template< size_t i, class F, class ...T >
constexpr auto applyRow( F&& f, T&& ...t )
    -> decltype( forward<F>(f)( get<i>(forward<T>(t))... ) )
{
    return forward<F>(f)( get<i>(forward<T>(t))... );
}

template< template<size_t> class Fi, size_t ...i, class R, class F, class ...T >
constexpr auto applyRowsBy( const R& r, const F& f, T&& ...t )
    -> decltype( r( applyRowBy<Fi,i>(f,forward<T>(t)...)... ) )
{
    return r( applyRowBy<Fi,i>(f,forward<T>(t)...)... );
}

template< size_t ...i, class R, class F, class ...T >
constexpr auto applyRows( const R& r, const F& f, T&& ...t )
    -> decltype( r( applyRow<i>(f,forward<T>(t)...)... ) )
{
    return r( applyRow<i>(f,forward<T>(t)...)... );
}

template< template<size_t> class Fi, size_t ...i, class R, class F, class ...T >
constexpr auto applyIndexedRowsBy( IndexList<i...>, const R& r, const F& f, T&& ...t )
    -> decltype( applyRowBy<Fi,i...>( r, f, forward<T>(t)... ) )
{
    return applyRowBy<Fi,i...>( r, f, forward<T>(t)... );
}

template< size_t ...i, class R, class F, class ...T >
constexpr auto applyIndexedRows( IndexList<i...>, const R& r, const F& f, T&& ...t )
    -> decltype( applyRows<i...>( r, f, forward<T>(t)... ) )
{
    return applyRows<i...>( r, f, forward<T>(t)... );
}

constexpr struct ApplyTupleRows : Binary<ApplyTupleRows> {
    using Binary<ApplyTupleRows>::operator();

    template< class R, class F, class T, class ...U,
              class IS = TupleIndicies<T> >
    constexpr auto operator () ( const R& r, const F& f, T&& t, U&& ...u )
        -> decltype( applyIndexedRows(IS(),r,f,forward<T>(t),forward<U>(u)...) )
    {
        return applyIndexedRows( IS(), r, f, forward<T>(t), forward<U>(u)... );
    }
} applyTupleRows{};

template< template<size_t> class Fi, size_t ...i, class F, class X >
constexpr auto applyIndiciesBy( F&& f, X&& x )
    -> decltype( forward<F>(f)( Fi<i>()(forward<X>(x))... ) )
{
    return forward<F>(f)( Fi<i>()(forward<X>(x))... );
}

template< template<size_t> class Fi, size_t ...i, class F, class X >
constexpr auto applyIndexListBy( IndexList<i...>, F&& f, X&& x )
    -> decltype( applyIndiciesBy<Fi,i...>(forward<F>(f),forward<X>(x)) )
{
    return applyIndiciesBy<Fi,i...>(forward<F>(f),forward<X>(x));
}

template< size_t ...i, class F, class T >
constexpr auto applyIndicies( F&& f, T&& t ) 
    -> Result< F, Elem<i,T>... >
{
    return forward<F>(f)( get<i>(forward<T>(t))... );
}

template< size_t ...i, class F, class T >
constexpr auto applyIndexList( IndexList<i...>, F&& f, T&& t )
    -> decltype( forward<F>(f)( get<i>(forward<T>(t))... ) )
{
    return forward<F>(f)( get<i>(forward<T>(t))... );
}


/*
 * Apply f to t's members.
 *
 * apply : (a x b... -> c) x {a,b...} -> c
 * apply : (a x b x c... -> d) x {a,b...} x c... -> d
 * apply( f, {x,y,z} ) = f(x,y,z)
 */
// In full, this is tpl::apply.
constexpr struct Apply : Binary<Apply> {
    using Binary<Apply>::operator();

    template< class F, class T, class I = TupleIndicies<T> >
    constexpr auto operator () ( F&& f, T&& t )
        -> decltype( applyIndexList( I(), forward<F>(f), forward<T>(t) ) )
    {
        return applyIndexList( I(), forward<F>(f), forward<T>(t) );
    }
} apply{};

template< template<size_t> class Fi, class F, class T, class I = TupleIndicies<T> >
constexpr auto applyBy( F&& f, T&& t )
    -> decltype( applyIndexListBy<Fi>( I(), forward<F>(f), forward<T>(t) ) )
{
    return applyIndexListBy<Fi>( I(), forward<F>(f), forward<T>(t) );
}

template< template<size_t> class Fi >
struct ApplyBy : Binary<ApplyBy<Fi>> {
    using Binary<ApplyBy<Fi>>::operator();

    template< class F, class T >
    constexpr auto operator () ( F&& f, T&& t )
        -> decltype( applyBy<Fi>( forward<F>(f), forward<T>(t) ) )
    {
        return applyBy<Fi>( forward<F>(f), forward<T>(t) );
    }
};



/* LIST-LIKE ACCESS */

constexpr struct toArray {
    template< class R, size_t ...I, class T >
    static constexpr R impl( IndexList<I...>, T&& t )
    {
        return {{ get<I>(forward<T>(t))... }};
    }

    template< class T, class X = Decay<Elem<0,T>>, size_t S = size<T>(),
              class IS = TupleIndicies<T>,
              class A = std::array<X,S> >
    constexpr A operator () ( T&& t ) {
        return impl<A>( IS(), forward<T>(t) );
    }
} toArray{};

constexpr auto head = Get<0>();

constexpr struct last {
    template< class T, size_t S = size<T>() >
    constexpr Elem<S-1,T> operator () ( T&& t ) {
        return get<S-1>( forward<T>(t) );
    }
} last{};

/* Make a tuple of N X's. */
template< size_t N, class X, class I = BuildList<N> >
constexpr auto repeat( const X& x )
    -> decltype( applyIndexListBy<XOfI>( I(), tuple, x ) )
{
    return applyIndexListBy<XOfI>( I(), tuple, x );
}

/* Make a tuple of N references to x. */
template< size_t N, class X, class I = BuildList<N> >
constexpr auto repeatTie( X&& x )
    -> decltype( applyIndexListBy<XOfI>( I(), tie, x ) )
{
    return applyIndexListBy<XOfI>( I(), tie, x );
}

template< size_t N > struct Repeat {
    template< class X >
    constexpr auto operator () ( const X& x )
        -> decltype( repeat<N>( x ) )
    {
        return repeat<N>( x );
    }
};

template< size_t N > struct RepeatTie {
    template< class X >
    constexpr auto operator () ( const X& x )
        -> decltype( repeatTie<N>( x ) )
    {
        return repeatTie<N>( x );
    }
};

/* reverse : {a...} -> {...a} */
constexpr auto reverse = ApplyBy<RGet>()( tuple );

/*
 * Take each value, with the offset, N.
 * takeFrom< 2, I...={2,3} > ( {1,2,3,4} ) = {3,4}
 */
template< size_t N, size_t ...I, class T >
constexpr auto takeFrom( IndexList<I...>, T&& t )
    -> std::tuple< Decay<Elem<I+N,T>>... >
{
    return tuple( get<I+N>(forward<T>(t))... );
}

/* Take N elements. */
template< size_t N, class T, class IS = BuildList<N> >
constexpr auto take( T&& t )
    -> decltype( takeFrom<0>( IS(), forward<T>(t) ) )
{
    return takeFrom<0>( IS(), forward<T>(t) );
}

template< size_t N > struct Take {
    template< class T >
    constexpr auto operator () ( T&& t )
        -> decltype( take<N>( forward<T>(t) ) )
    {
        return take<N>( forward<T>(t) );
    }
};

/* Drop N elements. */
template< size_t N, class T, class IS = BuildList<size<T>()-N> >
constexpr auto drop( T&& t )
    -> decltype( takeFrom<N>( IS(), forward<T>(t) ) )
{
    return takeFrom<N>( IS(), forward<T>(t) );
}

template< size_t N > struct Drop {
    template< class T >
    constexpr auto operator () ( T&& t )
        -> decltype( drop<N>( forward<T>(t) ) )
    {
        return drop<N>( forward<T>(t) );
    }
};

constexpr auto tail = Drop<1>();

/* Drop the last N elements. */
template< size_t N = 1, class T, size_t M = size<T>() - N >
constexpr auto init( T&& t )
    -> decltype( take<M>( forward<T>(t) ) )
{
    return take<M>( forward<T>(t) );
}

template< size_t N = 1, class T, size_t M = size<T>() - N >
constexpr auto rinit( T&& t )
    -> decltype( drop<M>( forward<T>(t) ) )
{
    return drop<M>( forward<T>(t) );
}

template< size_t N = 1 > struct Init {
    template< class T >
    constexpr auto operator () ( T&& t )
        -> decltype( init<N>( forward<T>(t) ) )
    {
        return init<N>( forward<T>(t) );
    }
};

template< size_t N = 1 > struct RInit {
    template< class T >
    constexpr auto operator () ( T&& t )
        -> decltype( rinit<N>( forward<T>(t) ) )
    {
        return rinit<N>( forward<T>(t) );
    }
};

constexpr auto arrayToTuple = closure( apply, tuple );

/* append : {a...} x {b...} x ... -> {a..., b..., ...} */
constexpr struct Append {
    template< class ...T >
    constexpr auto operator () ( T&& ...t )
        -> decltype( std::tuple_cat( forward<T>(t)... ) )
    {
        return std::tuple_cat( forward<T>(t)... );
    }
} append{};

/* cons : {a...} x b -> {a...,b} */
constexpr struct Cons : Chainable<Cons> {
    using Chainable<Cons>::operator();

    template< class T, class X >
    constexpr auto operator () ( T t, X&& x )
        -> decltype( append( move(t), tuple(forward<X>(x)) ) )
    {
        return append( move(t), tuple(forward<X>(x)) );
    }
} cons{};

/* rcons : {b...} x a -> {a,b...} */
constexpr struct RCons : Chainable<RCons> {
    using Chainable<RCons>::operator();

    template< class T, class X >
    constexpr auto operator () ( T t, X&& x )
        -> decltype( append( tuple(forward<X>(x)), move(t) ) )
    {
        return append( tuple(forward<X>(x)), move(t) );
    }
} rcons{};

/*
 * Zip each tuple together with f.
 *
 * zipWith : (a -> b) x {a...} -> {b...}
 * zipWith( f, {x,y}, {a,b} ) = { f(x,a), f(y,b) }
 */
constexpr struct ZipWith : Binary<ZipWith> {
    using Binary<ZipWith>::operator();

    template< class F, class ...T >
    constexpr auto operator () ( const F& f, T&& ...t )
        -> decltype( applyTupleRows( tuple, f, forward<T>(t)... ) )
    {
        return applyTupleRows( tuple, f, forward<T>(t)... );
    }
} zipWith{};

template< size_t i, class ...T >
constexpr auto zipRow( T&& ...t )
    -> decltype( tuple( get<i>(forward<T>(t))... ) )
{
    return tuple( get<i>(forward<T>(t))... );
}

/* zip( {x,y}, {a,b} ) = { (x,a), (y,b) } */
constexpr auto zip = closure( zipWith, tuple );

/* fold : (a -> a -> b) x {a...} -> {b...} */
constexpr auto foldl = compose( apply, chainl );
constexpr auto foldr = compose( apply, chainr );

/* concat : { {a...}, {b...}, ... } -> { a..., b..., ... } */
constexpr auto concat = closure( foldl, append );

// If given a tuple, return it. If not, make a tuple!
constexpr auto makeTupleIfNot = selectF<TupleOrPair>(id,tuple);

/* 
 * Make the tuple one level more shallow.
 * level( {a,{b},{{c}}} ) = {a,b,{c}} 
 *
 * Make each element a tuple (unless it already is one)
 * and then concatenates them together.
 *
 * Example:
 *     input:        {  0,   1,  {2}, {{3}} }  
 *     after zip:    { {0}, {1}, {2}, {{3}} }
 *     concatenated: {  0,   1,   2,   {3}  }
 */
constexpr auto level = compose (
    concat, zipWith(makeTupleIfNot)
);



/* STACK-LIKE ACCESS */

/* rot : { a, b... } -> { b..., a } */
constexpr auto rot  = bcompose( cons, tail, head );

/* rrot : { a..., b } -> { b, a... } */
constexpr auto rrot = bcompose( rcons, Init<1>(), last );

/* dup : { a..., b } -> { a..., b, b } */
constexpr auto dup = bcompose( cons, id, last );

/* swap : { a..., b, c } -> { a..., c, b } */
constexpr auto swap = bcompose (
    append,                        // t = { a..., b, c }
    Init<2>(),                     // t -> { a... }
    compose( reverse, RInit<2>() ) // t -> { b, c } -> { c, b }
);

/* nip : { a..., b, c } -> { a..., c } */
constexpr auto nip = bcompose( cons, Init<2>(), last );

/* over : { a..., b, c } -> { a..., b, c, b } */
constexpr auto over = bcompose( cons, id, RGet<1>() );

/* tuck : { a..., b, c } -> { a..., c, b, c } */
constexpr auto tuck = bcompose( cons, swap, last );

template< size_t N, class F, class T >
constexpr auto applyNary( F&& f, T&& t )
    -> decltype( cons( init<N>(forward<T>(t)),
                       apply(forward<F>(f),rinit<N>(forward<T>(t))) ) )
{
    return cons( init<N>(forward<T>(t)),
                 apply(forward<F>(f),rinit<N>(forward<T>(t))) );
}

template< size_t N > struct ApplyNary : Binary<ApplyNary<N>> {
    using Binary<ApplyNary<N>>::operator();

    template< class F, class T >
    constexpr auto operator () ( F&& f, T&& t )
        -> decltype( applyNary<N>( forward<F>(f), forward<T>(t) ) )
    {
        return applyNary<N>( forward<F>(f), forward<T>(t) );
    }
};

constexpr auto applyUnary  = ApplyNary<1>();
constexpr auto applyBinary = ApplyNary<2>();



/* APLICATION */

/* 
 * Completely flatten a tuple.
 * flatten( {1,{2},{{3}}} = {1,2,3}
 */
constexpr struct Flatten {
    static constexpr struct {
        template< class T >
        constexpr auto operator () ( T&& t ) 
            -> Result< Flatten, Result<decltype(level),T> >
        {
            // One level at a time.
            return Flatten()( level(forward<T>(t)) );
        }
    } level1{};

    // Flatten by one layer if tuple is recursive.
    static constexpr auto select = selectF< RecursiveTuple >( level1, id );

    // Recursively level t until flat.
    template< class T >
    constexpr auto operator () ( T&& t ) 
        -> Result< decltype(select), T >
    {
        return select( forward<T>(t) );
    }
} flatten{};

/* surround : {a...} x {c...} x b -> { a..., b, c... } */
constexpr struct Surround {
    template< class Pre, class X, class Post >
    constexpr auto operator () ( Pre&& pre, Post&& post, X&& x )
        -> decltype( append (
            forward<Pre>(pre),
            tuple( forward<X>(x) ),
            forward<Post>(post)
        ) )

    {
        return append (
            forward<Pre>(pre),
            tuple( forward<X>(x) ),
            forward<Post>(post)
        );
    }
} surround{};

/* Cut out the nth element, returning {before,after} */
template< size_t i, class T >
constexpr auto cutOut( T&& t )
    -> decltype( pair( take<i>(forward<T>(t)), drop<i+1>(forward<T>(t)) ) )
{
    return pair( take<i>(forward<T>(t)), drop<i+1>(forward<T>(t)) );
}

/* nth -- Apply f to the nth element of t. */
// Helper
template< size_t N, class F, class T >
constexpr auto _nth( const F& f, T&& t )
    -> decltype( std::tuple_cat (
        take<N>( forward<T>(t) ),  tuple( f(get<N>(forward<T>(t))) ),
        drop<N+1>(forward<T>(t))
    ) )
{
    // The tuple needs to be cut apart and repackaged because f may not return
    // the same as what it gets.
    return std::tuple_cat (
        take<N>( forward<T>(t) ),          // Before the nths,
        tuple( f(get<N>(forward<T>(t))) ), // the nts,
        drop<N+1>(forward<T>(t))           // and after.
    );
}

template< size_t N, class F, class T >
constexpr auto nth( const F& f, T&& t )
    -> typename std::enable_if <
        isTuple<T>(), decltype( _nth<N>(f,forward<T>(t)) )
    >::type
{
    return _nth<N>( f, forward<T>(t) );
}

// When the input is a pair, a pair must be returned!
template< size_t N, class F, class P >
constexpr auto nth( const F& f, P&& p )
    -> typename std::enable_if <
        isPair<P>(), decltype( apply( pair, _nth<N>(f, forward<P>(p)) ) )
    >::type
{
    // Inlines to pair(fst,snd).
    return apply( pair, _nth<N>(f, forward<P>(p)) );
}

template< size_t N > struct Nth : Binary<Nth<N>> {
    using Binary<Nth<N>>::operator();

    template< class F, class T >
    constexpr auto operator () ( const F& f, T&& t )
        -> decltype( nth<N>(f,forward<T>(t)) )
    {
        return nth<N>(f,forward<T>(t));
    }
};

/* Apply the nth where get<n>(t) is an X. */
template< class X, class F, class T,
          size_t i = iOfType<X,T>(), class Fi = Nth<i> >
constexpr auto xth( F&& f, T&& t )
    -> decltype( Fi()( forward<F>(f), forward<T>(t) ) )
{
    return Fi()( forward<F>(f), forward<T>(t) );
}

/* Apply the nth where get<n>(t) is similar to X. */
template< class X, class F, class T,
          size_t i = iOfSimilar<X,T>(), class Fi = Nth<i> >
constexpr auto sth( F&& f, T&& t )
    -> decltype( Fi()( forward<F>(f), forward<T>(t) ) )
{
    return Fi()( forward<F>(f), forward<T>(t) );
}

/*
 * Apply every function from the first tuple,
 * treating the other tuples as arguments.
 *
 * ap : {(a -> b)...} x {a...} -> {b...}
 * ap( {f,g}, {x,a}, {y,b} ) = { f(x,y), g(a,b) }
 */
constexpr struct ap : Binary<ap> {
    using Binary<ap>::operator();

    template< class TF, class TT = TTraits<TF>, class ...T >
    constexpr auto operator () ( const TF& tf, T&& ...t )
        -> decltype( applyTupleRows(TT::ctor,id,tf,forward<T>(t)...) )
    {
        // Note: id fi ti... = fi( ti... )
        return applyTupleRows( TT::ctor, id, tf, forward<T>(t)... );
    }
} ap{};

/* fan : a x (a -> b)... -> {b...} */
constexpr struct Fan {
    template< class X, class ...F >
    constexpr auto operator () ( const X& x, const F& ...f )
        -> decltype( tuple( f(x)... ) )
    {
        return tuple( f(x)... );
    }

    template< class X, class ...F >
    constexpr auto operator () ( const X& x, const std::tuple<F...>& tf )
        -> decltype( apply( (*this), rcons(tf,x) ) )
    {
        return apply( (*this), rcons(tf,x) );
    }
} fan{};

constexpr auto tcloset = compose( zipWith, closet );

constexpr auto concatZip = ncompose( concat, zipWith );

constexpr struct CrossImpl : Chainable<CrossImpl> {
    using Chainable<CrossImpl>::operator();

    template< class T, class U >
    constexpr auto operator () ( const T& t, const U& u )
        -> decltype( concatZip( rcloset(fan,tcloset(cons,t)), u ) )
    {
        return concatZip( rcloset(fan,tcloset(cons,t)), u );
    }
} crossImpl{};

constexpr struct Cross : Binary<Cross> {
    using Binary<Cross>::operator();
    template< class T, class U, class ...V >
    constexpr auto operator () ( const T& t, const U& u, const V& ...v )
        -> decltype( crossImpl( zipWith(tuple,t), u, v... ) )
    {
        return crossImpl( zipWith(tuple,t), u, v... );
    }
} cross{};

constexpr struct Map : Binary<Map> {
    using Binary<Map>::operator();

    template< class F, class T >
    constexpr auto operator () ( F&& f, T&& t )
        -> decltype( zipWith( forward<F>(f), forward<T>(t) ) )
    {
        return zipWith( forward<F>(f), forward<T>(t) );
    }

    template< class F, class T, class U, class ...V >
    constexpr auto operator () ( const F& f, const T& t, const U& u, const V& ...v )
        -> decltype( zipWith( apply(f), cross(t,u,v...) ) )
    {
        return zipWith( apply(f), cross(t,u,v...) );
    }
} map{};


constexpr struct MapTuple {
    static constexpr struct Impl {
        template< class TF, class X >
        constexpr auto operator () ( const TF& tf, const X& x )
            -> decltype( apply( fan, rcons(tf,x) ) )
        {
            return apply( fan, rcons(tf,x) );
        }
    } impl{};

    template< class TF, class T >
    constexpr auto operator () ( const TF& tf, const T& t )
        -> decltype ( 
            map(ncompose(apply(fan),rcons(tf)),t) 
        )
    {
        return map (
            ncompose( apply(fan), rcons(tf) ),
            t
        );
    }
} mapTuple{};


constexpr struct fanPair {
    template< class X, class ...F >
    constexpr auto operator () ( const X& x, const F& ...f )
        -> decltype( pair( f(x)... ) )
    {
        return pair( f(x)... );
    }
} fanPair{};

/* split : {a...} x (a -> b)... -> {b...} */
constexpr struct split {
    template< class T, class ...F >
    constexpr auto operator () ( T&& t, F&& ...f )
        -> decltype( ap( forwardTuple(forward<F>(f)...), forward<T>(t) ) )
    {
        return ap( forwardTuple(forward<F>(f)...), forward<T>(t) );
    }
} split{};

/*
 * fork : [a] x (a -> bool)... -> {[a]...}
 *
 * Returns a tuple, {leftovers,xs0,xs1,...,xsn}, where xsN is every x from xs
 * such that predN(x) is true. leftovers contains every x for which no
 * predicate is true.
 */
// Ref version.
constexpr struct Fork_ : Binary<Fork_> {
    using Binary<Fork_>::operator();

    template< class XS, class ...P >
    using Forked = decltype( repeat<sizeof...(P)>(XS()) );

    // First, convert ps... to a tuple.
    template< class XS, class ...P >
    Forked<XS,P...> operator () ( XS& xs, P&& ...ps ) const {
        return doFork( xs, std::forward_as_tuple(ps...) );
    }

    // Build the results.
    template< class XS, class ...P >
    static Forked<XS,P...> doFork( XS& xs, const std::tuple<P...>& ps ) {
        Forked<XS,P...> r;

        auto it = std::begin(xs);
        for( ; it != std::end(xs); ) {
            // doSendWhen will try to move *it when a predicate is matched.
            if( doSendWhen<0>( *it, r, ps ) )
                // Thusly, take out the moved element.
                it = xs.erase(it);
            else
                it++;
        }

        return r;
    }

    template< bool when >
    using EnableWhen = typename std::enable_if< when, bool >::type;

    // Check each predicate and send it to the correct resultant.
    // If nothing matched, send x to the leftovers.
    template< size_t N, class X, class Dsts, class Preds >
    static auto doSendWhen( const X&, Dsts&, const Preds& )
        -> EnableWhen< N >= size<Dsts>() >
    {
        return false; // No match.
    }

    template< size_t N, class X, class Dsts, class Preds >
    static auto doSendWhen( X& x, Dsts& dsts, const Preds& preds )
        -> EnableWhen<( N < size<Dsts>() )>
    {
        if( std::get<N>(preds)(x) ) {
            std::get<N>(dsts).push_back( std::move(x) );
            return true;
        } else {
            return doSendWhen<N+1>( x, dsts, preds );
        }
    }
} fork_{};

constexpr struct fork {
    template< class XS, class ...P >
    using Forked = decltype( repeat<sizeof...(P) + 1>(XS()) );

    // First, convert ps... to a tuple.
    template< class XS, class ...P >
    Forked<XS,P...> operator () ( XS xs, P&& ...ps ) const {
        auto forks = fork_( xs, forward<P>(ps)... );
        return rcons( move(forks), move(xs) );
    }
} fork{};

} // namespace tpl

} // namespace pure
