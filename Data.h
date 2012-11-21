
#pragma once

#include <memory>

namespace pure {

namespace data {

template< template<class...> class Ptr >
struct ConstructPtr {
    template< class X, class _X = Decay<X> >
    Ptr<_X> operator () ( X&& x ) const {
        return Ptr<_X>( new _X(std::forward<X>(x)) );
    }
};

constexpr auto Just  = ConstructPtr<std::unique_ptr>();
constexpr auto Share = ConstructPtr<std::shared_ptr>();

template< class T, class M = std::unique_ptr<T> > 
constexpr M Nothing() {
    return nullptr;
}

template< class T > struct ReturnNothing {
    constexpr auto operator () () -> decltype( Nothing<T>() ) {
        return Nothing<T>();
    }
};

/* maybe b (a->b) (Maybe a) -> b */
template< class R, class F, class P >
constexpr R maybe( R&& nothingVal, F&& f, P&& m ) {
    return m ? forward<F>(f)( *forward<P>(m) )
             : forward<R>( nothingVal );
}

/* Either a b : Left a | Right b */
template< class L, class R >
struct Either
{
    typedef L left_type;
    typedef R right_type;

    std::unique_ptr<left_type> left;
    std::unique_ptr<right_type> right;

    // TODO: Make these type constructors, not types.
    struct Left { 
        left_type value;
        constexpr Left( left_type value ) : value(move(value)) { }
    };

    struct Right { 
        right_type value;
        constexpr Right( right_type value ) : value(move(value)) { }
    };

    template< class X >
    explicit constexpr Either( X&& x )  
        : left(  new left_type (move(x.value)) ) { }
    explicit constexpr Either( Right x ) 
        : right( new right_type(move(x.value)) ) { }
};

/* 
 * Left<b>  a -> Either a b
 * Right<a> b -> Either a b
 * Since an Either cannot be constructed without two type arguments, the
 * 'other' type must be explicitly declared when called. It is, for
 * convenience, the first type argument in both.
 */
template< class R, class L, class E = Either<L,R> >
constexpr E Left( L x ) { return E( typename E::Left(move(x)) ); }

template< class L, class R, class E = Either<L,R> >
constexpr E Right( R x ) { return E( typename E::Right(move(x)) ); }

/* either (a->c) (b->c) (Either a b) -> c */
template< class F, class G, class L, class R >
constexpr auto either( F&& f, G&& g, const Either<L,R>& e )
    -> decltype( declval<F>()(*e.right) )
{
    return e.right ? forward<F>(f)(*e.right) : forward<G>(g)(*e.left);
}

/*
 * Right f * Right x = Right (f x)
 * _       * _       = Left _
 *
 * TODO: Replace this with an instantiation in Applicative.
 */
template< class L, class F, class T, 
          class Ret = decltype( declval<F>()(declval<T>()) ) >
constexpr Either<L,Ret> operator* ( const Either<L,F>& a, 
                                    const Either<L,T>& b )
{
    return a.right and b.right ? Right<L>( (*a.right)(*b.right) )
        : a.left ? Left<Ret>( *a.left ) : Left<Ret>( *b.left );
}

} // namespace data

} // namespace pure
