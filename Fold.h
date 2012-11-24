
#include "Monoid.h"
#include "Pure.h"

namespace pure {

namespace fold {

template< class ... > struct Foldable;

template< class X, class F = Foldable< Cat<X> > >
auto fold( X&& x ) -> decltype( F::fold(declval<X>()) ) {
    return F::fold( forward<X>(x) );
}

template< class F, class X, class ...Y, class Fo = Foldable< Cat<X> > >
auto foldMap( F&& f, X&& x, Y&& ...y ) 
    -> decltype( Fo::foldMap(declval<F>(),declval<X>(),declval<Y>()...) ) 
{
    return Fo::foldMap( forward<F>(f), forward<X>(x), forward<Y>(y)... );
}

template< class F, class X, class Fo = Foldable< Cat<X> > >
auto foldl( F&& f, X&& x ) 
    -> decltype( Fo::foldl(declval<F>(), declval<X>()) ) 
{
    return Fo::foldl( forward<F>(f), forward<X>(x) );
}

template< class F, class X, class Y, class ...Z, 
          class Fo = Foldable< Cat<Y> > >
auto foldl( F&& f, X&& x, Y&& y, Z&& ...z ) 
    -> decltype( Fo::foldl(declval<F>(),
                           declval<X>(),declval<Y>(),declval<Z>()...) ) 
{
    return Fo::foldl( forward<F>(f), 
                      forward<X>(x), forward<Y>(y), forward<Z>(z)... );
}

template< class F, class X, class Fo = Foldable< Cat<X> > >
auto foldr( F&& f, X&& x ) 
    -> decltype( Fo::foldr(declval<F>(), declval<X>()) ) 
{
    return Fo::foldr( forward<F>(f), forward<X>(x) );
}


template< class F, class X, class Y, class ...Z, 
          class Fo = Foldable< Cat<Y> > >
auto foldr( F&& f, X&& x, Y&& y, Z&& ...z ) 
    -> decltype( Fo::foldr(declval<F>(),
                           declval<X>(),declval<Y>(),declval<Z>()...) ) 
{
    return Fo::foldr( forward<F>(f), 
                      forward<X>(x), forward<Y>(y), forward<Z>(z)... );
}

template<> struct Foldable< category::sequence_type > {
    template< class S > using Val = list::SeqVal<S>;
    template< class S > using Ref = list::SeqRef<S>;

    template< class S, class X = Val<S> > static
    X fold( const S& s ) {
        using namespace monoid;
        return list::foldr( mappend, mempty<X>(), s );
    }

    template< class F, class S, class X = Val<S>,
              class R = Result<F,X> > 
    static R foldMap( F&& f, const S& s ) {
        using namespace monoid;
        return list::foldr( compose( mappend, forward<F>(f) ),
                            mempty<R>(), s );
    }

    template< class F, class XS, class YS, class ...ZS,
              class R = decltype (
                  declval<F>()( declval<Ref<XS>>(), declval<Ref<YS>>(),
                                declval<Ref<ZS>>()... )
              ) >
    static R foldMap( F&& f, const XS& xs, const YS& ys, const ZS& ...zs ) {
        using namespace monoid;
        R r = mempty<R>();
        for( const auto& x : xs )
            r = mappend( move(r),
                         foldMap( closure(forward<F>(f),x), 
                                  ys, zs... ) );
        return r;
    }

    template< class F, class ...X > static
    auto foldl( F&& f, X&& ...x ) 
        -> decltype( list::foldl(declval<F>(),declval<X>()...) ) 
    {
        return list::foldl( forward<F>(f), forward<X>(x)... );
    }

    template< class F, class ...X > static
    auto foldr( F&& f, X&& ...x ) 
        -> decltype( list::foldr(declval<F>(),declval<X>()...) ) 
    {
        return list::foldr( forward<F>(f), forward<X>(x)... );
    }
};

template<> struct Foldable< category::maybe_type > {
    template< class F, class X, class M >
    static Decay<X> foldr( F&& f, X&& x, M&& m ) {
        return not m ? forward<X>(x)
            : forward<F>(f)( *forward<M>(m), forward<X>(x) );
    }
    template< class F, class X, class M >
    static Decay<X> foldl( F&& f, X&& x, M&& m ) {
        return foldr( flip(forward<F>(f)), forward<X>(x), forward<M>(m) );
    }
};


}

}
