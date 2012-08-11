
#include "../Pure.h"

#include <SDL/SDL.h>
#include <SDL/SDL_opengl.h>

#include <utility>
#include <string>
#include <cstdio>
#include <cmath>
#include <random>

bool quit = false;

GLenum init_gl( int w, int h )
{
    glClearColor( 0.0f, 0.0f, 0.0f, 0.0f );

    glMatrixMode( GL_PROJECTION );
    glLoadIdentity();
    glOrtho( 0, w, h, 0, -10, 10 );

    glMatrixMode( GL_MODELVIEW );
    glLoadIdentity();

    glEnable( GL_BLEND );
    glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    return glGetError();
}

enum { SCREEN_HEIGHT = 600,
       SCREEN_WIDTH  = 600 };

bool resize_window( float w_, float h_ )
{
    float w = w_, h = h_;
    float ratio = (float)SCREEN_HEIGHT / SCREEN_WIDTH;

    if( !SDL_SetVideoMode( w, h, 32, SDL_OPENGL|SDL_RESIZABLE ) )
        return false;

    if( w*ratio > h ) 
        // h is the limiting factor.
        w = h / ratio;
    else
        h = w * ratio;

    float wOff = ( w_ - w ) / 2;
    float hOff = ( h_ - h );

    glViewport( wOff, hOff, w, h );
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glOrtho( -1, 1, 1, -1, -10, 10 );
    glMatrixMode(GL_MODELVIEW);

    return true;
}

bool make_sdl_gl_window( int w, int h )
{
    init_gl( w, h );
    resize_window( w, h );

    return true;
}

void update_screen()
{
    SDL_GL_SwapBuffers();
    glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );
}

template< class F, class Val=unsigned int >
struct Incf
{
    typedef Val value_type;

    F f;
    value_type step, n;

    Incf( const F& f, value_type step = value_type(1) ) 
        : f(f), step(step), n(value_type()) 
    {
    }

    decltype(f(n)) operator() () { 
        auto x = f( n );
        n += step;
        return x;
    }
};

template< class F, class Val = unsigned int, class I = Incf<F,Val> >
I incf( F&& f, Val&& step ) {
    return I( std::forward<F>(f), std::forward<Val>(step) );
}

typedef std::pair<float,float> Vec;
constexpr Vec vec( float x, float y ) { return Vec(x,y); }
constexpr float get_x( const Vec& v ) { return v.first; }
constexpr float get_y( const Vec& v ) { return v.second; }
void set_x( Vec& v, float x ) { v.first = x; }
void set_y( Vec& v, float y ) { v.second = y; }

float times( float x, float y ) { return x * y; }

Vec operator* ( const Vec& v, float x ) {
    return pure::fmap( pure::partial(times,x), Vec(v) );
}

Vec operator/ ( const Vec& v, float x ) {
    return v * (1/x);
}
Vec operator/ ( float x, const Vec& v ) {
    return Vec(x/get_x(v), x/get_y(v));
}

constexpr Vec operator+ ( const Vec& a, const Vec& b ) {
    return Vec( a.first+b.first, a.second+b.second );
}

constexpr Vec operator- ( const Vec& a, const Vec& b ) {
    return Vec( a.first-b.first, a.second-b.second );
}

Vec operator- ( const Vec& v ) {
    return pure::fmap( std::negate<float>(), Vec(v) );
}

constexpr Vec vecFromAngle( float a ) {
    return Vec( std::cos(a), std::sin(a) );
}


constexpr float sqr( float x ) { return x * x; }

constexpr float len_sqr( const Vec& v ) { 
    return sqr( get_x(v) ) + sqr( get_y(v) );
}
constexpr float len( const Vec& v ) {
    return std::sqrt( len_sqr(v) );
}

std::string show( int x ) {
    char digits[10];
    sprintf( digits, "%d", x );
    return digits;
}

std::string show( float x ) {
    char digits[10];
    sprintf( digits, "%.2f", x );
    return digits;
}

std::string show( const Vec& v ) {
    return "<" + show(get_x(v)) + " " + show(get_y(v)) + ">";
}

constexpr auto vert = pure::join( glVertex2f, get_x, get_y );

constexpr float CIRCUMFERENCE = 2 * 3.145; // with r=1.
constexpr unsigned int STEPS = 25;
auto unitCircle = 
    pure::generate( incf(vecFromAngle,CIRCUMFERENCE/STEPS), STEPS+1 ); 

constexpr bool in_range( float x, float min, float max ) {
    return x >= min and x <= max;
}

struct Smily {
    float left, right, y;
    float scale;
};

Vec left(  const Smily& s ) { return Vec(s.left, s.y); }
Vec right( const Smily& s ) { return Vec(s.right,s.y); }

Smily smily( const Vec& v, float scale, bool down ) {
    Vec off = vec(0.45,-0.15);
    float newScale;
    constexpr float DS = 0.45;

    if( down ) {
        newScale = scale*DS;
    } else {
        off = -off;
        newScale = scale/DS;
    }

    Vec o = off * scale;
    return { get_x(v)+get_x(o), get_x(v)-get_x(o), 
             get_y(v)+get_y(o), newScale };
}

void paint_face( const Vec& v, float scale = 1 )
{
    auto rng = pure::partial( pure::rot(in_range), -3, 3 );

    float a = 0.2/scale;
    glColor3f( get_x(v)/len_sqr(v), std::cos(a)/2 + 0.5, std::sin(a)/2 + 0.5 );

    glBegin( GL_QUAD_STRIP );
    for( const auto& p : unitCircle ) {
        vert( p*scale + v );
        vert( p*0.98f*scale + v );
    }
    glEnd(); 

    glBegin( GL_QUAD_STRIP );
    for( float x = -0.7f; x <= 0.7f; x += 0.15f ) {
        float y = -(x*x)*0.8 + 0.75;
        vert( Vec(x,y)*scale + v );
        vert( Vec(x,y*1.2)*scale + v );
    }
    glEnd(); 

    float diameter = scale * 2;
    if( diameter > 0.03 ) {
        Smily s = smily( v, scale, true );
        paint_face( left(s),  s.scale );
        paint_face( right(s), s.scale );
    }
}



int main()
{
    if( SDL_Init( SDL_INIT_EVERYTHING ) < 0 )
        return 1;
    make_sdl_gl_window( 600, 600 );

    enum KeyState{ NOT_PRESSED=0, PRESSED=1 };
    auto keys = pure::generate( pure::pure(NOT_PRESSED), (unsigned)SDLK_LAST );

    Vec pos( 0, 0 );
    float scale = 1;

    std::random_device rd;
    std::mt19937 gen( rd() );

    std::bernoulli_distribution rnd;
    
    while( not quit )
    {
        Uint8* keyEvents = SDL_GetKeyState( 0 );
        
        static SDL_Event event;
		while( SDL_PollEvent(&event) )
		{
            switch( event.type ) 
            {
              case SDL_QUIT: quit = true; break;

              case SDL_KEYDOWN: keys[event.key.keysym.sym] = PRESSED; break;
              case SDL_KEYUP:   keys[event.key.keysym.sym] = NOT_PRESSED; break;

              case SDL_VIDEORESIZE:
                float w=event.resize.w, h=event.resize.h;
                resize_window( w, h );
                break;
            }
        }

        float SPEED = 0.003f;
        Vec off;
        if( keys[SDLK_LEFT] )
            off.first = SPEED;
        if( keys[SDLK_RIGHT] )
            off.first = -SPEED;
        if( keys[SDLK_UP] )
            off.second = SPEED;
        if( keys[SDLK_DOWN] )
            off.second = -SPEED;

        pos = pos + off;

        float dscale = 0;
        if( keys[SDLK_EQUALS] )
            dscale = SPEED;
        if( keys[SDLK_MINUS] )
            dscale = -SPEED;

        if( dscale != 0 ) {
            float newScale = dscale*scale + scale;
            pos = pos + pos*dscale;
            scale = newScale;
        }

        if( scale < 1 ) {
            Smily s = smily( pos, scale, false );

            Vec dO; // Change in origin.
            if( rnd(gen) )
                dO = right(s);
            else
                dO = left(s);

            pos = dO + (pos - dO)*(1 - s.scale);
            scale = s.scale;
        }

        paint_face( pos, scale );

        update_screen();
    }
}
