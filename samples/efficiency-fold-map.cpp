
#include "../Pure.h"

#include <vector>
#include <array>
#include <iostream>
#include <algorithm>

constexpr unsigned int fact( unsigned int x ) {
    return x > 1 ? x * fact(x-1) : 1;
}

using VUI = std::vector<unsigned int>;
using Arr = std::array<unsigned int,4>;

unsigned int headOnly( const VUI& v ) {
    // GCC will require mapping the whole vector, even if we throw out all but
    // the head. This shows it is better to write f( head(s) ) than
    // head(map(f,s)).
    //
    // It's worth noting that GCC inlines a non-recursive fact function, making
    // this function monstrous.
    using pure::list::map;
    using pure::list::head;
    return head( map( fact, v ) );
}

constexpr unsigned int headOnlyA( const Arr& a ) {
    using pure::list::map;
    using pure::list::head;
    return head( map( fact, a ) );
}

unsigned int sumAccum( const VUI& v ) {
    /* 
     * GCC produces a very fast, minimal loop here.
     *
     * _Z8sumAccumRKSt6vectorIjSaIjEE:
     *      movq    8(%rdi), %rcx # e = v.end()
     *      movq    (%rdi), %rdx  # it = v.begin()
     *      xorl    %eax, %eax    # acc = 0
     *      cmpq    %rdx, %rcx    # if (it == e) 
     *      je  .L20              #     return
     *
     * .L19:                      # do {
     *
     *      addl    (%rdx), %eax  #     acc += *it
     *      addq    $4, %rdx      #     it++
     *
     *      cmpq    %rdx, %rcx    # } while( it == e )
     *      jne .L19
     *
     *      rep 
     *      ret
     * .L20:
     *      rep
     *      ret
     */
    return std::accumulate( v.begin(), v.end(), 0 );
}

unsigned int sumAccumA( const Arr& a ) {
    return std::accumulate( a.begin(), a.end(), 0 );
}

unsigned int sumFold( const VUI& v ) {
    // GCC produces equivalent code here as sumAccum.
    using pure::list::foldl;
    using pure::Add;
    return foldl( Add(), 0, v );
}

unsigned int sumFold2( const VUI& v ) {
    // GCC is unable to create as optimal code here. When supplied no initial
    // fold value, it starts with the first element and inlines to
    // std::accumulate on v's tail. For whatever reasons, this produces a
    // larger and more complicated function.
    using pure::list::foldl;
    using pure::Add;
    return foldl( Add(), v );
}

constexpr unsigned int sumFoldA( const Arr& a ) {
    using pure::list::foldl;
    using pure::Add;
    return foldl( Add(), 0, a );
}

constexpr unsigned int sumFoldA2( const Arr& a ) {
    using pure::list::foldl;
    using pure::Add;
    return foldl( Add(), a );
}

/*
 * When operating on arrays, GCC uses vectorized instruction. For this
 * function, GCC uses a label in the text section:
 *
 * .LC2:
 *      .long   2
 *      .long   2
 *      .long   2
 *      .long   2
 *
 * and its use looks like this:
 *      movedqa .LC2(%rip), %xmm1 # Add the vector, xmm1, with LC2.
 */
constexpr unsigned plusTwo( const unsigned x ) { return x + 2; }

void p2Transform( VUI& v ) {
    /* 
     * Apparently, a transform is a very complex operation.
     *
     *      movq    (%rdi), %r9   # it = v.begin()
     *      movq    8(%rdi), %rdi # e = v.end()
     *      cmpq    %rdi, %r9     # if it==e
     * je   .L43                  #     return
     *      leaq    4(%r9), %rax  # 
     *      movq    %r9, %rcx
     *      movq    %rdi, %rsi
     *      andl    $15, %ecx
     *      subq    %rax, %rsi
     *      shrq    $2, %rcx
     *      movq    %r9, %rax
     *      shrq    $2, %rsi
     *      negq    %rcx
     *      addq    $1, %rsi
     *      andl    $3, %ecx
     *      cmpq    %rsi, %rcx
     *      cmova   %rsi, %rcx
     *      cmpq    $4, %rsi
     *      cmovbe  %rsi, %rcx
     *      testq   %rcx, %rcx
     * je   .L46
     *      xorl    %edx, %edx
     * .L47:
     *      addq    $1, %rdx
     *      addl    $2, (%rax)
     *      addq    $4, %rax
     *      cmpq    %rdx, %rcx
     *      ja  .L47
     *      cmpq    %rcx, %rsi
     *      je  .L61
     * .L46:
     *      movq    %rsi, %r11
     *      subq    %rcx, %r11
     *      movq    %r11, %r8
     *      shrq    $2, %r8
     *      leaq    0(,%r8,4), %r10
     *      testq   %r10, %r10
     *      je  .L49
     *      movdqa  .LC2(%rip), %xmm1
     *      leaq    (%r9,%rcx,4), %rsi
     *      xorl    %edx, %edx
     *      xorl    %ecx, %ecx
     * .L50:
     *      movdqa  (%rsi,%rdx), %xmm0
     *      addq    $1, %rcx
     *      paddd   %xmm1, %xmm0
     *      movdqa  %xmm0, (%rsi,%rdx)
     *      addq    $16, %rdx
     *      cmpq    %r8, %rcx
     *      jb  .L50
     *      cmpq    %r10, %r11
     *      leaq    (%rax,%r10,4), %rax
     *      je  .L43
     * .L49:
     *      movq    %rax, %rdx
     * .L52:
     *      movl    (%rdx), %ecx
     *      addq    $4, %rdx
     *      addl    $2, %ecx
     *      movl    %ecx, (%rax)
     *      addq    $4, %rax
     *      cmpq    %rdx, %rdi
     *      jne .L52
     * .L43:
     *      rep
     *      ret
     * .L61:
     *      ret
     */
    std::transform( v.begin(), v.end(), v.begin(), plusTwo );
}

void p2TransformA( Arr& a ) {
    // Same as p2Transform.
    std::transform( a.begin(), a.end(), a.begin(), plusTwo );
}

VUI p2Transform2( VUI v ) {
    std::transform( v.begin(), v.end(), v.begin(), plusTwo );
    return v;
}

Arr p2TransformA2( Arr a ) {
    /*
     * _Z13p2TransformA2St5arrayIjLm3EE:
     *      movq    %rdi, -40(%rsp)
     *      movq    %rsi, -32(%rsp)
     *      addl    $2, -40(%rsp)
     *      addl    $2, -36(%rsp)
     *      movq    -40(%rsp), %rax
     *      addl    $2, -32(%rsp)
     *      addl    $2, -28(%rsp)
     *      movq    -32(%rsp), %rdx
     *      ret
     */
    std::transform( a.begin(), a.end(), a.begin(), plusTwo );
    return a;
}

VUI p2Transform3( const VUI& v ) {
    VUI w;
    std::transform( v.begin(), v.end(), std::back_inserter(w), plusTwo );
    return w;
}

Arr p2TransformA3( const Arr& a ) {
    /*
     *_Z13p2TransformA3RKSt5arrayIjLm3EE:
     *      .cfi_startproc
     *      leaq    12(%rdi), %rsi
     *      leaq    4(%rdi), %rdx
     *      movq    %rdi, %rax
     *      movq    %rsi, %rcx
     *      subq    %rdx, %rcx
     *      movq    %rcx, %rdx
     *      shrq    $2, %rdx
     *      leaq    1(%rdx), %r9
     *      movq    %r9, %rcx
     *      shrq    $2, %rcx
     *      leaq    0(,%rcx,4), %r8
     *      testq   %r8, %r8
     *      je  .L110
     *      cmpq    $3, %r9
     *      jbe .L110
     *      movdqa  .LC2(%rip), %xmm1
     *      xorl    %eax, %eax
     *      xorl    %edx, %edx
     *      .p2align 4,,10
     *      .p2align 3
     *.L106:
     *      movdqu  (%rdi,%rax), %xmm0
     *      addq    $1, %rdx
     *      paddd   %xmm1, %xmm0
     *      movdqa  %xmm0, -40(%rsp,%rax)
     *      addq    $16, %rax
     *      cmpq    %rdx, %rcx
     *      ja  .L106
     *      leaq    0(,%r8,4), %rdx
     *      leaq    -40(%rsp), %rcx
     *      leaq    (%rdi,%rdx), %rax
     *      addq    %rcx, %rdx
     *      cmpq    %r8, %r9
     *      je  .L109
     *      .p2align 4,,10
     *      .p2align 3
     *.L111:
     *      movl    (%rax), %ecx
     *      addq    $4, %rax
     *      addl    $2, %ecx
     *      movl    %ecx, (%rdx)
     *      addq    $4, %rdx
     *      cmpq    %rax, %rsi
     *      jne .L111
     *.L109:
     *      movl    -32(%rsp), %edx
     *      movq    -40(%rsp), %rax
     *      ret
     *.L110:
     *      leaq    -40(%rsp), %rdx
     *      jmp .L111
     */
    Arr b;
    std::transform( a.begin(), a.end(), b.begin(), plusTwo );
    return b;
}

void p2Map( VUI& v ) {
    v = pure::list::map( plusTwo, std::move(v) );
}

void p2MapA( Arr& a ) {
    /*
     * p2TransformA produces quite a long function with a loop. GCC can
     * actually better optimize this version!
     *
     * _Z6p2MapARSt5arrayIjLm4EE:
     *      movdqu  (%rdi), %xmm0
     *      paddd   .LC2(%rip), %xmm0
     *      movdqu  %xmm0, (%rdi)
     *      ret
     */
    a = pure::list::map( plusTwo, std::move(a) );
}

VUI p2Map2( VUI v ) {
    // The produced code is similar to the transform version.  It appears to do
    // slightly less work at the start, but less at the end. 
    // (Probably inconsequential.)
    return pure::list::map( plusTwo, std::move(v) );
}

constexpr Arr p2MapA2( Arr a ) {
    return pure::list::map( plusTwo, std::move(a) );
}

VUI p2Map3( const VUI& v ) {
    // The produces assembly is longer here, but more inlined. The transform
    // version calls VUI::emplace_back, and _ZNSt6vectorIjSaIjEED1Ev
    // (whatever that is). This version calls _Znwm (allocate, I believe),
    // memmove, and _ZSt17__throw_bad_allocv on failure.
    return pure::list::map( plusTwo, v );
}

constexpr Arr p2MapA3( const Arr& a ) {
    return pure::list::map( plusTwo, a );
}

VUI append( VUI v, const VUI& w ) {
    std::copy( w.begin(), w.end(), back_inserter(v) );
    return v;
}

VUI foldAppend( VUI v, const VUI& w ) {
    // This is somewhat less efficient compared to the above.
    foldl( pure::list::Cons(), v, w );
    return v;
}

VUI pm2( unsigned x ) { return {x+2,x-2}; }

VUI pm2Copy( const VUI& v ) {
    VUI w;
    for( auto x : v ) {
        // GCC inlines this whole loop.
        auto m = pm2(x);
        std::copy( std::begin(m), std::end(m), std::back_inserter(w) );
    }
    return w;
}

VUI pm2Append( const VUI& v ) {
    VUI w;
    for( auto x : v ) 
        pure::list::append_( w, pm2(x) );
    return w;
}

VUI pm2Concat( const VUI& v ) {
    // Equivalent to pm2Copy.
    return pure::list::concatMap( pm2, v );
}

VUI forplusPlus( const VUI& v ) {
    VUI r;
    /* 
     * GCC optimizes plusTwo(plusTwo(x)) to x + 4.
     *
     *  	movl	(%rbx), %eax
     *      addl	$4, %eax
     *      cmpq	%rcx, %rdx
     *      movl	%eax, (%rsp)
     *
     */
    for( auto x : v ) 
        /* But fails to optimize the vector operation.
         *
         *  	jne	.L485
         *  	movq	%rsp, %rsi
         *  	movq	%rbp, %rdi
         *  .LEHB13:
         *  	call	_ZNSt6vectorIjSaIjEE19_M_emplace_back_auxIIjEEEvDpOT_
         *
         * It cannot tell then r will grow to v's size.
         */
        pure::list::cons_( r, plusTwo(plusTwo(x)) );
    return r;
}

VUI mapMap( const VUI& v ) {
    using pure::list::map;

    /*
     * GCC inlines the actual plus operations with the LC2 vector, but that
     * vector holds {2,2,2,2}. 
     *
     *      movq	%r8, %r14
     *      subq	%rcx, %r14
     *      movq	%r14, %r10
     *      shrq	$2, %r10
     *      leaq	0(,%r10,4), %r11
     *      testq	%r11, %r11
     *      je	.L230
     *      movdqa	.LC2(%rip), %xmm0 # twos = {2,2,2,2}
     *      leaq	0(%rbp,%rcx,4), %rsi # it = (base+c*4)
     *      xorl	%eax, %eax # a = 0
     *      xorl	%ecx, %ecx # c = 0
     *      .p2align 4,,10
     *      .p2align 3
     * .L231:                          # do {
     *      movdqa	(%rsi,%rcx), %xmm1 #    v = it[c]a // Load four.
     *      addq	$1, %rax           #    a++
     *      paddd	%xmm0, %xmm1       #    v += twos
     *      movdqa	%xmm1, (%rsi,%rcx) #    it[c] = v // Store four
     *      addq	$16, %rcx          #    c += sizeof int * 4
     *      cmpq	%r10, %rax         # } while( a < r10 )
     *      jb	.L231                  #
     *      cmpq	%r11, %r14
     *      leaq	(%rdx,%r11,4), %rdx
     *      je	.L229
     *
     * This snippet appears twice in the generated code. That GCC cannot
     * convert this to forplusPlus is not surprising; it cannot implicitly
     * convert a two-pass algorithm to one-pass.
     *
     * The more general, non vectorized version of the +2 loop seen in
     * forplusPlus also appears twice as well, presumably for the end of the
     * array when it can't load 4 any more.
     */
    return map (
        plusTwo,
        map( plusTwo, v )
    );
}

VUI transTrans( const VUI& v ) {
    // This produces similar code to forplusPlus, but with mapMap's vectorized
    // optimizations. It does not inline the push_back.
    VUI r;
    std::transform( v.begin(), v.end(), std::back_inserter(r), plusTwo );
    std::transform( r.begin(), r.end(), r.begin(), plusTwo );
    return r;
}

VUI mapCompose( const VUI& v ) {
    /*
     * GCC does not inline plusTwo here, but it does recognize that the size of
     * the result will be that of v, so insertion operations are optimized out,
     * as opposed to the forplusPlus example.
     *
     * .L190:                     # do {
     *  	movl	(%rbx), %edi  #     d = *it
     *  	call	_Z7plusTwoj   #     a = plusTwo(d)
     *  	movl	%eax, %edi    #     d = a
     *  	call	_Z7plusTwoj   #     a = plusTwo(d)
     *  	movl	%eax, (%rbx)  #     *it = a
     *  	addq	$4, %rbx      #     it++
     *  	cmpq	%rbx, %rbp    # } while( it != end )
     *  	jne	.L190
     *
     * The trade off here is not optimizing plusTwo, but the advantage is
     * optimizing the loop.
     */
    return pure::list::map (
        pure::compose( plusTwo, plusTwo ),
        v
    );
}

std::ostream& operator << ( std::ostream& os, const VUI& s ) {
    os << '{';
    for( const auto& x : s )
        os << x << ' ';
    os << '}';
    return os;
}
std::ostream& operator << ( std::ostream& os, const Arr& s ) {
    os << '{';
    for( const auto& x : s )
        os << x << ' ';
    os << '}';
    return os;
}

int main() {
    using std::cout;
    using std::endl;

    cout << headOnly( {1,2,3,4} ) << endl;

    // GCC does not produce an assembly version of headOnlyA, but it does
    // inline it here, which shows that even though it has been labelled
    // constexpr, it isn't really. However, perhaps being constexpr makes it
    // easier to inline.
    cout << headOnlyA( {{1,2,3,4}} ) << endl;

    
    /*
     * GCC is find with sumFoldA.
     *
     *      movl    $1234, %esi
     *      movl    $_ZSt4cout, %edi
     *      movl    $1000, 544(%rsp)
     *      movl    $200, 548(%rsp)
     *      movl    $30, 552(%rsp)
     *      movl    $4, 556(%rsp)
     *      call    _ZNSo9_M_insertImEERSoT_
     *      movq    %rax, %rdi
     *      call    _ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_
     *
     * It still insists on creating the array, but just calls inserts 1234 to
     * cout anyway.
     */
    cout << sumFoldA( {{1000,200,30,4}} ) << endl;
    // It seems to have a problem with sumFoldA2 as the inlined code it
    // generates spans several pages. sumAccumA produces a very small loop!
    cout << sumFoldA2( {{1000,200,30,4}} ) << endl;

    // Equivalent to sumFoldA.
    cout << sumAccumA( {{1000,200,30,4}} ) << endl;

    // Prevent GCC from optimizing out the fallowing functions entirely.
    Arr arr;
    for( int i=0; i < 4; i++ )
        std::cin >> arr[i];

    /*
     * Fold is optimal!
     * movl 20(%rsp), %esi # esi = arr[1]
     * addl 16(%rsp), %esi # esi += arr[0]
     * addl 24(%rsp), %esi # esi += arr[2]
     */
    cout << sumFoldA( arr ) << endl;
    // Again, sumFoldA2 produces an odd loop.
    cout << sumFoldA2( arr ) << endl;
    // Same as fold.
    cout << sumAccumA( arr ) << endl;

    /*
     * GCC can optimize out the transform.
     *      leaq    16(%rsp), %rdi
     *      movdqa  .LC2(%rip), %xmm0 # Note: LC2 = {2,2,2,2}
     *      paddd   16(%rsp), %xmm0
     *      movdqa  %xmm0, 16(%rsp)
     */
    p2TransformA( arr );
    p2MapA( arr ); // Not inlined.
    cout << arr << endl;

    /*
     * GCC inlines the p2TransformA2 here.
     *
     *      movdqa  .LC2(%rip), %xmm0
     *      leaq    304(%rsp), %rsi
     *      movl    $1, 48(%rsp)
     *      movl    $2, 52(%rsp)
     *      movl    $_ZSt4cout, %edi
     *      movl    $3, 56(%rsp)
     *      movl    $4, 60(%rsp)
     *      paddd   48(%rsp), %xmm0
     *      movdqa  %xmm0, 48(%rsp)
     *      movq    48(%rsp), %rax
     *      movq    %rax, 304(%rsp)
     *      movq    56(%rsp), %rax
     *      movq    %rax, 312(%rsp)
     */
    cout << p2TransformA2( {{1,2,3,4}} ) << endl;
    /*
     * But GCC does this one at compile time!
     *
     *      leaq    304(%rsp), %rsi
     *      movl    $_ZSt4cout, %edi
     *      movl    $3, 304(%rsp)
     *      movl    $4, 308(%rsp)
     *      movl    $5, 312(%rsp)
     *      movl    $6, 316(%rsp)
     */
    cout << p2MapA2( {{1,2,3,4}} ) << endl;
    // Same as transform for the rest.
    cout << p2TransformA2( arr ) << endl;
    cout << p2MapA2( arr ) << endl;
    cout << p2TransformA3( arr ) << endl;
    cout << p2MapA3( arr ) << endl; 

    VUI v = { 2, 4, 8, 10 };
    VUI w = { 1, 3, 5, 7  };
    cout << append(v,w) << endl;
    cout << foldAppend(v,w) << endl;
}
