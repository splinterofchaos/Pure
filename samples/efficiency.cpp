
#include "../Pure.h"

#include <vector>
#include <array>
#include <iostream>
#include <algorithm>

constexpr unsigned int fact( unsigned int x ) {
    return x > 1 ? x * fact(x-1) : 1;
}

using VUI = std::vector<unsigned int>;
using Arr = std::array<unsigned int,3>;

unsigned int headOnly( const VUI& v ) {
    // GCC will require mapping the whole vector, even if we throw out all but
    // the head. This shows it is better to write f( head(s) ) than
    // head(map(f,s)).
    using pure::list::map;
    using pure::list::head;
    return head( map( fact, v ) );
}

unsigned int sumAccum( const VUI& v ) {
    /* 
     * GCC produces a very fast, minimal loop here.
     *
     * _Z8sumAccumRKSt6vectorIjSaIjEE:
     *      movq	8(%rdi), %rcx # e = v.end()
     *      movq	(%rdi), %rdx  # it = v.begin()
     *      xorl	%eax, %eax    # acc = 0
     *      cmpq	%rdx, %rcx    # if (it == e) 
     *      je	.L20              #     return
     *
     * .L19:                      # do {
     *
     *      addl	(%rdx), %eax  #     acc += *it
     *      addq	$4, %rdx      #     it++
     *
     *      cmpq	%rdx, %rcx    # } while( it == e )
     *      jne	.L19
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
    /*
     * std::accumulate is not a constexpr, so GCC compiles the fallowing:
     *
     * _Z9sumAccumARKSt5arrayIjLm3EE:
     *                             # rdi = it = a.begin()
     *      leaq	12(%rdi), %rdx # e = a.end()
     *      xorl	%eax, %eax     # acc = 0
     *      cmpq	%rdi, %rdx     # if it == e:
     * je	.L25                   #    return
     *
     * .L24:                       # do {
     *      addl	(%rdi), %eax   #    acc += *it
     *      addq	$4, %rdi       #    it++
     *      cmpq	%rdi, %rdx     # } while( it != e )
     *      jne	.L24
     *      rep
     *      ret
     * .L25:
     *      rep
     *      ret
     *
     * It is similar to sumAccum, except in that the array is on the stack.
     *
     * Even though an assembly version is produced, GCC still computes the
     * result at compile time when a is a constexpr.
     */
    return std::accumulate( a.begin(), a.end(), 0 );
}

unsigned int sumFold( const VUI& v ) {
    // GCC produces equivalent code here as sumAccum.
    using pure::list::foldl;
    using pure::Add;
    return foldl( Add(), 0, v );
}

unsigned int sumFold2( const VUI& v ) {
    // GCC is unable to create as optimal code here. 
    // Or, if it is optimal, it is much larger and harder to understand.
    using pure::list::foldl;
    using pure::Add;
    return foldl( Add(), v );
}

constexpr unsigned int sumFoldA( const Arr& a ) {
    // Despite the two-arg version being less optimal, GCC inlines this
    // function optimally.
    using pure::list::foldl;
    using pure::Add;
    return foldl( Add(), a );
}

unsigned plusTwo( unsigned x ) { return x + 2; }

void p2Transform( VUI& v ) {
    /* 
     * Apparently, a transform is a very complex operation.
     *
     *      movq	(%rdi), %r9   # it = v.begin()
     *      movq	8(%rdi), %rdi # e = v.end()
     *      cmpq	%rdi, %r9     # if it==e
     * je	.L43                  #     return
     *      leaq	4(%r9), %rax  # 
     *      movq	%r9, %rcx
     *      movq	%rdi, %rsi
     *      andl	$15, %ecx
     *      subq	%rax, %rsi
     *      shrq	$2, %rcx
     *      movq	%r9, %rax
     *      shrq	$2, %rsi
     *      negq	%rcx
     *      addq	$1, %rsi
     *      andl	$3, %ecx
     *      cmpq	%rsi, %rcx
     *      cmova	%rsi, %rcx
     *      cmpq	$4, %rsi
     *      cmovbe	%rsi, %rcx
     *      testq	%rcx, %rcx
     * je	.L46
     *      xorl	%edx, %edx
     * .L47:
     *      addq	$1, %rdx
     *      addl	$2, (%rax)
     *      addq	$4, %rax
     *      cmpq	%rdx, %rcx
     *      ja	.L47
     *      cmpq	%rcx, %rsi
     *      je	.L61
     * .L46:
     *      movq	%rsi, %r11
     *      subq	%rcx, %r11
     *      movq	%r11, %r8
     *      shrq	$2, %r8
     *      leaq	0(,%r8,4), %r10
     *      testq	%r10, %r10
     *      je	.L49
     *      movdqa	.LC2(%rip), %xmm1
     *      leaq	(%r9,%rcx,4), %rsi
     *      xorl	%edx, %edx
     *      xorl	%ecx, %ecx
     * .L50:
     *      movdqa	(%rsi,%rdx), %xmm0
     *      addq	$1, %rcx
     *      paddd	%xmm1, %xmm0
     *      movdqa	%xmm0, (%rsi,%rdx)
     *      addq	$16, %rdx
     *      cmpq	%r8, %rcx
     *      jb	.L50
     *      cmpq	%r10, %r11
     *      leaq	(%rax,%r10,4), %rax
     *      je	.L43
     * .L49:
     *      movq	%rax, %rdx
     * .L52:
     *      movl	(%rdx), %ecx
     *      addq	$4, %rdx
     *      addl	$2, %ecx
     *      movl	%ecx, (%rax)
     *      addq	$4, %rax
     *      cmpq	%rdx, %rdi
     *      jne	.L52
     * .L43:
     *      rep
     *      ret
     * .L61:
     *      ret
     */
    std::transform( v.begin(), v.end(), v.begin(), plusTwo );
}

void p2TransformA( Arr& a ) {
    // GCC cannot easily optimize this.
    std::transform( a.begin(), a.end(), a.begin(), plusTwo );
}

VUI p2Transform2( VUI v ) {
    std::transform( v.begin(), v.end(), v.begin(), plusTwo );
    return v;
}

Arr p2TransformA2( Arr a ) {
    /*
     * _Z13p2TransformA2St5arrayIjLm3EE:
     *      movq	%rdi, -40(%rsp)
     *      movl	%esi, -32(%rsp)
     *      addl	$2, -40(%rsp)
     *      addl	$2, -36(%rsp)
     *      movq	-40(%rsp), %rax
     *      addl	$2, -32(%rsp)
     *      movl	-32(%rsp), %edx
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
     * actually better optimize this version.
     *
     * _Z6p2MapARSt5arrayIjLm3EE:
     *      movq	(%rdi), %rax
     *      movq	%rax, -72(%rsp)
     *      movl	8(%rdi), %eax
     *      addl	$2, -72(%rsp)
     *      addl	$2, -68(%rsp)
     *      movl	%eax, -64(%rsp)
     *      movq	-72(%rsp), %rax
     *      addl	$2, -64(%rsp)
     *      movq	%rax, (%rdi)
     *      movl	-64(%rsp), %eax
     *      movl	%eax, 8(%rdi)
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

Arr p2MapA2( Arr a ) {
    // Equivalent to transform version.
    return pure::list::map( plusTwo, std::move(a) );
}

VUI p2Map3( const VUI& v ) {
    // The produces assembly is longer here, but more inlined. The transform
    // version calls VUI::emplace_back, and _ZNSt6vectorIjSaIjEED1Ev
    // (whatever that is). This version calls _Znwm (allocate, I believe),
    // memmove, and _ZSt17__throw_bad_allocv on failure.
    return pure::list::map( plusTwo, v );
}

Arr p2MapA3( const Arr& a ) {
    // Equivalent to transform version.
    return pure::list::map( plusTwo, a );
}

int main() {
    using std::cout;
    using std::endl;

    cout << headOnly( {1,2,3,4} ) << endl;

    /*
     * GCC computes both of these calls at compile-time.
     *      movl	$123, %esi 
     */
    cout << sumFoldA( {{100,20,3}} ) << endl;
    cout << sumAccumA( {{100,20,3}} ) << endl;

    std::array<unsigned,3> arr;
    for( int i=0; i < 3; i++ )
        std::cin >> arr[i];

    /*
     * In both cases here, GCC produces an inlined version of the fold.
     *
     * movl	4(%rsp), %esi # esi = arr[1]
     * addl	(%rsp), %esi  # esi += arr[0]
     * addl	8(%rsp), %esi # esi += arr[2]
     */
    cout << sumFoldA( arr ) << endl;
    cout << sumAccumA( arr ) << endl;

    VUI v = { 2, 4, 8, 10 };
    p2Transform( v );
    p2Map( v );
}
