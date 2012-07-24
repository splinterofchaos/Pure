
#include "Pure.h"

#include <cstdio>
#include <cmath>
#include <vector>

using namespace std;
using namespace pure;

int five() { return 5; }
int times_two(int x) { return x * 2; }

int main()
{
    // Fold left and right work.
    printf (
        "sum of (1,2,3,4) = %d\n", // = 10
        foldl<int>( std::plus<int>(), vector<int>{1,2,3,4} )
    );
    printf (
        "sum of (4,3,2,1) = %d\n", // = 10
        foldr<int>( std::plus<int>(), vector<int>{1,2,3,4} )
    );
}
