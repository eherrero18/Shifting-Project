#include <vector>
#include <algorithm>
#include <cmath>
#include <string>
#include <iostream>
#include "dataComp.h"
#include <iomanip>

using namespace std;



int main()
{
    std::ios_base::sync_with_stdio(false);
    cout << std::setprecision(2); //Always show 2 decimal places
    cout << std::fixed;
    Pull letsgo;
    letsgo.read_in();
    letsgo.organize();
    letsgo.print();
}