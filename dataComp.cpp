#include <vector>
#include <algorithm>
#include <cmath>
#include <string>
#include <iostream>
#include <limits>
#include <unordered_map>
#include "dataComp.h"


using namespace std;

void Pull::printIDs()
{
    string player_name, playerID;
    cin >> player_name >> playerID;
    string first, last;
    int ID;
    cin >> first >> last >> ID;
    string yay = first + last;
    cout << ID << "," << yay << endl;
    int currID = ID;
    while(cin >> first >> last >> ID)
    {
        if(currID == ID)
        {
            continue;
        }
        else
        {
            currID = ID;
            string name = first + last;
            cout << ID << "," << name << "\n";
        }
        
    }
}