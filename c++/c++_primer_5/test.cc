#include <iostream>
#include <vector>
#include <pthread.h>

using namespace std;

int main()
{
    string s;

    vector<int> v = {1, 2, 3, 4};
    int &i = v.at(0);

    i = 0;
    int *p = &i;
    *p = 5;

    for (auto &i : v) {
        cout << ++i << endl;
    }

    return 0;
}
