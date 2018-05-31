#include <iostream>
#include <vector>

using namespace std;

int main()
{
    int i;

    if (((i = 0) = 1)) {
        cout << i << "ok" << endl;
    }

    try {
        throw runtime_error("error test");
    } catch (runtime_error err) {
        cout << err.what() << endl;
    }

    return 0;
}
