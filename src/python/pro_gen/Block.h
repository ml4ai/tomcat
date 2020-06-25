#include <string>
#include "Pos.h"

using namespace std;
class Block{

    private:
        string name;
        string material;
        Pos pos;
    
    public:
        string getName();
        string getMaterial();
        int getX();
        int getY();
        int getZ();
        Block(string, string, Pos);
        ~Block();

};