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
        int getBlockX();
        int getBlockY();
        int getBlockZ();
        Block(string, string, Pos);
        ~Block();

};