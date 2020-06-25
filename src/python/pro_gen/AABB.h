#include "Pos.h"
#include <string>
using namespace std;

class AABB{

    private:
        int id;
        string material;
        Pos topLeft;
        Pos bottomRight;

    public:
        int getID();
        string getMaterial();
        Pos getTopLeft();
        Pos getBottomRight();
        int getMidpointX();
        int getMidpointY();
        int getMidpointZ();
        Pos getRandomPosAtBase();
        AABB(int, string, Pos, Pos);
        ~AABB();
};