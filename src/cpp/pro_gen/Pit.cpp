#include "Pit.h"
using namespace std;

Pit::Pit(int id, string material, Pos* topLeft, Pos* bottomRight)
    : AABB(id, "pit", material, topLeft, bottomRight, false, true) {}

void Pit::generateFluidSquareAtBase(string fluid,
                   int offsetPosX,
                   int offsetNegX,
                   int offsetPosZ,
                   int offsetNegZ) {

    int startX = this->getTopLeft().getX() + offsetPosX;
    int startZ = this->getTopLeft().getZ() + offsetPosZ;

    int endX = this->getBottomRight().getX() - offsetNegX;
    int endZ = this->getBottomRight().getZ() - offsetNegZ;

    int base = (this->getTopLeft()).getY();

    for (int x = startX; x <= endX; x++) {
        for (int z = startZ; z <= endZ; z++) {
            Pos pos(x, base, z);
            Block fluidBlock("fluid", fluid, &pos);
            this->addBlock(&fluidBlock);
        }
    }
}

Pit::~Pit() {}