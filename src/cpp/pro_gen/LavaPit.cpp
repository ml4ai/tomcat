#include "LavaPit.h"
using namespace std;

LavaPit::LavaPit(int id,
                 string material,
                 Pos* topLeft,
                 Pos* bottomRight,
                 bool isHollow,
                 bool hasRoof)
    : AABB(id, material, topLeft, bottomRight, isHollow, hasRoof) {}

void LavaPit::generateLava() {

    int base = (this->getTopLeft()).getY();
    for (int x = (this->getTopLeft()).getX();
         x <= (this->getBottomRight()).getX();
         x++) {
        for (int z = (this->getTopLeft()).getZ();
             z <= (this->getBottomRight()).getZ();
             z++) {
            Pos pos(x, base, z);
            Block lava("lava", "lava", &pos);
            this->addBlock(&lava);
        }
    }
}

LavaPit::~LavaPit(){}