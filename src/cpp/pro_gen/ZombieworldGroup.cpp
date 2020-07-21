#include "ZombieworldGroup.h"
using namespace std;

ZombieworldGroup::ZombieworldGroup(int id,
                                   Pos& firstTopLeft,
                                   Pos& firstBottomRight)
    : Group(id) {
    this->decorate(firstTopLeft, firstBottomRight);
}

void ZombieworldGroup::decorate(Pos& firstTopLeft, Pos& firstBottomRight) {
    this->createAABB(firstTopLeft, firstBottomRight);
    this->generateAllDoorsInAABB();
    this->addLights();
}

void ZombieworldGroup::addLights() {
    for (auto& aabbInGroup : this->getAABBList()) {
        (*aabbInGroup).generateBox("glowstone", 2, 2, 5, 0, 2, 2);
    }
}

void ZombieworldGroup::createAABB(Pos& firstTopLeft, Pos& firstBottomRight) {
    this->addAABB(*(new AABB(
        1, "room", "planks", firstTopLeft, firstBottomRight, true, true)));

    int id = this->getID();
    if (!(id == 1 || id == 7 || id == 9)) {

        Pos secondTopLeft(firstTopLeft);
        secondTopLeft.shiftX(3);
        secondTopLeft.shiftZ(9);

        Pos secondBottomRight(firstBottomRight);
        secondBottomRight.shiftX(3);
        secondBottomRight.shiftZ(9);

        this->addAABB(*(new AABB(2,
                                 "room",
                                 "planks",
                                 secondTopLeft,
                                 secondBottomRight,
                                 true,
                                 true)));
    }
}

void ZombieworldGroup::addLevers() {
    AABB* aabbTwo = this->getAABB(2);

    if (aabbTwo != nullptr) {

        Pos topEdgeMidpoint = (*aabbTwo).getEdgeMidpointAtBase().at(0);
        topEdgeMidpoint.shiftY(2);
        topEdgeMidpoint.shiftX(-1);
        topEdgeMidpoint.shiftZ(-1);

        this->addBlock(*(new Block("lever", topEdgeMidpoint)));
    }
}