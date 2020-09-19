#include "ZombieworldGroup.h"

using namespace std;

ZombieworldGroup::ZombieworldGroup(string id,
                                   Pos& firstTopLeft,
                                   Pos& firstBottomRight)
    : Group(id) {
    this->decorate(firstTopLeft, firstBottomRight);
}

void ZombieworldGroup::decorate(Pos& firstTopLeft, Pos& firstBottomRight) {
    this->createAABB(firstTopLeft, firstBottomRight);
    this->generateAllDoorsInAABB();
    this->addLights();
    this->addLevers();
    this->addEntities();
}

void ZombieworldGroup::addEntities() {
    AABB* aabbTwo = this->getAABB("2");

    if (aabbTwo != nullptr) {

        int sizeY = (*aabbTwo).getSizeY() - 1;
        Pos randomPos = (*aabbTwo).getRandomPos(this->gen, 1, 1, sizeY, 1, 1);
        (*aabbTwo).addEntity(*(new Entity("villager", randomPos)));
    }

    AABB* aabbOne = this->getAABB("1"); // 1 sub AABB definitely exists
    int sizeY = (*aabbOne).getSizeY() - 1;
    Pos randomPos = (*aabbOne).getRandomPos(this->gen, 1, 1, sizeY, 1, 1);
    this->addEntity(*(new Entity("zombie", randomPos)));
}

void ZombieworldGroup::addLights() {
    for (auto& aabbInGroup : this->getAABBList()) {
        (*aabbInGroup).generateBox("glowstone", 2, 2, 5, 0, 2, 2);
    }
}

void ZombieworldGroup::createAABB(Pos& firstTopLeft, Pos& firstBottomRight) {
    this->addAABB(*(new AABB(
        "1", "room", "planks", firstTopLeft, firstBottomRight, true, true)));

    string id = this->getID();
    if (!(strcmp(id.c_str(), "1") == 0 || strcmp(id.c_str(), "7") == 0 ||
          strcmp(id.c_str(), "9") == 0)) {

        Pos secondTopLeft(firstTopLeft);
        secondTopLeft.shiftX(3);
        secondTopLeft.shiftZ(9);

        Pos secondBottomRight(firstBottomRight);
        secondBottomRight.shiftX(3);
        secondBottomRight.shiftZ(9);

        this->addAABB(*(new AABB("2",
                                 "room",
                                 "planks",
                                 secondTopLeft,
                                 secondBottomRight,
                                 true,
                                 true)));
    }
}

void ZombieworldGroup::addLevers() {
    AABB* aabbTwo = this->getAABB("2");

    if (aabbTwo != nullptr) {

        Pos topEdgeMidpoint = (*aabbTwo).getEdgeMidpointAtBase().at(0);
        topEdgeMidpoint.shiftY(2);
        topEdgeMidpoint.shiftX(-1);
        topEdgeMidpoint.shiftZ(-1);

        this->addBlock(*(new Lever(topEdgeMidpoint, false, "north")));

        // Adds the connection representing this doorway
        // This is an example and may have innacurate coordinates
        this->addConnection(*(new Connection(
            "c1", "entrance to second room", "door", "rectangle")));
        vector<string> connectedLocations{"1", "2"};
        this->getConnectionList().at(0)->addManyConnectedLocations(
            connectedLocations);

        Pos adjacent(topEdgeMidpoint);
        adjacent.shiftX(1);
        vector<Pos> coordinates{topEdgeMidpoint, adjacent};
        this->getConnectionList().at(0)->addManyCoordinates(coordinates);
    }
}

ZombieworldGroup::~ZombieworldGroup() {}
