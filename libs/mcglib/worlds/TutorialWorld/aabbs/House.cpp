#include "House.h"
using namespace std;

House::House(string id, Pos& topLeft) : AABB(id) {
    // Set the base material to be a log
    this->setMaterial("log");

    // Define the object's boundaries
    Pos bottomRight(topLeft);
    bottomRight.shift(5, 4, 5);
    this->setTopLeft(topLeft);
    this->setBottomRight(bottomRight);
}

void House::init(){

    // The floor should be made of planks
    this->generateBox("planks", 1, 1, 0, 4, 1, 1);

    // Add windows
    this->generateBox("glass", 0, 5, 1, 1, 1, 1);
    this->generateBox("glass", 5, 0, 1, 1, 1, 1);
    this->generateBox("glass", 1, 1, 1, 1, 0, 5);

    // Add a roof
    this->hasRoof = true;

    // Add a friend
    Pos randomPos = this->getRandomPos(this->gen, 1, 1, 1, 2, 1, 1);
    auto villager = make_unique<Entity>("villager", randomPos);
    this->addEntity(move(villager));
}

House::~House() {}