#include "DungeonRoom1.h"

using namespace std;

DungeonRoom1::DungeonRoom1(string id, Pos& topLeft) : AABB(id) {
    this->setFields(topLeft);
    this->decorate();
    this->addRewards();
    this->addBrokenWalls();
    this->addEntities();
}

void DungeonRoom1::addEntities() {
    Pos randomPos(this->getRandomPos(this->gen,1,1,1,1,1));
    auto curEntity = make_unique<Entity>("blaze", randomPos);
    this->addEntity(move(curEntity));
}

void DungeonRoom1::addRewards() {
    this->addRandomBlocks(3, "gold_block", this->gen, 1, 1, 0, 5, 1, 1);
}

void DungeonRoom1::addBrokenWalls() {
    this->addRandomBlocks(18, "air", this->gen, 0, 11, 0, 0, 0, 0);
    this->addRandomBlocks(17, "air", this->gen, 0, 0, 0, 0, 0, 11);
    this->addRandomBlocks(15, "air", this->gen, 0, 0, 0, 0, 11, 0);
    this->addRandomBlocks(15, "air", this->gen, 11, 0, 0, 0, 0, 0);
}

void DungeonRoom1::decorate() {
    this->addRandomBlocks(5, "lava", this->gen, 2, 2, 0, 5, 2, 2);
        this->generateBox(
        "barrier",
        1,
        1,
        5,
        0,
        1,
        1); // Puts an invisible blockade on the roof so mobs don't fly out
}

void DungeonRoom1::setFields(Pos& topLeft) {
    // We want a new room each time
    this->gen.seed(rand());

    // Defines the room as size 11x11x5
    this->topLeft = topLeft;
    auto bottomRight(topLeft);
    bottomRight.shiftX(11);
    bottomRight.shiftZ(11);
    bottomRight.shiftY(5);
    this->bottomRight = bottomRight;

    // Other info
    this->type = "dungeon_room_1";
    this->isHollow = true;
    this->hasRoof = false;
    this->material = "nether_brick";
}

DungeonRoom1::~DungeonRoom1() {}