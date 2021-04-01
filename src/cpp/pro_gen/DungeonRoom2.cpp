#include "DungeonRoom2.h"

using namespace std;

DungeonRoom2::DungeonRoom2(string id, Pos& topLeft) : AABB(id) {
    this->setFields(topLeft);
    this->decorate();
    this->addRewards();
    this->addBrokenWalls();
    this->addEntities();
}

void DungeonRoom2::addEntities() {
    for (int i = 0; i <= 2; i++) {
        Pos randomPos(this->getRandomPos(this->gen,1,1,1,1,1));
        auto curEntity = make_unique<Entity>("wither_skeleton", randomPos);
        this->addEntity(move(curEntity));
    }
}

void DungeonRoom2::addRewards() {
    this->addRandomBlocks(2, "diamond_block", this->gen, 1, 1, 0, 5, 1, 1);
}

void DungeonRoom2::addBrokenWalls() {
    this->addRandomBlocks(15, "air", this->gen, 0, 11, 0, 0, 0, 0);
    this->addRandomBlocks(16, "air", this->gen, 0, 0, 0, 0, 0, 11);
    this->addRandomBlocks(15, "air", this->gen, 0, 0, 0, 0, 11, 0);
    this->addRandomBlocks(15, "air", this->gen, 11, 0, 0, 0, 0, 0);
}

void DungeonRoom2::decorate() {
    this->addRandomBlocks(15, "gravel", this->gen, 1, 1, 0, 5, 1, 1);
    this->addRandomBlocks(15, "cobblestone", this->gen, 1, 1, 0, 5, 1, 1);
    this->addRandomBlocks(9, "web", this->gen, 0, 0, 0, 1, 0, 11);
    this->addRandomBlocks(5, "leaves", this->gen, 0, 0, 0, 1, 11, 0);
    this->addRandomBlocks(5, "web", this->gen, 11, 0, 0, 1, 0, 0);
}

void DungeonRoom2::setFields(Pos& topLeft) {
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
    this->type = "dungeon_room_2";
    this->isHollow = true;
    this->hasRoof = false;
    this->material = "stonebrick";
}

DungeonRoom2::~DungeonRoom2() {}