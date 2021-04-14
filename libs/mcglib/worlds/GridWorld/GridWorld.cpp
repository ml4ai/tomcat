#include "GridWorld.h"
using namespace std;

void GridWorld::addRandomVictim(AABB& aabb, Pos& pos, double greenBias) {

    mt19937_64& gen = this->getRandom();

    uniform_int_distribution<> dist(1, 100);
    double greenProbability = greenBias * 100;
    int randomInt = dist(gen);
    if (randomInt <= greenProbability) {
        auto block = make_unique<Block>("prismarine", pos);
        auto curObj = make_unique<Object>("vg1", "green_victim", move(block));
        aabb.addObject(move(curObj));
    }
    else {
        auto block = make_unique<Block>("gold", pos);
        auto curObj = make_unique<Object>("vg1", "yellow_victim", move(block));
        aabb.addObject(move(curObj));
    }
}

void GridWorld::generateAABBGrid() {

    mt19937_64& gen = this->getRandom();

    // Add the first one
    int idCtr = 1;
    string material = "planks";
    Pos topLeft(1, 3, 1);
    Pos bottomRight(AABB_size, 3 + AABB_size, AABB_size);

    auto firstAABB = make_unique<AABB>(
        to_string(idCtr), "room", material, topLeft, bottomRight, true, false);
    this->addAABB(move(firstAABB));
    auto prevAABB = this->getAABBList().back().get();

    // Use relative coordinates for the "previous" AABB to generate the rest at
    // each step
    while (idCtr <= (N * N - 1)) {
        idCtr++;

        if ((idCtr - 1) % N == 0) {
            // Condition for when a row in the grid is complete and we move onto
            // the next one
            Pos newTopLeft(1, 3, 1);
            newTopLeft.setZ((*prevAABB).getBottomRight().getZ() + sep);

            Pos newBottomRight(
                AABB_size, 3 + AABB_size, newTopLeft.getZ() + AABB_size);

            auto curAABB = make_unique<AABB>(to_string(idCtr),
                                             "room",
                                             material,
                                             newTopLeft,
                                             newBottomRight,
                                             true,
                                             false);
            this->addAABB(move(curAABB));
        }
        else {
            // Condition for the next AABB in the current row
            Pos newTopLeft = (*prevAABB).getTopLeft();
            newTopLeft.setX((*prevAABB).getBottomRight().getX() + sep);

            Pos newBottomRight = (*prevAABB).getBottomRight();
            newBottomRight.setX(newTopLeft.getX() + AABB_size);

            auto curAABB = make_unique<AABB>(to_string(idCtr),
                                             "room",
                                             material,
                                             newTopLeft,
                                             newBottomRight,
                                             true,
                                             false);
            this->addAABB(move(curAABB));
        }
        prevAABB = this->getAABBList().back().get();
    }
}

void GridWorld::generateVictimInAABB(AABB& aabb) {
    mt19937_64& gen = this->getRandom();
    int sizeY = aabb.getSizeY() - 1;
    Pos randPos(aabb.getRandomPos(gen, 2, 2, 1, sizeY, 2, 2));

    uniform_int_distribution<> dist(1, 100);
    int randInteger = dist(gen);

    if (randInteger <= 75) {
        addRandomVictim(aabb, randPos, 0.60);
    }
}

void GridWorld::generateBlocks() {
    for (auto& aabb : this->getAABBList()) {
        (*aabb).generateAllDoorsInAABB();
        generateVictimInAABB(*aabb);
    }
}

GridWorld::GridWorld(int N, int separation, int AABB_size, int seed) {
    this->setRandom(seed);
    this->N = N;
    this->sep = separation;
    this->AABB_size = AABB_size;

    this->generateAABBGrid();
    this->generateBlocks();
}

GridWorld::~GridWorld() {}
