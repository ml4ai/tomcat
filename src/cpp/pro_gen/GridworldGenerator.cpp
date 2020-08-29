#include "GridworldGenerator.h"
using namespace std;

void GridworldGenerator::addRandomVictim(AABB& aabb,
                                         Pos& pos,
                                         double greenBias) {

    mt19937_64& gen = this->getRandom();

    uniform_int_distribution<> dist(1, 100);
    double greenProbability = greenBias * 100;
    int randomInt = dist(gen);
    if (randomInt <= greenProbability) {
        aabb.addBlock(*(new Block("prismarine", pos, "victim")));
    }
    else {
        aabb.addBlock(*(new Block("gold", pos, "victim")));
    }
}

void GridworldGenerator::generateAABBGrid() {

    World& world = this->getWorld();
    mt19937_64& gen = this->getRandom();

    // Add the first one
    int idCtr = 1;
    string material = "planks";
    Pos topLeft(1, 3, 1);
    Pos bottomRight(AABB_size, 3 + AABB_size, AABB_size);

    world.addAABB(*(
        new AABB(idCtr, "room", material, topLeft, bottomRight, true, false)));
    AABB prevAABB = *(world.getAABBList().back());

    // Use relative coordinates for the "previous" AABB to generate the rest at
    // each step
    while (idCtr <= (N * N - 1)) {
        idCtr++;

        if ((idCtr - 1) % N == 0) {
            // Condition for when a row in the grid is complete and we move onto
            // the next one
            Pos newTopLeft(1, 3, 1);
            newTopLeft.setZ(prevAABB.getBottomRight().getZ() + sep);

            Pos newBottomRight(
                AABB_size, 3 + AABB_size, newTopLeft.getZ() + AABB_size);
            world.addAABB(*(new AABB(idCtr,
                                     "room",
                                     material,
                                     newTopLeft,
                                     newBottomRight,
                                     true,
                                     false)));
            prevAABB = *(world.getAABBList().back());
            ;
        }
        else {
            // Condition for the next AABB in the current row
            Pos newTopLeft = prevAABB.getTopLeft();
            newTopLeft.setX(prevAABB.getBottomRight().getX() + sep);

            Pos newBottomRight = prevAABB.getBottomRight();
            newBottomRight.setX(newTopLeft.getX() + AABB_size);

            world.addAABB(*(new AABB(idCtr,
                                     "room",
                                     material,
                                     newTopLeft,
                                     newBottomRight,
                                     true,
                                     false)));
            prevAABB = *(world.getAABBList().back());
        }
    }
}

void GridworldGenerator::generateVictimInAABB(AABB& aabb) {
    mt19937_64& gen = this->getRandom();

    Pos randPos(aabb.getRandomPosAtBase(gen, 2, 2, 2, 2));
    randPos.setY(randPos.getY() + 1);

    uniform_int_distribution<> dist(1, 100);
    int randInteger = dist(gen);

    if (randInteger <= 75) {
        addRandomVictim(aabb, randPos, 0.60);
    }
    else {
        ;
    }
}

void GridworldGenerator::generateBlocks() {

    World& world = this->getWorld();

    for (auto& aabb : world.getAABBList()) {
        (*aabb).generateAllDoorsInAABB();
        generateVictimInAABB(*aabb);
    }
}

GridworldGenerator::GridworldGenerator(int N,
                                       int separation,
                                       int AABB_size,
                                       int seed) {
    this->setRandom(seed);
    this->N = N;
    this->sep = separation;
    this->AABB_size = AABB_size;

    this->generateAABBGrid();
    this->generateBlocks();
}

GridworldGenerator::~GridworldGenerator() {}
