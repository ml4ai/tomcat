#include "ZombieWorld.h"
using namespace std;

void ZombieWorld::chooseZombieworldAABB(int idCtr,
                                        Pos& topLeft,
                                        Pos& bottomRight) {

    mt19937_64& gen = this->getRandom();

    if (idCtr % 2 == 0) {
        Pos newTopLeft(topLeft);
        newTopLeft.shiftY(-2);
        uniform_int_distribution<> dist(1, 100);
        int rand = dist(gen);

        // Choose between an air, water or lava pit
        if (rand <= 25) {
            this->addAABB(ZombieworldPit(idCtr, topLeft, "air"));
        }

        else if (rand > 25 && rand <= 75) {
            this->addAABB(ZombieworldPit(idCtr, newTopLeft, "water"));
        }
        else {
            this->addAABB(ZombieworldPit(idCtr, newTopLeft, "lava"));
        }
    }
    else {
        this->addAABB(ZombieworldGroup(idCtr, topLeft, bottomRight));
    }
}

void ZombieWorld::generateAABBGrid() {
    mt19937_64& gen = this->getRandom();

    // Add the first one
    int idCtr = 1;
    Pos prevTopLeft(1, 3, 1);
    Pos prevBottomRight(AABB_size, 3 + AABB_size / 2, AABB_size);

    this->addAABB(ZombieworldGroup(idCtr, prevTopLeft, prevBottomRight));

    // Use relative coordinates for the "previous" AABB to generate the rest
    // at each step
    while (idCtr <= (N * N - 1)) {
        idCtr++;

        if ((idCtr - 1) % N == 0) {
            // Condition for when a row in the grid is complete and we move
            // onto the next one

            Pos topLeft(1, 3, 1);
            topLeft.setZ(prevBottomRight.getZ() + sep);
            Pos bottomRight(
                AABB_size, 3 + AABB_size / 2, topLeft.getZ() + AABB_size - 1);

            this->chooseZombieworldAABB(
                idCtr,
                topLeft,
                bottomRight); // Choose the AABB to add. DOESN'T change
                              // topLeft and bottomRight

            prevTopLeft = topLeft;
            prevBottomRight = bottomRight; // Set as if prev AABB was a room for
                                           // consistency. Direct result of the
                                           // fact that choosing an AABB doesn't
                                           // change the coordinates
        }
        else {
            // Condition for the next AABB in the current row

            Pos topLeft(prevTopLeft);
            topLeft.setX(prevBottomRight.getX() + sep);

            Pos bottomRight(prevBottomRight);
            bottomRight.setX(topLeft.getX() + AABB_size - 1);

            this->chooseZombieworldAABB(
                idCtr,
                topLeft,
                bottomRight); // Choose the AABB to add. DOESN'T change
                              // topLeft and bottomRIght

            prevTopLeft = topLeft;
            prevBottomRight = bottomRight; // Set as if prev AABB was a room for
                                           // consistency. Direct result of the
                                           // fact that choosing an AABB doesn't
                                           // change the coordinates
        }
    }
}

void ZombieWorld::generateBoundingWalls() {
    AABB firstAABB = this->getAABBList().front();
    AABB lastAABB = this->getAABBList().back();

    // Create boundary
    Pos boundaryTopLeft(firstAABB.getTopLeft());
    boundaryTopLeft.shiftX(-4);
    boundaryTopLeft.setY(-3);
    boundaryTopLeft.shiftZ(-4);

    Pos boundaryBottomRight(lastAABB.getBottomRight());
    boundaryBottomRight.shiftX(4);
    boundaryBottomRight.setY(13);
    boundaryBottomRight.shiftZ(4);

    this->addAABB(AABB(
        0, "boundary", "cobblestone", boundaryTopLeft, boundaryBottomRight));

    // Create Internal Separator 1
    Pos separator1BottomRight(boundaryBottomRight);
    separator1BottomRight.shiftX(-20);

    Pos separator1TopLeft(separator1BottomRight);
    separator1TopLeft.shiftZ(-50);
    separator1TopLeft.setY(boundaryTopLeft.getY());

    this->addAABB(AABB(-1,
                       "wall",
                       "cobblestone",
                       separator1TopLeft,
                       separator1BottomRight,
                       false));

    AABB separatorWall1 = this->getAABBList().back();
    separatorWall1.generateBox("fence", 0, 0, 3, 2, 1, 1);

    // Create Internal Separator 2
    Pos separator2BottomRight(separator1TopLeft);
    separator2BottomRight.setY(separator1BottomRight.getY());

    Pos separator2TopLeft(separator2BottomRight);
    separator2TopLeft.shiftX(-25);
    separator2TopLeft.setY(boundaryTopLeft.getY());

    this->addAABB(AABB(-2,
                       "wall",
                       "cobblestone",
                       separator2TopLeft,
                       separator2BottomRight,
                       false));
    AABB separatorWall2 = this->getAABBList().back();
    separatorWall2.generateBox("fence", 1, 1, 3, 2, 0, 0);
}

ZombieWorld::ZombieWorld(int seed) {
    this->setRandom(seed);
    this->generateAABBGrid();
    this->generateBoundingWalls();
}

ZombieWorld::~ZombieWorld() {}
