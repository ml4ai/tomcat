#include "ZombieworldGenerator.h"
using namespace std;

void ZombieWorldGenerator::addLevers() {
    for (auto& aabb : this->getWorld().getAABBList()) {
        Group* g = dynamic_cast<Group*>(aabb);

        if (g) {
            AABB* aabbTwo = (*g).getAABB(2);

            if (aabbTwo != nullptr) {

                Pos topEdgeMidpoint = (*aabbTwo).getEdgeMidpointAtBase().at(0);
                topEdgeMidpoint.shiftY(2);
                topEdgeMidpoint.shiftX(-1);
                topEdgeMidpoint.shiftZ(-1);

                (*g).addBlock(*(new Block("lever", topEdgeMidpoint)));
            }
        }
    }
}

void ZombieWorldGenerator::addGroupOfAABB(int idCtr,
                                          Pos& firstTopLeft,
                                          Pos& firstBottomRight) {

    World& world = this->getWorld();
    world.addAABB(*(new Group(idCtr)));

    Group* g = dynamic_cast<Group*>((world.getAABBList()).back());

    (*g).addAABB(*(new AABB(
        1, "room", "planks", firstTopLeft, firstBottomRight, true, true)));

    if (!(idCtr == 1 || idCtr == 7 || idCtr == 9)) {

        Pos secondTopLeft(firstTopLeft);
        secondTopLeft.shiftX(3);
        secondTopLeft.shiftZ(9);

        Pos secondBottomRight(firstBottomRight);
        secondBottomRight.shiftX(3);
        secondBottomRight.shiftZ(9);

        (*g).addAABB(*(new AABB(2,
                                "room",
                                "planks",
                                secondTopLeft,
                                secondBottomRight,
                                true,
                                true)));
    }
}

void ZombieWorldGenerator::chooseZombieworldAABB(int idCtr,
                                                 Pos& topLeft,
                                                 Pos& bottomRight) {

    World& world = this->getWorld();
    mt19937_64& gen = this->getRandom();

    if (idCtr % 2 == 0) {
        Pos newBottomRight(bottomRight);
        newBottomRight.setY(topLeft.getY()); // We are adding a Pit so we want
                                             // the AABB to be much flatter

        uniform_int_distribution<> dist(1, 100);
        int rand = dist(gen);

        // Choose between an air, water or lava pit
        if (rand <= 25) {
            Pos newTopLeft(topLeft);

            newTopLeft.shiftY(1);
            newBottomRight.shiftY(
                1); // Move both above ground level for an air pit which achives
                    // the effect of an empty plot

            world.addAABB(*(new Pit(idCtr, "air", newTopLeft, newBottomRight)));
        }

        else if (rand > 25 && rand <= 75) {
            Pos newTopLeft(topLeft);
            newTopLeft.shiftY(-2); // In this case I'm chosing to offset the
                                   // base Y by -2 so we have a deeper water pit

            world.addAABB(
                *(new Pit(idCtr, "sand", newTopLeft, newBottomRight)));
            AABB* waterPit = (world.getAABBList()).back();

            (*waterPit).generateBox("water",
                                    3,
                                    3,
                                    1,
                                    0,
                                    3,
                                    3); // Add a box of water to it

            // Randomly add other blocks to give the effect of randomization
            (*waterPit).addRandomBlocks(20, "grass", gen, 0, 0, 1, 0, 0, 0);
            (*waterPit).addRandomBlocks(40, "water", gen, 1, 1, 2, 0, 0, 0);
        }
        else {
            world.addAABB(*(new Pit(idCtr, "grass", topLeft, newBottomRight)));
            AABB* lavaPit = world.getAABBList().back();
            (*lavaPit).generateBox("lava", 3, 2, 0, 0, 1, 3);
            (*lavaPit).addRandomBlocks(10, "grass", gen, 1, 1, 0, 0, 1, 1);
            (*lavaPit).addRandomBlocks(
                30,
                "cobblestone",
                gen); // Example showing use of all default values
            (*lavaPit).addRandomBlocks(10, "lava", gen, 1, 0, 0, 0, 1, 1);
        }
    }

    else {

        this->addGroupOfAABB(idCtr, topLeft, bottomRight);
    }
}

void ZombieWorldGenerator::generateAABBGrid() {
    World& world = this->getWorld();
    mt19937_64& gen = this->getRandom();

    // Add the first one
    int idCtr = 1;
    Pos prevTopLeft(1, 3, 1);
    Pos prevBottomRight(AABB_size, 3 + AABB_size / 2, AABB_size);

    this->addGroupOfAABB(idCtr, prevTopLeft, prevBottomRight);

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

            chooseZombieworldAABB(
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

            chooseZombieworldAABB(
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

void ZombieWorldGenerator::decorate() {
    World& world = this->getWorld();

    // Add doors and lights
    for (auto& aabb : world.getAABBList()) {
        if (strcmp((*aabb).getType().c_str(), "pit") != 0) {
            (*aabb).generateAllDoorsInAABB();

            Group* g = dynamic_cast<Group*>(aabb);
            if (g) {
                for (auto& aabbInGroup : (*g).getAABBList())
                    (*aabbInGroup).generateBox("glowstone", 2, 2, 5, 0, 2, 2);
            }
        }
    }

    // Add levers
    this->addLevers();
}

void ZombieWorldGenerator::generateBoundingWalls() {
    World& world = this->getWorld();
    AABB* firstAABB = world.getAABBList().front();
    AABB* lastAABB = world.getAABBList().back();

    // Create boundary
    Pos boundaryTopLeft((*firstAABB).getTopLeft());
    boundaryTopLeft.shiftX(-4);
    boundaryTopLeft.setY(-3);
    boundaryTopLeft.shiftZ(-4);

    Pos boundaryBottomRight((*lastAABB).getBottomRight());
    boundaryBottomRight.shiftX(4);
    boundaryBottomRight.setY(13);
    boundaryBottomRight.shiftZ(4);

    world.addAABB(*(new AABB(
        0, "boundary", "cobblestone", boundaryTopLeft, boundaryBottomRight)));

    // Create Internal Separator 1
    Pos separator1BottomRight(boundaryBottomRight);
    separator1BottomRight.shiftX(-20);

    Pos separator1TopLeft(separator1BottomRight);
    separator1TopLeft.shiftZ(-50);
    separator1TopLeft.setY(boundaryTopLeft.getY());

    world.addAABB(*(new AABB(-1,
                             "wall",
                             "cobblestone",
                             separator1TopLeft,
                             separator1BottomRight,
                             false)));

    AABB* separatorWall1 = world.getAABBList().back();
    (*separatorWall1).generateBox("fence", 0, 0, 3, 2, 1, 1);

    // Create Internal Separator 2
    Pos separator2BottomRight(separator1TopLeft);
    separator2BottomRight.setY(separator1BottomRight.getY());

    Pos separator2TopLeft(separator2BottomRight);
    separator2TopLeft.shiftX(-25);
    separator2TopLeft.setY(boundaryTopLeft.getY());

    world.addAABB(*(new AABB(-2,
                             "wall",
                             "cobblestone",
                             separator2TopLeft,
                             separator2BottomRight,
                             false)));
    AABB* separatorWall2 = world.getAABBList().back();
    (*separatorWall2).generateBox("fence", 1, 1, 3, 2, 0, 0);
}

ZombieWorldGenerator::ZombieWorldGenerator(int seed) {
    this->setRandom(seed);
    this->generateAABBGrid();
    this->decorate();
    this->generateBoundingWalls();
}

ZombieWorldGenerator::~ZombieWorldGenerator() {}
