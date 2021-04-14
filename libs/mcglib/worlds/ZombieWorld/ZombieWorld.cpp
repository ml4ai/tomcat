#include "ZombieWorld.h"
using namespace std;

void ZombieWorld::chooseZombieworldAABB(int idCtr,
                                        Pos& topLeft,
                                        Pos& bottomRight,
                                        unique_ptr<AABB>& enclosing_boundary) {

    mt19937_64& gen = this->getRandom();

    if (idCtr % 2 == 0) {
        Pos newTopLeft(topLeft);
        newTopLeft.shiftY(-2);
        uniform_int_distribution<> dist(1, 100);
        int rand = dist(gen);

        // Choose between a blank box, water or lava pit
        if (rand <= 25) {

            // A blank material type means nothing is placed
            // Note that anytime I add this object, it'll have an id =
            // "blank_box" this won't be unique accross and is generally bad
            // practice. However, I know that I will never need to uniquely
            // reference this again so I didn't bother making it's id unique

            auto zWorldPit =
                make_unique<ZombieworldPit>("blank_box", topLeft, "blank");

            enclosing_boundary->addAABB(move(zWorldPit));
        }
        else if (rand > 25 && rand <= 75) {
            auto zWorldPit = make_unique<ZombieworldPit>(
                "pool_of_water_" + to_string(this->water_pool_count),
                newTopLeft,
                "water");
            enclosing_boundary->addAABB(move(zWorldPit));
            this->water_pool_count++;
        }
        else {
            auto zWorldPit = make_unique<ZombieworldPit>(
                "pool_of_lava_" + to_string(this->lava_pool_count),
                newTopLeft,
                "lava");
            enclosing_boundary->addAABB(move(zWorldPit));
            this->lava_pool_count++;
        }
    }
    else {
        auto zWorldGroup = make_unique<ZombieworldGroup>(
            "building_" + to_string(this->building_count),
            topLeft,
            bottomRight);
        enclosing_boundary->addAABB(move(zWorldGroup));
        this->building_count++;
    }
}

void ZombieWorld::generateAABBGrid() {
    mt19937_64& gen = this->getRandom();
    auto& enclosing_boundary = this->getAABBList().front();

    // Add the first one
    int idCtr = 1;
    Pos prevTopLeft(1, 3, 1);
    Pos prevBottomRight(AABB_size, 3 + AABB_size / 2, AABB_size);

    auto firstAABB = make_unique<ZombieworldGroup>(
        "building_" + to_string(this->building_count),
        prevTopLeft,
        prevBottomRight);
    enclosing_boundary->addAABB(move(firstAABB));
    this->building_count++;

    // Use relative coordinates for the "previous" AABB to generate the rest
    // at each step
    int numberOfBuildings = N * N - 1;
    while (idCtr <= numberOfBuildings) {
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
                bottomRight,
                enclosing_boundary); // Choose the AABB to add. DOESN'T change
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
                bottomRight,
                enclosing_boundary); // Choose the AABB to add. DOESN'T change
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
    // Create boundary
    Pos boundaryTopLeft(1, 3, 1);
    boundaryTopLeft.shiftX(-4);
    boundaryTopLeft.setY(-3);
    boundaryTopLeft.shiftZ(-4);

    Pos boundaryBottomRight(61, 8, 67);
    boundaryBottomRight.shiftX(4);
    boundaryBottomRight.setY(13);
    boundaryBottomRight.shiftZ(4);

    auto boundary = make_unique<AABB>("enclosing_boundary",
                                      "boundary",
                                      "cobblestone",
                                      boundaryTopLeft,
                                      boundaryBottomRight,
                                      true,
                                      false,
                                      false);
    this->addAABB(move(boundary));

    auto& enclosing_boundary = this->getAABBList().front();

    // Create Internal Separator 1
    Pos separator1BottomRight(boundaryBottomRight);
    separator1BottomRight.shiftX(-20);

    Pos separator1TopLeft(separator1BottomRight);
    separator1TopLeft.shiftZ(-50);
    separator1TopLeft.setY(boundaryTopLeft.getY());

    auto separator1 = make_unique<AABB>("internal_separator_1",
                                        "wall",
                                        "cobblestone",
                                        separator1TopLeft,
                                        separator1BottomRight,
                                        false);

    enclosing_boundary->addAABB(move(separator1));

    auto& separatorWall1 = *(enclosing_boundary->getAABBList().back().get());
    separatorWall1.generateBox("fence", 0, 0, 3, 2, 1, 1);

    // Create Internal Separator 2
    Pos separator2BottomRight(separator1TopLeft);
    separator2BottomRight.setY(separator1BottomRight.getY());

    Pos separator2TopLeft(separator2BottomRight);
    separator2TopLeft.shiftX(-25);
    separator2TopLeft.setY(boundaryTopLeft.getY());

    auto separator2 = make_unique<AABB>("internal_separator_2",
                                        "wall",
                                        "cobblestone",
                                        separator2TopLeft,
                                        separator2BottomRight,
                                        false);
    enclosing_boundary->addAABB(move(separator2));

    auto& separatorWall2 = *(enclosing_boundary->getAABBList().back().get());
    separatorWall2.generateBox("fence", 1, 1, 3, 2, 0, 0);
}

ZombieWorld::ZombieWorld(int seed) {
    this->setRandom(seed);
    this->generateBoundingWalls();
    this->generateAABBGrid();
}

ZombieWorld::~ZombieWorld() {}
