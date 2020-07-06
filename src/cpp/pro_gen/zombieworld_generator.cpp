/**
 * @file gridworld_generator.cpp
 * @brief Defines an algorithm and script to generate the JSON representing an
 * N x N gridworld of cubic AABB with options to specify the separation between
 * AABB in the cardinal direction and the size of the AABB. The generated file
 * is saved to Minecraft/run/procedural.json
 *
 * The size N must be provided while running this script.
 * Separation defaults to 0.
 * AABB_size defaults to 10
 */

#include "Pit.h"
#include "World.h"
#include <boost/program_options.hpp>
#include <boost/random/mersenne_twister.hpp>
#include <boost/random/uniform_int_distribution.hpp>
#include <fstream>
#include <iostream>

using namespace std;
boost::random::mt19937 gen; // Global randomness object
namespace po = boost::program_options;

/**
 * @brief Randomly choose and return an AABB. Sincce this is specific to the
 * zombie world, the function will return a standard room when idCtr is not
 * divisible by 2, but if idCtr is divisible by 2, it will return a subclass of
 * AABB which is a Pit.
 *
 * @param idCtr The id counter which notes the id to assign the new AABB
 * @param topLeft The top left value for the new AABB. A modified value may be
 * used in case of Pit.
 * @param bottomRight The bottom right value for the new AABB. A modified value
 * may be used in case of Pit.
 * @return AABB The AABB object
 */
AABB chooseZombieworldAABB(int idCtr, Pos* topLeft, Pos* bottomRight) {
    if (idCtr % 2 == 0) {
        Pos newBottomRight(*bottomRight);
        newBottomRight.setY(
            (*topLeft).getY()); // We are adding a Pit so we want the AABB to be
                                // much flatter

        boost::random::uniform_int_distribution<> dist(1, 100);
        int rand = dist(gen);

        // Choose between an air, water or lava pit
        if (rand <= 25) {
            Pos newTopLeft(*topLeft);

            newTopLeft.shiftY(1);
            newBottomRight.shiftY(
                1); // Move both above ground level for an air pit which achives
                    // the effect of an empty plot

            Pit airPit(idCtr, "air", &newTopLeft, &newBottomRight);
            return airPit;
        }
        else if (rand > 25 && rand <= 75) {
            Pos newTopLeft(*topLeft);
            newTopLeft.shiftY(-2); // In this case I'm chosing to offset the
                                   // base Y by -2 so we have a deeper water pit

            Pit waterPit(
                idCtr,
                "grass",
                &newTopLeft,
                &newBottomRight); // Start by creating a pit of grass which is
                                  // simply an AABB filled with grass

            waterPit.generateBox("water",
                                 3,
                                 3,
                                 1,
                                 0,
                                 3,
                                 3); // Add a box of water to it

            // Randomly add other blocks to give the effect of randomization
            waterPit.addRandomBlocks(30, "sand", &gen, 0, 1, 2, 0, 1, 0);
            waterPit.addRandomBlocks(20, "water", &gen, 0, 0, 2, 0, 0, 0);
            return waterPit;
        }
        else {
            Pit lavaPit(idCtr, "grass", topLeft, &newBottomRight);
            lavaPit.generateBox("lava", 3, 2, 0, 0, 1, 3);
            lavaPit.addRandomBlocks(10, "grass", &gen, 1, 1, 0, 0, 1, 1);
            lavaPit.addRandomBlocks(
                30,
                "cobblestone",
                &gen); // Example showing use of all default values
            lavaPit.addRandomBlocks(10, "lava", &gen, 1, 0, 0, 0, 1, 1);
            return lavaPit;
        };
    }

    else {
        AABB aabb(idCtr, "room", "planks", topLeft, bottomRight);
        return aabb;
    }
}

/**
 * @brief Generate the basic AABB grid for the zombie world
 *
 * @param worldptr The world where the AABB is to be added
 * @param N The number of AABB on each axis
 * @param sep The separation between AABB in the cardinal directions
 * @param AABB_size The size of each cubic AABB
 * @param material The material of each AABB
 */
void generateAABBGrid(
    World* worldptr, int N, int sep, int AABB_size, string material) {

    // Add the first one
    int idCtr = 1;
    Pos prevTopLeft(1, 3, 1);
    Pos prevBottomRight(AABB_size, 3 + AABB_size, AABB_size);

    AABB prevAABB(idCtr, "room", material, &prevTopLeft, &prevBottomRight);
    (*worldptr).addAABB(&prevAABB);

    // Use relative coordinates for the "previous" AABB to generate the rest at
    // each step
    while (idCtr <= (N * N - 1)) {
        idCtr++;

        if ((idCtr - 1) % N == 0) {
            // Condition for when a row in the grid is complete and we move onto
            // the next one

            Pos topLeft(1, 3, 1);
            topLeft.setZ(prevBottomRight.getZ() + sep);
            Pos bottomRight(
                AABB_size, 3 + AABB_size, topLeft.getZ() + AABB_size);

            AABB curAABB = chooseZombieworldAABB(
                idCtr,
                &topLeft,
                &bottomRight); // Choose the AABB to add. DOESN'T change topLeft
                               // and bottomRIght
            (*worldptr).addAABB(&curAABB);

            prevTopLeft = topLeft;
            prevBottomRight =
                bottomRight; // Set as if prev AABB was a room for consistency.
                             // Direct result of the fact that choosing an AABB
                             // doesn't change the coordinates
        }
        else {
            // Condition for the next AABB in the current row

            Pos topLeft(prevTopLeft);
            topLeft.setX(prevBottomRight.getX() + sep);

            Pos bottomRight(prevBottomRight);
            bottomRight.setX(topLeft.getX() + AABB_size);

            AABB curAABB = chooseZombieworldAABB(
                idCtr,
                &topLeft,
                &bottomRight); // Choose the AABB to add. DOESN'T change topLeft
                               // and bottomRIght
            (*worldptr).addAABB(&curAABB);

            prevTopLeft = topLeft;
            prevBottomRight =
                bottomRight; // Set as if prev AABB was a room for consistency.
                             // Direct result of the fact that choosing an AABB
                             // doesn't change the coordinates
        }
    }
}

/**
 * @brief Generate all 4 doors for an AABB. Door blocks are added to the AABB
 * object.
 *
 * @param aabb The AABB for which doors are to be generated.
 */
void generateAllDoorsInAABB(AABB* aabb) {
    // Get edge midpoints for the AABB because that is where the doors will be
    // placed
    vector<Pos> edges = (*aabb).getEdgeMidpointAtBase();
    Pos topEdgeMid(edges.at(0));
    Pos rightEdgeMid(edges.at(1));
    Pos bottomEdgeMid(edges.at(2));
    Pos leftEdgeMid(edges.at(3));

    // Since points are at base we want them to be at base + 1
    topEdgeMid.shiftY(1);
    bottomEdgeMid.shiftY(1);
    leftEdgeMid.shiftY(1);
    rightEdgeMid.shiftY(1);

    // Use the coordinates to create door blocks
    Block topDoor("door", &topEdgeMid);
    Block bottomDoor("door", &bottomEdgeMid);
    Block leftDoor("door", &leftEdgeMid);
    Block rightDoor("door", &rightEdgeMid);

    // Add it to the AABB's doors
    (*aabb).addBlock(&topDoor);
    (*aabb).addBlock(&bottomDoor);
    (*aabb).addBlock(&leftDoor);
    (*aabb).addBlock(&rightDoor);
}

/**
 * @brief Generate special blocks related to the world and each AABB
 *
 * @param worldptr The world where blocks must be placed
 */
void generateBlocks(World* worldptr) {
    for (auto& aabb : *(*worldptr).getAABBList()) {
        if (strcmp(aabb.getType().c_str(), "pit") != 0) {
            generateAllDoorsInAABB(&aabb);
        }
    }
}

void generateBoundingWalls(World* world) {
    AABB firstAABB = (*(*world).getAABBList()).front();
    AABB lastAABB = (*(*world).getAABBList()).back();

    // Create boundary
    Pos boundaryTopLeft(firstAABB.getTopLeft());
    boundaryTopLeft.shiftX(-4);
    boundaryTopLeft.shiftY(-3);
    boundaryTopLeft.shiftZ(-4);

    Pos boundaryBottomRight(lastAABB.getBottomRight());
    boundaryBottomRight.shiftX(4);
    boundaryBottomRight.shiftZ(4);

    AABB boundingBox(
        0, "boundary", "cobblestone", &boundaryTopLeft, &boundaryBottomRight);
    (*world).addAABB(&boundingBox);

    // Create Internal Separator 1
    Pos separator1BottomRight(boundaryBottomRight);
    separator1BottomRight.shiftX(-20);

    Pos separator1TopLeft(separator1BottomRight);
    separator1TopLeft.shiftZ(-50);
    separator1TopLeft.setY(boundaryTopLeft.getY());

    AABB separatorWall(-1,
                       "wall",
                       "cobblestone",
                       &separator1TopLeft,
                       &separator1BottomRight,
                       false);
    separatorWall.generateBox("fence", 0, 0, 3, 3, 1, 1);

    (*world).addAABB(&separatorWall);

    // Create Internal Separator 2
    Pos separator2BottomRight(separator1TopLeft);
    separator2BottomRight.setY(separator1BottomRight.getY());

    Pos separator2TopLeft(separator2BottomRight);
    separator2TopLeft.shiftX(-25);
    separator2TopLeft.setY(boundaryTopLeft.getY());

    AABB separatorWall2(-2,
                        "wall",
                        "cobblestone",
                        &separator2TopLeft,
                        &separator2BottomRight,
                        false);
    separatorWall2.generateBox("fence", 1, 1, 3, 3, 0, 0);
    (*world).addAABB(&separatorWall2);
}

/**
 * @brief Generate the zombie mision world
 *
 * @return World The generated world object representing the zombie mission
 */
World generateZombieWorld() {
    int N = 3;
    int sep = 15;
    int AABB_size = 10;
    string AABB_material = "planks";

    World world;
    generateAABBGrid(&world, N, sep, AABB_size, AABB_material);
    generateBlocks(&world);
    generateBoundingWalls(&world);
    return world;
}

/**
 * @brief Directive method to create the world and write the JSON and TSV output
 * to file.
 */
int main() {

    // Process input and generate output
    cout << "Generating gridworld..." << endl;
    World world = generateZombieWorld();
    cout << "Writing to file..." << endl;

    // Write JSON
    ofstream outputJSON(
        "../../../../external/malmo/Minecraft/run/procedural.json", ios::out);
    outputJSON << world.toJSON();
    outputJSON.close();

    // Write TSV
    ofstream outputTSV(
        "../../../../external/malmo/Minecraft/run/procedural.tsv", ios::out);
    outputTSV << world.toTSV();
    outputTSV.close();

    cout << "Done. The generated files are in Minecraft/run/procedural.json "
            "and Minecraft/run/procedural.tsv"
         << endl;

    return 0;
}