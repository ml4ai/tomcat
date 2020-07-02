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

#include "LavaPit.h"
#include "ProceduralGenerator.h"
#include "World.h"
#include <boost/program_options.hpp>
#include <boost/random/mersenne_twister.hpp>
#include <boost/random/uniform_int_distribution.hpp>
#include <fstream>
#include <iostream>

using namespace std;
boost::random::mt19937 gen;
namespace po = boost::program_options;

/**
 * @brief Generate N^2 AABB and place them in the given world such that there
 * are N AABB on each axis separated from the other AABB by sep in the cardinal
 * direction and each AABB is of the given AABB_size. The AABB are also made of
 * the specified material
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
    Pos tL(1, 3, 1);
    Pos bR(AABB_size, 3 + AABB_size, AABB_size);

    AABB prevAABB(idCtr, material, &tL, &bR, true, false);
    (*worldptr).addAABB(&prevAABB);

    // Use relative coordinates for the "previous" AABB to generate the rest at
    // each step
    while (idCtr <= (N * N - 1)) {
        idCtr++;

        if ((idCtr - 1) % N == 0) {
            // Condition for when a row in the grid is complete and we move onto
            // the next one
            Pos topLeft(1, 3, 1);
            topLeft.setZ(prevAABB.getBottomRight().getZ() + sep);

            Pos bottomRight(
                AABB_size, 3 + AABB_size, topLeft.getZ() + AABB_size);

            AABB curAABB(idCtr, material, &topLeft, &bottomRight, true, false);

            if (idCtr % 2 == 0) {
                Pos newBottomRight(bottomRight);
                newBottomRight.setY(topLeft.getY());

                LavaPit lavaPit(
                    idCtr, "lava", &topLeft, &newBottomRight, false, true);
                lavaPit.generateLava();

                (*worldptr).addAABB(&lavaPit);
            }
            else {

                (*worldptr).addAABB(&curAABB);
            }
            prevAABB = curAABB;
        }
        else {
            // Condition for the next AABB in the current row
            Pos topLeft = prevAABB.getTopLeft();
            topLeft.setX(prevAABB.getBottomRight().getX() + sep);

            Pos bottomRight = prevAABB.getBottomRight();
            bottomRight.setX(topLeft.getX() + AABB_size);

            AABB curAABB(idCtr, material, &topLeft, &bottomRight, true, false);

            if (idCtr % 2 == 0) {
                Pos newBottomRight(bottomRight);
                newBottomRight.setY(topLeft.getY());

                LavaPit lavaPit(
                    idCtr, "lava", &topLeft, &newBottomRight, false, true);
                lavaPit.generateLava();

                (*worldptr).addAABB(&lavaPit);
            }
            else {

                (*worldptr).addAABB(&curAABB);
            }
            prevAABB = curAABB;
        }
    }
}

/**
 * @brief Gets the midpoint of each edge of the given AABB such that the Y
 * coordinate of each edge coordinate returned is equal to the Y of the base of
 * the AABB
 *
 * @param aabb The AABB whose edge midpoints are to be found
 * @return vector<Pos> The list of size 4 containing the top,right,bottom and
 * left edge midpoints respectively
 */
vector<Pos> getAABBEdgeMidpointAtBase(AABB* aabb) {
    int midX = (*aabb).getMidpointX();
    int midZ = (*aabb).getMidpointZ();
    int base = (*aabb).getTopLeft().getY() + 1;

    Pos topEdgeMid((*aabb).getTopLeft());
    topEdgeMid.setX(midX);
    topEdgeMid.setY(base);

    Pos bottomEdgeMid((*aabb).getBottomRight());
    bottomEdgeMid.setX(midX);
    bottomEdgeMid.setY(base);

    Pos leftEdgeMid((*aabb).getTopLeft());
    leftEdgeMid.setZ(midZ);
    leftEdgeMid.setY(base);

    Pos rightEdgeMid((*aabb).getBottomRight());
    rightEdgeMid.setZ(midZ);
    rightEdgeMid.setY(base);

    vector<Pos> midEdgesAtBase;
    midEdgesAtBase.push_back(topEdgeMid);
    midEdgesAtBase.push_back(rightEdgeMid);
    midEdgesAtBase.push_back(bottomEdgeMid);
    midEdgesAtBase.push_back(leftEdgeMid);

    return midEdgesAtBase;
}

/**
 * @brief Generates all 4 doors for the given AABB and adds those blocks to the
 * list that the given world must keep track of
 *
 * @param worldptr The world in which doors are to be added
 * @param aabb The AABB whose doors are to be generated
 */
void generateAllDoorsInAABB(World* worldptr, AABB* aabb) {
    // Get edge midpoints for the AABB because that is where the doors will be
    // placed
    vector<Pos> edges = getAABBEdgeMidpointAtBase(aabb);
    Pos topEdgeMid(edges.at(0));
    Pos rightEdgeMid(edges.at(1));
    Pos bottomEdgeMid(edges.at(2));
    Pos leftEdgeMid(edges.at(3));

    // Use the coordinates to create door blocks
    Block topDoor("door", "door", &topEdgeMid);
    Block bottomDoor("door", "door", &bottomEdgeMid);
    Block leftDoor("door", "door", &leftEdgeMid);
    Block rightDoor("door", "door", &rightEdgeMid);

    // Tell the world to keep track of the blocks
    (*worldptr).addBlock(&topDoor);
    (*worldptr).addBlock(&bottomDoor);
    (*worldptr).addBlock(&leftDoor);
    (*worldptr).addBlock(&rightDoor);
}

/**
 * @brief A high level function that generates the doors and victims in each
 * AABB by calling the appropriate function
 *
 * @param worldptr The world in which everything is to be added
 */
void generateBlocks(World* worldptr) {
    for (auto aabb : *((*worldptr).getAABBList())) {
        if (strcmp(aabb.getMaterial().c_str(), "lava") != 0) {
            generateAllDoorsInAABB(worldptr, &aabb);
        }
    }
}

/**
 * @brief A high level function that generates the AABB grid in a world and
 * then generates the blocks associated with those AABB
 *
 * @param N The number of AABB on each axis
 * @param sep The separation between AABB in the cardinal directions
 * @param AABB_size The size of each cubic AABB
 * @param AABB_material The material each AABB is made of
 * @return World The generated grid world
 */
World generateGridWorld(int N, int sep, int AABB_size, string AABB_material) {
    World world;
    generateAABBGrid(&world, N, sep, AABB_size, AABB_material);
    generateBlocks(&world);
    return world;
}

int main(int argc, char* argv[]) {
    int N = 3;
    int sep = 30;       // Separation defaults to 0
    int AABB_size = 10; // Size defaults to 10

    // Process input and generate output
    cout << "Generating gridworld..." << endl;
    World world = generateGridWorld(N, sep, AABB_size, "planks");
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