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

AABB chooseZombieworldAABB(int idCtr, Pos* topLeft, Pos* bottomRight) {
    if (idCtr % 2 == 0) {
        Pos newBottomRight(*bottomRight);
        newBottomRight.setY((*topLeft).getY());

        boost::random::uniform_int_distribution<> dist(1, 100);
        int rand = dist(gen);
        if (rand <= 25) {
            Pos newTopLeft(*topLeft);
            newTopLeft.shiftY(1);
            newBottomRight.shiftY(1);

            AABB curr(idCtr, "room", "air", &newTopLeft, &newBottomRight);
            return curr;
        }
        else if (rand > 25 && rand <= 90) {
            Pit waterPit(idCtr, "grass", topLeft, &newBottomRight);
            waterPit.generateFluidSquareAtBase("water", 2, 2, 2, 2);
            return waterPit;
        }
        else {
            Pit lavaPit(idCtr, "grass", topLeft, &newBottomRight);
            lavaPit.generateFluidSquareAtBase("lava", 2, 2, 2, 2);
            return lavaPit;
        };
    }

    else {
        AABB aabb(idCtr, "room", "planks", topLeft, bottomRight);
        return aabb;
    }
}

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

            AABB curAABB = chooseZombieworldAABB(idCtr, &topLeft, &bottomRight);
            (*worldptr).addAABB(&curAABB);
            prevTopLeft = topLeft;
            prevBottomRight = bottomRight;
        }
        else {
            // Condition for the next AABB in the current row
            Pos topLeft(prevTopLeft);
            topLeft.setX(prevBottomRight.getX() + sep);

            Pos bottomRight(prevBottomRight);
            bottomRight.setX(topLeft.getX() + AABB_size);

            AABB curAABB = chooseZombieworldAABB(idCtr, &topLeft, &bottomRight);
            (*worldptr).addAABB(&curAABB);
            prevTopLeft = topLeft;
            prevBottomRight = bottomRight;
        }
    }
}

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
    Block topDoor("door", "door", &topEdgeMid);
    Block bottomDoor("door", "door", &bottomEdgeMid);
    Block leftDoor("door", "door", &leftEdgeMid);
    Block rightDoor("door", "door", &rightEdgeMid);

    // Add it to the AABB's doors
    (*aabb).addBlock(&topDoor);
    (*aabb).addBlock(&bottomDoor);
    (*aabb).addBlock(&leftDoor);
    (*aabb).addBlock(&rightDoor);
}

void generateBlocks(World* worldptr) {
    for (auto& aabb : *(*worldptr).getAABBList()) {
        if (strcmp(aabb.getType().c_str(), "pit") != 0) {
            generateAllDoorsInAABB(&aabb);
        }
    }
}

World generateGridWorld(int N, int sep, int AABB_size, string AABB_material) {
    World world;
    generateAABBGrid(&world, N, sep, AABB_size, AABB_material);
    generateBlocks(&world);
    return world;
}

int main(int argc, char* argv[]) {
    int N = 3;
    int sep = 15;
    int AABB_size = 10;

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