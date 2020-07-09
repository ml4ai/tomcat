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

#include "ProceduralGenerator.h"
#include "World.h"
#include <boost/program_options.hpp>
#include <fstream>
#include <iostream>
#include <random>

using namespace std;
random_device r;
mt19937_64 gen(r());
namespace po = boost::program_options;

Block getRandomVictim(Pos* pos, double greenBias) {
    uniform_int_distribution<> dist(1, 100);
    double greenProbability = greenBias * 100;
    int randomInt = dist(gen);
    if (randomInt <= greenProbability) {
        Block block("prismarine", pos, "victim");
        return block;
    }
    else {
        Block block("gold", pos, "victim");
        return block;
    }
}

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
    Pos topLeft(1, 3, 1);
    Pos bottomRight(AABB_size, 3 + AABB_size, AABB_size);

    AABB prevAABB(idCtr, "room", material, &topLeft, &bottomRight, true, false);
    (*worldptr).addAABB(&prevAABB);

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
            AABB curAABB(idCtr,
                         "room",
                         material,
                         &newTopLeft,
                         &newBottomRight,
                         true,
                         false);
            (*worldptr).addAABB(&curAABB);
            prevAABB = curAABB;
        }
        else {
            // Condition for the next AABB in the current row
            Pos newTopLeft = prevAABB.getTopLeft();
            newTopLeft.setX(prevAABB.getBottomRight().getX() + sep);

            Pos newBottomRight = prevAABB.getBottomRight();
            newBottomRight.setX(newTopLeft.getX() + AABB_size);

            AABB curAABB(idCtr,
                         "room",
                         material,
                         &newTopLeft,
                         &newBottomRight,
                         true,
                         false);
            (*worldptr).addAABB(&curAABB);
            prevAABB = curAABB;
        }
    }
}

/**
 * @brief Adds a random victim to the aabb's list of blocks such that the
 * victim is in a random position inside the given AABB. The addition of victims
 * is random so this function won't add a victim every time it is called
 *
 * @param aabb The AABB within which the victim is to ve generated
 */
void generateVictimInAABB(AABB* aabb) {
    Pos randPos((*aabb).getRandomPosAtBase(&gen, 2, 2, 2, 2));
    randPos.setY(randPos.getY() + 1);

    uniform_int_distribution<> dist(1, 100);
    int randInteger = dist(gen);

    if (randInteger <= 75) {
        Block victim = getRandomVictim(&randPos, 0.60);
        (*aabb).addBlock(&victim);
    }
    else {
        ;
    }
}

/**
 * @brief Generates all 4 doors for the given AABB to keep track of.
 *
 * @param aabb The AABB whose doors are to be generated
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
 * @brief A high level function that generates the doors and victims in each
 * AABB by calling the appropriate function
 *
 * @param worldptr The world in which everything is to be added
 */
void generateBlocks(World* worldptr) {
    for (auto aabb : *((*worldptr).getAABBList())) {
        generateAllDoorsInAABB(aabb);
        generateVictimInAABB(aabb);
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
    int N;
    int sep = 0;        // Separation defaults to 0
    int AABB_size = 10; // Size defaults to 10
    string jsonPath =
        "../../../../external/malmo/Minecraft/run/procedural.json";
    string tsvPath =
        "../../../../external/malmo/Minecraft/run/procedural.tsv"; // Default
                                                                   // locations

    // Handle options
    po::options_description desc("Allowed options");
    desc.add_options()("help", "produce help message")(
        "N",
        po::value<int>(),
        "The number of AABB on an axis. Grid generated is N*N.")(
        "sep",
        po::value<int>(),
        "The separation between AABB in the cardinal directions. Defaults to "
        "0.")("aabb_size",
              po::value<int>(),
              "The size of the cubic AABB. Defaults to 10.")(
        "json_path",
        po::value<std::string>(),
        "Specify where to save the JSON file with filename an extension. "
        "Defaults to procedural.json in Minecraft/run/")(
        "tsv_path",
        po::value<std::string>(),
        "Specify where to save the TSV file with filename an extension. "
        "Defaults to procedural.tsv in Minecraft/run/");

    po::variables_map vm;
    po::store(po::command_line_parser(argc, argv).options(desc).run(), vm);
    po::notify(vm);

    if (vm.count("help")) {
        cout << desc << endl;
        return 0;
    }

    if (!vm.count("N")) {
        cout << "No grid size specified." << endl;
        return 1;
    }
    else {
        N = vm["N"].as<int>();
    }

    if (vm.count("sep")) {
        sep = vm["sep"].as<int>();
    }

    if (vm.count("aabb_size")) {
        AABB_size = vm["aabb_size"].as<int>();
    }

    if (vm.count("json_path")) {
        jsonPath = vm["json_path"].as<std::string>();
    }

    if (vm.count("tsv_path")) {
        tsvPath = vm["tsv_path"].as<std::string>();
    }

    // Process input and generate output
    cout << "Generating gridworld..." << endl;
    World world = generateGridWorld(N, sep, AABB_size, "planks");
    cout << "Writing to file..." << endl;

    // Write JSON
    ofstream outputJSON(jsonPath, ios::out);
    outputJSON << world.toJSON();
    outputJSON.close();

    // Write TSV
    ofstream outputTSV(tsvPath, ios::out);
    outputTSV << world.toTSV();
    outputTSV.close();

    cout << "Done. The generated files are in Minecraft/run/procedural.json "
            "and Minecraft/run/procedural.tsv"
         << endl;

    return 0;
}