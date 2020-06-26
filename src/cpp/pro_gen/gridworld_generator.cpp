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

void generateAABBGrid(
    World * worldptr, int N, int sep, int AABB_size, string  material) {
    int idCtr = 1;
    Pos topLeft(1, 0, 1);
    Pos bottomRight(AABB_size, AABB_size, AABB_size);

    AABB prevAABB(idCtr, material, &topLeft, &bottomRight);
    (*worldptr).addAABB(&prevAABB);

    while (idCtr <= (N * N - 1)) {
        idCtr++;

        if ((idCtr - 1) % N == 0) {
            Pos newTopLeft(1, 0, 1);
            newTopLeft.setZ(prevAABB.getBottomRight().getZ() + sep);

            Pos newBottomRight(
                AABB_size, AABB_size, newTopLeft.getZ() + AABB_size);
            AABB curAABB(idCtr, material, &newTopLeft, &newBottomRight);
            (*worldptr).addAABB(&curAABB);
            prevAABB = curAABB;
        }
        else {
            Pos newTopLeft = prevAABB.getTopLeft();
            newTopLeft.setX(prevAABB.getBottomRight().getX() + sep);

            Pos newBottomRight = prevAABB.getBottomRight();
            newBottomRight.setX(newTopLeft.getX() + AABB_size);

            AABB curAABB(idCtr, material, &newTopLeft, &newBottomRight);
            (*worldptr).addAABB(&curAABB);
            prevAABB = curAABB;
        }
    }
}

void generateVictimInAABB(World * worldptr, AABB * aabb) {
    Pos randPos((*aabb).getRandomPosAtBase(2, 2, 2, 2));
    boost::random::uniform_int_distribution<> dist(1, 100);
    int randInteger = dist(gen);

    if (randInteger <= 75) {
        ProceduralGenerator pgen;
        Block victim = pgen.getRandomVictim(&randPos, 0.60, &gen);
        (*worldptr).addBlock(&victim);
    }
    else {
        ;
    }
}

vector<Pos> getAABBEdgeMidpointAtBase(AABB * aabb) {
    int midX = (*aabb).getMidpointX();
    int midZ = (*aabb).getMidpointZ();
    int base = (*aabb).getTopLeft().getY();

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

void generateAllDoorsInAABB(World *worldptr, AABB *aabb) {
    vector<Pos> edges = getAABBEdgeMidpointAtBase(aabb);
    Pos topEdgeMid(edges.at(0));
    Pos rightEdgeMid(edges.at(1));
    Pos bottomEdgeMid(edges.at(2));
    Pos leftEdgeMid(edges.at(3));

    Block topDoor("door", "oak_door", &topEdgeMid);
    Block bottomDoor("door", "oak_door", &bottomEdgeMid);
    Block leftDoor("door", "oak_door", &leftEdgeMid);
    Block rightDoor("door", "oak_door", &rightEdgeMid);

    (*worldptr).addBlock(&topDoor);
    (*worldptr).addBlock(&rightDoor);
    (*worldptr).addBlock(&leftDoor);
    (*worldptr).addBlock(&rightDoor);
}

void generateBlocks(World * worldptr) {
    for (auto& aabb : *((*worldptr).getAABBList())) {
        generateAllDoorsInAABB(worldptr, &aabb);
        generateVictimInAABB(worldptr, &aabb);
    }

}

World generateGridWorld(
    int N, int sep, int AABB_size, string AABB_material) {
    World world;
    generateAABBGrid(&world, N, sep, AABB_size, AABB_material);
    generateBlocks(&world);
    return world;
}

int main(int argc, char *argv[]) {
    int N;
    int sep = 0;
    int AABB_size = 10;

    po::options_description desc("Allowed options");
    desc.add_options()
            ("help", "produce help message")
            ("N", po::value<int>(), "The number of AABB on an axis. Grid generated is N*N.")
            ("sep", po::value<int>(), "The separation between AABB in the cardinal directions. Defaults to 0.")
            ("aabb_size", po::value<int>(), "The size of the cubic AABB. Defaults to 10.")
        ;

    po::variables_map vm;
    po::store(po::command_line_parser(argc, argv).options(desc).run(), vm);
    po::notify(vm);

    if (!vm.count("N")){
        cout << "No grid size specified." << endl;
        return 1;
    }else{
        N = vm["N"].as<int>();
    }

    if(vm.count("sep")){
        sep = vm["sep"].as<int>();
    }

    if(vm.count("aabb_size")){
        AABB_size = vm["aabb_size"].as<int>();
    }

    cout << "Generating gridworld..." << endl;
    World world = generateGridWorld(N,sep,AABB_size, "planks");
    cout << "Writing to file..." << endl;
    ofstream outputFile("../../../../external/malmo/Minecraft/run/procedural.json", ios::out);
    outputFile << world.toJSON();
    outputFile.close();
    cout << "Done. The generated file is in Minecraft/run/" << endl;

    return 0;
}