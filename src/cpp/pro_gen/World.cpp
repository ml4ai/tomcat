#include "World.h"
#include <iostream>

World::World() {}

void World::addAABB(AABB aabb) { (this->aabbList).push_back(aabb); }

void World::addBlock(Block block) { (this->blockList).push_back(block); }

void World::toJSON(string filename) {
    cout << "TODO: toJSON isn't imlemented yet." << endl;
}

vector<AABB> World::getAABBList(){
    return this -> aabbList;
}

vector<Block> World::getBlockList(){
    return this -> blockList;
}


World::~World(){}