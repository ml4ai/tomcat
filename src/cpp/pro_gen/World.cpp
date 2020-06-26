#include "World.h"
#include <string>
#include <iostream>

using namespace std;

World::World() {}

void World::addAABB(AABB *aabb) { (this->aabbList).push_back(*aabb); }

void World::addBlock(Block *block) { (this->blockList).push_back(*block); }

void World::toJSON(string filename) {
    cout << "TODO: toJSON isn't imlemented yet." << endl;
}

vector<AABB> * World::getAABBList(){
    return &(this -> aabbList);
}

vector<Block> * World::getBlockList(){
    return &(this -> blockList);
}

string World::toString(){
    string retval = "AABB List:\n\n";
    for(auto aabb : this->aabbList){
        retval += aabb.toString() + "\n";
    }

    retval += "\nBlock List:\n\n";
    for(auto block : this->blockList){
        retval += block.toString() + "\n";
    }
    return retval;
}


World::~World(){}
