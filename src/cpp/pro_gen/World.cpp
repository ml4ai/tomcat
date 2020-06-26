#include "World.h"
#include <string>
#include <iostream>

using namespace std;
using json = nlohmann::json;

World::World() {}

void World::addAABB(AABB *aabb) { (this->aabbList).push_back(*aabb); }

void World::addBlock(Block *block) { (this->blockList).push_back(*block); }

string World::toJSON() {
    vector<map<string,string>> aabb_map_list;
    vector<map<string,string>> block_map_list;

    for(auto aabb: this ->aabbList){
        map<string, string> m;
        m["id"] = to_string(aabb.getID());

        m["x1"] = to_string(aabb.getTopLeft().getX());
        m["y1"] = to_string(aabb.getTopLeft().getY());
        m["z1"] = to_string(aabb.getTopLeft().getZ());

        m["x2"] = to_string(aabb.getBottomRight().getX());
        m["y2"] = to_string(aabb.getBottomRight().getY());
        m["z2"] = to_string(aabb.getBottomRight().getZ());
        m["material"] = aabb.getMaterial();
        aabb_map_list.push_back(m);
    }

    for(auto block : this->blockList){
        map<string, string> m;
        m["name"] = block.getName();
        m["x"] = to_string(block.getX());
        m["y"] = to_string(block.getY());
        m["z"] = to_string(block.getZ());
        m["material"] = block.getMaterial();
        block_map_list.push_back(m);
    }

    json j;
    j["aabb_list"] = aabb_map_list;
    j["block_list"] = block_map_list;
    return j.dump(4);

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
