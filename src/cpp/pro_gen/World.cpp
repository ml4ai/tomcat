/**
 * @file World.cpp
 * @brief This file implements the methods in the World class.
 */
#include "World.h"
#include <iostream>
#include <string>

using namespace std;
using json = nlohmann::json;

/**
 * @brief Construct a new World:: World object
 *
 */
World::World() {}

/**
 * @brief Add an AABB to the vector of AABB held inside the world
 *
 * @param aabb Address of the AABB to add
 */
void World::addAABB(AABB* aabb) { (this->aabbList).push_back(*aabb); }

/**
 * @brief Add a Block to the vector of Block held inside the world
 *
 * @param aabb Address of the Block to add
 */
void World::addBlock(Block* block) { (this->blockList).push_back(*block); }

string World::toTSV() {
    string retval = "";

    for (auto aabb : (this->aabbList)) {
        retval += aabb.toTSV();
    }

    for (auto block : (this->blockList)) {
        retval += block.toTSV() + "\n";
    }

    return retval;
}

/**
 * @brief Converts the world into a JSON representation with
 * each entry indented by 4 and returns the string representation of it.
 *
 * @return string The JSON as a string
 */
string World::toJSON() {
    vector<map<string, string>> aabb_map_list;
    vector<map<string, string>> block_map_list;

    // Add AABBs to the JSON list
    for (auto aabb : this->aabbList) {
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

    // Add Blocks to the JSON List
    for (auto block : this->blockList) {
        map<string, string> m;
        m["type"] = block.getType();
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

/**
 * @brief Returns a pointer to the vector that holds the AABBs
 *
 * @return vector<AABB>* The AABB list
 */
vector<AABB>* World::getAABBList() { return &(this->aabbList); }

/**
 * @brief Returns a pointer to the vector that holds the Blocks
 *
 * @return vector<Block>* The Block list
 */
vector<Block>* World::getBlockList() { return &(this->blockList); }

/**
 * @brief Destroy the World:: World object
 */
World::~World() {}
