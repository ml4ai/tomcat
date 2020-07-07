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
    vector<json> json_aabb_list;
    vector<json> json_block_list;

    // Add AABBs to the JSON list
    for (auto aabb : this->aabbList) {
        json aabb_json;
        aabb_json["id"] = to_string(aabb.getID());

        aabb_json["x1"] = to_string(aabb.getTopLeft().getX());
        aabb_json["y1"] = to_string(aabb.getTopLeft().getY());
        aabb_json["z1"] = to_string(aabb.getTopLeft().getZ());

        aabb_json["x2"] = to_string(aabb.getBottomRight().getX());
        aabb_json["y2"] = to_string(aabb.getBottomRight().getY());
        aabb_json["z2"] = to_string(aabb.getBottomRight().getZ());
        aabb_json["material"] = aabb.getMaterial();

        vector<json> aabb_block_list_json;
        for (auto block : *(aabb.getBlockList())) {
            json block_json;
            block_json["type"] = block.getType();
            block_json["x"] = to_string(block.getX());
            block_json["y"] = to_string(block.getY());
            block_json["z"] = to_string(block.getZ());
            block_json["material"] = block.getMaterial();
            aabb_block_list_json.push_back(block_json);
        }
        aabb_json["block_list"] = aabb_block_list_json;
        json_aabb_list.push_back(aabb_json);
    }

    // Add Blocks to the JSON List
    for (auto block : this->blockList) {
        json block_json;
        block_json["type"] = block.getType();
        block_json["x"] = to_string(block.getX());
        block_json["y"] = to_string(block.getY());
        block_json["z"] = to_string(block.getZ());
        block_json["material"] = block.getMaterial();
        json_block_list.push_back(block_json);
    }

    json j;
    j["aabb_list"] = json_aabb_list;
    j["block_list"] = json_block_list;
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
