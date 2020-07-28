/**
 * @file World.cpp
 * @brief This file implements the methods in the World class.
 */
#include "World.h"
#include <fstream>
#include <iostream>

using namespace std;
using json = nlohmann::json;

World::World() {}

mt19937_64& World::getRandom() { return this->gen; }

void World::setRandom(int seed) {
    mt19937_64 newGen(seed);
    this->gen = newGen;
}

vector<AABB*>& World::getAABBList() { return (this->aabbList); }

vector<Block*>& World::getBlockList() { return (this->blockList); }

vector<Entity*>& World::getEntityList() { return this->entityList; }

void World::addAABB(AABB& aabb) { (this->aabbList).push_back(&aabb); }

void World::addBlock(Block& block) { (this->blockList).push_back(&block); }

void World::addEntity(Entity& entity) { this->entityList.push_back(&entity); }

string World::toTSV() {
    string retval = "";

    for (auto aabbPtr : (this->aabbList)) {
        retval += (*aabbPtr).toTSV();
    }

    for (auto& blockPtr : (this->blockList)) {
        retval += (*blockPtr).toTSV() + "\n";
    }

    for (auto& entityPtr : (this->entityList)) {
        retval += (*entityPtr).toTSV() + "\n";
    }

    return retval;
}

string World::toJSON() {
    vector<json> json_aabb_list;
    vector<json> json_block_list;
    vector<json> json_entity_list;

    // Add AABBs to the JSON list
    for (auto& aabbPtr : this->aabbList) {
        json aabb_json = (*aabbPtr).toJSON();
        json_aabb_list.push_back(aabb_json);
    }

    // Add Blocks to the JSON List
    for (auto& blockPtr : this->blockList) {
        json block_json = (*blockPtr).toJSON();
        json_block_list.push_back(block_json);
    }

    for (auto& entityPtr : this->blockList) {
        json entity_json = (*entityPtr).toJSON();
        json_entity_list.push_back(entity_json);
    }

    json j;
    j["aabb_list"] = json_aabb_list;
    j["block_list"] = json_block_list;
    j["entity_list"] = json_entity_list;
    return j.dump(4);
}

void World::writeToFile(string jsonPath, string tsvPath) {
    cout << "Writing to file..." << endl;

    // Write JSON
    ofstream outputJSON(jsonPath, ios::out);
    outputJSON << this->toJSON();
    outputJSON.close();

    // Write TSV
    ofstream outputTSV(tsvPath, ios::out);
    outputTSV << this->toTSV();
    outputTSV.close();
}

World::~World() {}
