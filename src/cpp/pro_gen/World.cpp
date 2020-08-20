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

vector<Object*>& World::getObjectList() { return this->objectList; }

void World::addAABB(AABB& aabb) { (this->aabbList).push_back(&aabb); }

void World::addBlock(Block& block) { (this->blockList).push_back(&block); }

void World::addEntity(Entity& entity) { this->entityList.push_back(&entity); }

void World::addObject(Object& object) { this->objectList.push_back(&object); }

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
    json world_json;

    vector<json> location_list;
    vector<json> entity_list;
    vector<json> object_list;

    world_json["locations"] = location_list;
    world_json["entities"] = entity_list;
    world_json["objects"] = object_list;

    // Add AABBs to the JSON list
    for (auto& aabbPtr : this->aabbList) {
        (*aabbPtr).toJSON(world_json);
    }

    for (auto& blockPtr : this->getBlockList()) {
        (*blockPtr).toJSON(world_json);
    }

    for (auto& entityPtr : this->getEntityList()) {
        (*entityPtr).toJSON(world_json);
    }

    for (auto& objectPtr : this->getObjectList()) {
        (*objectPtr).toJSON(world_json);
    }

    return world_json.dump(4);
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
