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

vector<Connection*>& World::getConnectionList() { return this->connectionList; }

void World::addAABB(AABB& aabb) { (this->aabbList).push_back(&aabb); }

void World::addBlock(Block& block) { (this->blockList).push_back(&block); }

void World::addEntity(Entity& entity) { this->entityList.push_back(&entity); }

void World::addObject(Object& object) { this->objectList.push_back(&object); }

void World::addConnection(Connection& connection) {
    this->connectionList.push_back(&connection);
}

string World::toLowLevelMapJSON() {
    json world_json;

    vector<json> location_list;
    vector<json> entity_list;

    world_json["blocks"] = location_list;
    world_json["entities"] = entity_list;

    // Add AABBs to the JSON list
    for (auto& aabbPtr : this->aabbList) {
        (*aabbPtr).toLowLevelMapJSON(world_json);
    }

    for (auto& blockPtr : this->getBlockList()) {
        (*blockPtr).toLowLevelMapJSON(world_json);
    }

    for (auto& entityPtr : this->getEntityList()) {
        (*entityPtr).toLowLevelMapJSON(world_json);
    }

    for (auto& objectPtr : this->getObjectList()) {
        (*objectPtr).toLowLevelMapJSON(world_json);
    }

    return world_json.dump(4);
}

string World::toSemanticMapJSON() {
    json world_json;

    vector<json> location_list;
    vector<json> entity_list;
    vector<json> object_list;
    vector<json> connection_list;

    world_json["locations"] = location_list;
    world_json["entities"] = entity_list;
    world_json["objects"] = object_list;
    world_json["connections"] = connection_list;

    // Add AABBs to the JSON list
    for (auto& aabbPtr : this->aabbList) {
        (*aabbPtr).toSemanticMapJSON(world_json);
    }

    for (auto& blockPtr : this->getBlockList()) {
        (*blockPtr).toSemanticMapJSON(world_json);
    }

    for (auto& entityPtr : this->getEntityList()) {
        (*entityPtr).toSemanticMapJSON(world_json);
    }

    for (auto& objectPtr : this->getObjectList()) {
        (*objectPtr).toSemanticMapJSON(world_json);
    }

    for (auto& connectionPtr : this->getConnectionList()) {
        (*connectionPtr).toSemanticMapJSON(world_json);
    }

    return world_json.dump(4);
}

void World::writeToFile(string jsonPath, string altJSONPath) {
    cout << "Writing to file..." << endl;

    // Write JSON
    ofstream outputJSON(jsonPath, ios::out);
    outputJSON << this->toSemanticMapJSON();
    outputJSON.close();

    // Write TSV
    ofstream outputLowLevelMapJSON(altJSONPath, ios::out);
    outputLowLevelMapJSON << this->toLowLevelMapJSON();
    outputLowLevelMapJSON.close();
}

World::~World() {}
