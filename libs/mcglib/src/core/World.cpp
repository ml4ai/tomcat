/**
 * @file World.cpp
 * @brief This file implements the methods in the World class.
 */
#include <fstream>
#include <iostream>

#include "mcglib/World.h"

using namespace std;
using json = nlohmann::json;

World::World() {}

mt19937_64& World::getRandom() { return this->gen; }

void World::setRandom(int seed) {
    mt19937_64 newGen(seed);
    this->gen = newGen;
}

vector<unique_ptr<AABB>>& World::getAABBList() { return (this->aabbList); }

vector<unique_ptr<Block>>& World::getBlockList() { return (this->blockList); }

vector<unique_ptr<Entity>>& World::getEntityList() { return this->entityList; }

vector<unique_ptr<Object>>& World::getObjectList() { return this->objectList; }

vector<unique_ptr<Connection>>& World::getConnectionList() {
    return this->connectionList;
}

void World::addAABB(unique_ptr<AABB> aabb) {
    (this->aabbList).push_back(move(aabb));
}

void World::addBlock(unique_ptr<Block> block) {
    (this->blockList).push_back(move(block));
}

void World::addEntity(unique_ptr<Entity> entity) {
    this->entityList.push_back(move(entity));
}

void World::addObject(unique_ptr<Object> object) {
    this->objectList.push_back(move(object));
}

void World::addConnection(unique_ptr<Connection> connection) {
    this->connectionList.push_back(move(connection));
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

    return world_json.dump();
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

    return world_json.dump();
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
