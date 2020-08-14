#include "Group.h"
#include <iostream>
using namespace std;
using json = nlohmann::json;

Group::Group(string id)
    : AABB(id, "group", "air", *(new Pos()), *(new Pos()), true, false) {}

void Group::addAABB(AABB& aabb) {
    this->aabbList.push_back(&aabb);
    this->recalculateGroupBoundaries();
}

void Group::generateAllDoorsInAABB() {
    for (auto& aabb : this->aabbList) {
        aabb->generateAllDoorsInAABB();
    }
}

vector<AABB*>& Group::getAABBList() { return this->aabbList; }

AABB* Group::getAABB(string id) {
    for (auto& aabb : this->aabbList) {
        if (strcmp((*aabb).getID().c_str(), id.c_str()) == 0) {
            return aabb;
        }
    }
    return nullptr;
}

void Group::recalculateGroupBoundaries() {

    int minX, minY, minZ;
    int maxX, maxY, maxZ;
    bool isFirst = true;

    for (auto& aabb : this->aabbList) {

        Pos topLeft = (*aabb).getTopLeft();
        int x1 = topLeft.getX(), y1 = topLeft.getY(), z1 = topLeft.getZ();

        Pos bottomRight = (*aabb).getBottomRight();
        int x2 = bottomRight.getX(), y2 = bottomRight.getY(),
            z2 = bottomRight.getZ();

        if (isFirst) {
            minX = x1;
            minY = y1;
            minZ = z1;
            maxX = x2;
            maxY = y2;
            maxZ = z2;
            isFirst = false;
        }

        if (x1 < minX) {
            minX = x1;
        }
        if (y1 < minY) {
            minY = y1;
        }
        if (z1 < minZ) {
            minZ = z1;
        }
        if (x2 > maxX) {
            maxX = x2;
        }
        if (y2 > maxY) {
            maxY = y2;
        }
        if (z2 > maxZ) {
            maxZ = z2;
        }
    }

    Pos newTopLeft(minX, minY, minZ);
    Pos newBottomRight(maxX, maxY, maxZ);

    this->setTopLeft(newTopLeft);
    this->setBottomRight(newBottomRight);
}

json Group::toJSON() {
    json group_json;

    vector<json> coordinates_list;
    coordinates_list.push_back(this->topLeft.toJSON());
    coordinates_list.push_back(this->bottomRight.toJSON());


    vector<json> block_list;
    for (auto& blockPtr : this->getBlockList()) {
        block_list.push_back((*blockPtr).toJSON());
    }

    vector<json> entity_list;
    for (auto& entityPtr : this->getEntityList()) {
        entity_list.push_back((*entityPtr).toJSON());
    }

    vector<json> aabb_list;
    for (auto& aabbPtr : this->getAABBList()) {
        aabb_list.push_back((*aabbPtr).toJSON());
    }

    group_json["bounds"] = {{"id", this->getID()},
                           {"type", "cuboid"},
                           {"coordinates", coordinates_list},
                           {"material", this->getMaterial()},
                           {"block_list", block_list},
                           {"aabb_list", aabb_list},
                           {"entity_list", entity_list}};

    return group_json;
}

string Group::toTSV() {
    string retval = "";

    for (auto aabb : this->aabbList) {
        retval += aabb->toTSV();
    }

    for (auto block : (this->getBlockList())) {
        retval += (*block).toTSV() + "\n";
    }

    return retval;
}

Group::~Group() {}
