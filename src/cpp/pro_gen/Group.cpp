#include "Group.h"
using namespace std;
using json = nlohmann::json;

Group::Group(string id) : AABB(id, "group", "air", true, false) {}

vector<unique_ptr<AABB>>& Group::getAABBList() { return this->aabbList; }

vector<unique_ptr<Connection>>& Group::getConnectionList() {
    return this->connectionList;
}

void Group::addAABB(unique_ptr<AABB> aabb) {
    this->aabbList.push_back(move(aabb));
    this->recalculateGroupBoundaries();
}

void Group::generateAllDoorsInAABB() {
    for (auto& aabb : this->aabbList) {
        aabb->generateAllDoorsInAABB();
    }
}

AABB* Group::getAABB(string id) {
    for (auto& aabb : this->aabbList) {
        if (strcmp((*aabb).getID().c_str(), id.c_str()) == 0) {
            return aabb.get();
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

void Group::addConnection(unique_ptr<Connection> connection) {
    this->connectionList.push_back(move(connection));
}

void Group::toSemanticMapJSON(json& json_base) {
    for (auto& aabbPtr : this->aabbList) {
        (*aabbPtr).toSemanticMapJSON(json_base);
    }

    for (auto& blockPtr : this->getBlockList()) {
        (*blockPtr).toSemanticMapJSON(json_base);
    }

    for (auto& entityPtr : this->getEntityList()) {
        (*entityPtr).toSemanticMapJSON(json_base);
    }

    for (auto& objectPtr : this->getObjectList()) {
        (*objectPtr).toSemanticMapJSON(json_base);
    }

    for (auto& connectionPtr : this->getConnectionList()) {
        (*connectionPtr).toSemanticMapJSON(json_base);
    }
}

void Group::toLowLevelMapJSON(json& json_base) {
    for (auto& aabbPtr : this->aabbList) {
        (*aabbPtr).toLowLevelMapJSON(json_base);
    }

    for (auto& blockPtr : this->getBlockList()) {
        (*blockPtr).toLowLevelMapJSON(json_base);
    }

    for (auto& entityPtr : this->getEntityList()) {
        (*entityPtr).toLowLevelMapJSON(json_base);
    }

    for (auto& objectPtr : this->getObjectList()) {
        (*objectPtr).toLowLevelMapJSON(json_base);
    }
}

Group::~Group() {}
