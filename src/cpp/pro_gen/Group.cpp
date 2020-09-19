#include "Group.h"
using namespace std;
using json = nlohmann::json;

Group::Group(string id)
    : AABB(id, "group", "air", *(new Pos()), *(new Pos()), true, false) {}

vector<AABB*>& Group::getAABBList() { return this->aabbList; }

vector<Connection*>& Group::getConnectionList() { return this->connectionList; }

void Group::addAABB(AABB& aabb) {
    this->aabbList.push_back(&aabb);
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

void Group::addConnection(Connection& connection) {
    this->connectionList.push_back(&connection);
}

void Group::toJSON(json& json_base) {
    for (auto& aabbPtr : this->aabbList) {
        (*aabbPtr).toJSON(json_base);
    }

    for (auto& blockPtr : this->getBlockList()) {
        (*blockPtr).toJSON(json_base);
    }

    for (auto& entityPtr : this->getEntityList()) {
        (*entityPtr).toJSON(json_base);
    }

    for (auto& objectPtr : this->getObjectList()) {
        (*objectPtr).toJSON(json_base);
    }

    for (auto& connectionPtr : this->getConnectionList()) {
        (*connectionPtr).toJSON(json_base);
    }
}

void Group::toAltJSON(json& json_base) {
    for (auto& aabbPtr : this->aabbList) {
        (*aabbPtr).toAltJSON(json_base);
    }

    for (auto& blockPtr : this->getBlockList()) {
        (*blockPtr).toAltJSON(json_base);
    }

    for (auto& entityPtr : this->getEntityList()) {
        (*entityPtr).toAltJSON(json_base);
    }

    for (auto& objectPtr : this->getObjectList()) {
        (*objectPtr).toAltJSON(json_base);
    }
}

Group::~Group() {}
