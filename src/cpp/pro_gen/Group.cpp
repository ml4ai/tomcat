#include "Group.h"
using namespace std;
using json = nlohmann::json;

Group::Group(int id)
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

AABB* Group::getAABB(int id) {
    for (auto& aabb : this->aabbList) {
        if ((*aabb).getID() == id) {
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
    vector<json> group_aabb_list;
    json group_json;

    group_json["id"] = to_string(this->getID());
    group_json["type"] = this->getType();

    group_json["x1"] = to_string(this->getTopLeft().getX());
    group_json["y1"] = to_string(this->getTopLeft().getY());
    group_json["z1"] = to_string(this->getTopLeft().getZ());

    group_json["x2"] = to_string(this->getBottomRight().getX());
    group_json["y2"] = to_string(this->getBottomRight().getY());
    group_json["z2"] = to_string(this->getBottomRight().getZ());
    group_json["material"] = this->getMaterial();

    vector<json> group_block_list_json;
    for (auto& blockPtr : this->getBlockList()) {
        json block_json = (*blockPtr).toJSON();
        group_block_list_json.push_back(block_json);
    }

    vector<json> group_entity_list_json;
    for (auto& entityPtr : this->getEntityList()) {
        json entity_json = (*entityPtr).toJSON();
        group_entity_list_json.push_back(entity_json);
    }

    for (auto& aabbPtr : this->aabbList) {
        json aabb_json = (*aabbPtr).toJSON();
        group_aabb_list.push_back(aabb_json);
    }

    group_json["block_list"] = group_block_list_json;
    group_json["entity_list"] = group_entity_list_json;
    group_json["aabb_list"] = group_aabb_list;
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
