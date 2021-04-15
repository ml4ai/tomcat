/**
 * @file AABB.cpp
 * @brief This file implements the methods in the AABB class.
 */
#include "mcglib/AABB.h"
using namespace std;
using json = nlohmann::json;

AABB::AABB(string id,
           string type,
           string material,
           Pos& topLeft,
           Pos& bottomRight,
           bool isHollow,
           bool hasRoof,
           bool autoAdjust)
    : id{id}, type{type}, material{material}, topLeft{topLeft},
      bottomRight{bottomRight}, isHollow{isHollow}, hasRoof{hasRoof},
      autoAdjust{autoAdjust} {}

AABB::AABB(string id)
    : id{id}, type{"blank_canvas"}, material{"blank"}, topLeft(0, 0, 0),
      bottomRight(0, 0, 0), isHollow{true}, hasRoof{false} {
    this->autoAdjust = true;
}

void AABB::addAABB(unique_ptr<AABB> aabb) {
    this->aabbList.push_back(move(aabb));
    this->recalculateOverallBoundary();
}

void AABB::addConnection(unique_ptr<Connection> connection) {
    this->connectionList.push_back(move(connection));
}

string AABB::getID() { return this->id; }

string AABB::getMaterial() { return this->material; }

string AABB::getType() { return this->type; }

Pos AABB::getTopLeft() { return this->topLeft; }

Pos AABB::getBottomRight() { return this->bottomRight; }

vector<unique_ptr<Block>>& AABB::getBlockList() { return (this->blockList); }

vector<unique_ptr<Entity>>& AABB::getEntityList() { return this->entityList; }

vector<unique_ptr<Object>>& AABB::getObjectList() { return this->objectList; }

vector<unique_ptr<AABB>>& AABB::getAABBList() { return this->aabbList; }

vector<unique_ptr<Connection>>& AABB::getConnectionList() {
    return this->connectionList;
}

AABB* AABB::getSubAABB(string id) {
    for (auto& aabb : this->aabbList) {
        if (strcmp((*aabb).getID().c_str(), id.c_str()) == 0) {
            return aabb.get();
        }
    }
    return nullptr;
}

int AABB::getMidpointX() {
    int mid_x = ((this->topLeft).getX() +
                 ((this->bottomRight).getX() - (this->topLeft).getX()) / 2);
    return mid_x;
}

int AABB::getMidpointY() {
    int mid_y = ((this->topLeft).getY() +
                 ((this->bottomRight).getY() - (this->topLeft).getY()) / 2);
    return mid_y;
}

int AABB::getMidpointZ() {
    int mid_z = ((this->topLeft).getZ() +
                 ((this->bottomRight).getZ() - (this->topLeft).getZ()) / 2);
    return mid_z;
}

int AABB::getSizeX() {
    return (this->bottomRight.getX() - this->topLeft.getX());
}

int AABB::getSizeY() {
    return (this->bottomRight.getY() - this->topLeft.getY());
}

int AABB::getSizeZ() {
    return (this->bottomRight.getY() - this->topLeft.getY());
}

void AABB::shiftX(int shift) {

    this->topLeft.shiftX(shift);
    this->bottomRight.shiftX(shift);

    for (auto& aabbPtr : this->aabbList) {
        aabbPtr->shiftX(shift);
    }

    for (auto& blockPtr : this->getBlockList()) {
        blockPtr->shiftX(shift);
    }

    for (auto& entityPtr : this->getEntityList()) {
        entityPtr->shiftX(shift);
    }

    for (auto& objectPtr : this->getObjectList()) {
        objectPtr->shiftX(shift);
    }

    this->connectionList.clear();
}

void AABB::shiftY(int shift) {
    this->topLeft.shiftY(shift);
    this->bottomRight.shiftY(shift);

    for (auto& aabbPtr : this->aabbList) {
        aabbPtr->shiftY(shift);
    }

    for (auto& blockPtr : this->getBlockList()) {
        blockPtr->shiftY(shift);
    }

    for (auto& entityPtr : this->getEntityList()) {
        entityPtr->shiftY(shift);
    }

    for (auto& objectPtr : this->getObjectList()) {
        objectPtr->shiftY(shift);
    }

    this->connectionList.clear();
}

void AABB::shiftZ(int shift) {
    this->topLeft.shiftZ(shift);
    this->bottomRight.shiftZ(shift);

    for (auto& aabbPtr : this->aabbList) {
        aabbPtr->shiftZ(shift);
    }

    for (auto& blockPtr : this->getBlockList()) {
        blockPtr->shiftZ(shift);
    }

    for (auto& entityPtr : this->getEntityList()) {
        entityPtr->shiftZ(shift);
    }

    for (auto& objectPtr : this->getObjectList()) {
        objectPtr->shiftZ(shift);
    }

    this->connectionList.clear();
}

void AABB::shift(int shiftX, int shiftY, int shiftZ) {
    this->topLeft.shift(shiftX, shiftY, shiftZ);
    this->bottomRight.shift(shiftX, shiftY, shiftZ);

    for (auto& aabbPtr : this->aabbList) {
        aabbPtr->shift(shiftX, shiftY, shiftZ);
    }

    for (auto& blockPtr : this->getBlockList()) {
        blockPtr->shift(shiftX, shiftY, shiftZ);
    }

    for (auto& entityPtr : this->getEntityList()) {
        entityPtr->shift(shiftX, shiftY, shiftZ);
    }

    for (auto& objectPtr : this->getObjectList()) {
        objectPtr->shift(shiftX, shiftY, shiftZ);
    }

    this->connectionList.clear();
}

Pos AABB::getRandomPos(mt19937_64& gen,
                       int offsetPosX,
                       int offsetNegX,
                       int offsetPosY,
                       int offsetNegY,
                       int offsetPosZ,
                       int offsetNegZ) {

    int startX = (this->topLeft).getX() + offsetPosX;
    int startY = (this->topLeft).getY() + offsetPosY;
    int startZ = (this->topLeft).getZ() + offsetPosZ;

    int endX = (this->bottomRight).getX() - offsetNegX;
    int endY = (this->bottomRight).getY() - offsetNegY;
    int endZ = (this->bottomRight).getZ() - offsetNegZ;

    uniform_int_distribution<> randXGen(startX, endX);
    uniform_int_distribution<> randYGen(startY, endY);
    uniform_int_distribution<> randZGen(startZ, endZ);

    int randX = randXGen(gen);
    int randY = randYGen(gen);
    int randZ = randZGen(gen);

    Pos pos(randX, randY, randZ);

    return pos;
}

vector<Pos> AABB::getEdgeMidpointAtBase() {
    int midX = this->getMidpointX();
    int midZ = this->getMidpointZ();
    int base = this->getTopLeft().getY();

    Pos topEdgeMid(this->getTopLeft());
    topEdgeMid.setX(midX);
    topEdgeMid.setY(base);

    Pos bottomEdgeMid(this->getBottomRight());
    bottomEdgeMid.setX(midX);
    bottomEdgeMid.setY(base);

    Pos leftEdgeMid(this->getTopLeft());
    leftEdgeMid.setZ(midZ);
    leftEdgeMid.setY(base);

    Pos rightEdgeMid(this->getBottomRight());
    rightEdgeMid.setZ(midZ);
    rightEdgeMid.setY(base);

    vector<Pos> midEdgesAtBase;
    midEdgesAtBase.push_back(topEdgeMid);
    midEdgesAtBase.push_back(rightEdgeMid);
    midEdgesAtBase.push_back(bottomEdgeMid);
    midEdgesAtBase.push_back(leftEdgeMid);

    return midEdgesAtBase;
}

void AABB::setTopLeft(Pos& topLeft) { this->topLeft = topLeft; }

void AABB::setBottomRight(Pos& bottomRight) { this->bottomRight = bottomRight; }

void AABB::setMaterial(string material) { this->material = material; };

void AABB::addBlock(unique_ptr<Block> block) {
    (this->blockList).push_back(move(block));
}

void AABB::addEntity(unique_ptr<Entity> entity) {
    this->entityList.push_back(move(entity));
}

void AABB::addObject(unique_ptr<Object> object) {
    this->objectList.push_back(move(object));
}

bool AABB::intersects(AABB& other) {
    int thisX1 = this->topLeft.getX();
    int thisX2 = this->bottomRight.getX();
    int thisY1 = this->topLeft.getY();
    int thisY2 = this->bottomRight.getY();
    int thisZ1 = this->topLeft.getZ();
    int thisZ2 = this->bottomRight.getZ();

    int otherX1 = other.topLeft.getX();
    int otherX2 = other.bottomRight.getX();
    int otherY1 = other.topLeft.getY();
    int otherY2 = other.bottomRight.getY();
    int otherZ1 = other.topLeft.getZ();
    int otherZ2 = other.bottomRight.getZ();

    if (thisX2 < otherX1 || otherX2 < thisX1 || thisY2 < otherY1 ||
        otherY2 < thisY1 || thisZ2 < otherZ1 || otherZ2 < thisZ1) {
        return false;
    }
    else {
        return true;
    }
}

void AABB::generateBox(string material,
                       int offsetPosX,
                       int offsetNegX,
                       int offsetPosY,
                       int offsetNegY,
                       int offsetPosZ,
                       int offsetNegZ) {

    int startX = this->getTopLeft().getX() + offsetPosX;
    int startY = this->getTopLeft().getY() + offsetPosY;
    int startZ = this->getTopLeft().getZ() + offsetPosZ;

    int endX = this->getBottomRight().getX() - offsetNegX;
    int endY = this->getBottomRight().getY() - offsetNegY;
    int endZ = this->getBottomRight().getZ() - offsetNegZ;

    for (int x = startX; x <= endX; x++) {
        for (int y = startY; y <= endY; y++) {
            for (int z = startZ; z <= endZ; z++) {
                Pos pos(x, y, z);
                auto curBlock = make_unique<Block>(material, pos);
                this->addBlock(move(curBlock));
            }
        }
    }
}

void AABB::addRandomBlocks(int n,
                           string material,
                           mt19937_64& gen,
                           int offsetPosX,
                           int offsetNegX,
                           int offsetPosY,
                           int offsetNegY,
                           int offsetPosZ,
                           int offsetNegZ) {

    int startX = this->getTopLeft().getX() + offsetPosX;
    int startY = this->getTopLeft().getY() + offsetPosY;
    int startZ = this->getTopLeft().getZ() + offsetPosZ;

    int endX = this->getBottomRight().getX() - offsetNegX;
    int endY = this->getBottomRight().getY() - offsetNegY;
    int endZ = this->getBottomRight().getZ() - offsetNegZ;

    while (n > 0) {
        uniform_int_distribution<> randX(startX, endX);
        uniform_int_distribution<> randY(startY, endY);
        uniform_int_distribution<> randZ(startZ, endZ);

        int x = randX(gen);
        int y = randY(gen);
        int z = randZ(gen);
        Pos pos(x, y, z);
        auto curBlock = make_unique<Block>(material, pos);
        this->addBlock(move(curBlock));
        n--;
    }
}

void AABB::generateAllDoorsInAABB() {
    if (strcmp(this->type.c_str(), "blank_canvas") != 0) {
        vector<Pos> edges = this->getEdgeMidpointAtBase();
        Pos topEdgeMid(edges.at(0));
        Pos rightEdgeMid(edges.at(1));
        Pos bottomEdgeMid(edges.at(2));
        Pos leftEdgeMid(edges.at(3));

        // Since points are at base we want them to be at base + 1
        topEdgeMid.shiftY(1);
        bottomEdgeMid.shiftY(1);
        leftEdgeMid.shiftY(1);
        rightEdgeMid.shiftY(1);

        auto topDoor = make_unique<Door>(topEdgeMid, false, false);
        auto bottomDoor = make_unique<Door>(bottomEdgeMid, false, false);
        auto leftDoor = make_unique<Door>(
            leftEdgeMid, false, false, "dark_oak_door", "east");
        auto rightDoor = make_unique<Door>(
            rightEdgeMid, false, false, "dark_oak_door", "east");

        this->addBlock(move(topDoor));
        this->addBlock(move(bottomDoor));
        this->addBlock(move(leftDoor));
        this->addBlock(move(rightDoor));
    }

    for (auto& aabb : this->aabbList) {
        aabb->generateAllDoorsInAABB();
    }
}

void AABB::recalculateOverallBoundary() {

    if (this->autoAdjust) {

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
}

void AABB::toSemanticMapJSON(json& json_base) {
    json aabb_json;

    vector<json> coordinates_list;
    coordinates_list.push_back(this->topLeft.toSemanticMapJSON());
    coordinates_list.push_back(this->bottomRight.toSemanticMapJSON());

    aabb_json["bounds"] = {{"type", "cuboid"},
                           {"coordinates", coordinates_list}};

    aabb_json["id"] = this->getID();
    aabb_json["material"] = this->getMaterial();

    vector<string> child_locations;
    for (auto& aabbPtr : this->aabbList) {
        (*aabbPtr).toSemanticMapJSON(json_base);
        child_locations.push_back(aabbPtr->getID());
    }

    aabb_json["child_locations"] = child_locations;
    json_base["locations"].push_back(aabb_json);

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

void AABB::toLowLevelMapJSON(json& json_base) {

    for (int x = (this->topLeft).getX(); x <= (this->bottomRight).getX(); x++) {
        for (int y = (this->topLeft).getY(); y <= (this->bottomRight).getY();
             y++) {
            for (int z = (this->topLeft).getZ();
                 z <= (this->bottomRight).getZ();
                 z++) {
                bool addCurrent = true;

                if (isHollow) {
                    if (x > (this->topLeft).getX() &&
                        x < (this->bottomRight).getX() &&
                        z > (this->topLeft).getZ() &&
                        z < (this->bottomRight).getZ()) {
                        if (y != (this->topLeft).getY() &&
                            y != (this->bottomRight).getY()) {
                            addCurrent = false;
                        }
                    }
                }

                if (!hasRoof) {
                    if (y == (this->bottomRight).getY()) {
                        addCurrent = false;
                    }
                }

                if (addCurrent) {
                    Pos thisPos(x, y, z);
                    Block thisBlock(this->material, thisPos);
                    thisBlock.toLowLevelMapJSON(json_base);
                }
            }
        }
    }

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

AABB::~AABB() {}
