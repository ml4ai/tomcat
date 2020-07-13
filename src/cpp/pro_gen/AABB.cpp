/**
 * @file AABB.cpp
 * @brief This file implements the methods in the AABB class.
 */
#include "AABB.h"
#include <iostream>

using namespace std;

AABB::AABB(int id,
           string type,
           string material,
           Pos& topLeft,
           Pos& bottomRight,
           bool isHollow,
           bool hasRoof)
    : id{id}, type{type}, material{material}, topLeft{topLeft},
      bottomRight{bottomRight}, isHollow{isHollow}, hasRoof{hasRoof} {}

int AABB::getID() { return this->id; }

string AABB::getMaterial() { return this->material; }

string AABB::getType() { return this->type; }

Pos AABB::getTopLeft() { return this->topLeft; }

Pos AABB::getBottomRight() { return this->bottomRight; }

vector<Block*>& AABB::getBlockList() { return (this->blockList); }

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

Pos AABB::getRandomPosAtBase(mt19937_64& gen,
                             int offsetPosX,
                             int offsetNegX,
                             int offsetPosZ,
                             int offsetNegZ) {

    int startX = (this->topLeft).getX() + offsetPosX;
    int startZ = (this->topLeft).getZ() + offsetPosZ;

    int endX = (this->bottomRight).getX() - offsetNegX;
    int endZ = (this->bottomRight).getZ() - offsetNegZ;

    uniform_int_distribution<> randXGen(startX, endX);
    uniform_int_distribution<> randZGen(startZ, endZ);

    int randX = randXGen(gen);
    int randZ = randZGen(gen);

    int base = (this->topLeft).getY();
    Pos pos(randX, base, randZ);

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

void AABB::addBlock(Block& block) { (this->blockList).push_back(&block); }

bool AABB::isOverlapping(AABB& other) {
    int xRange = (this->bottomRight.getX()) - (this->topLeft.getX());
    int yRange = (this->bottomRight.getY()) - (this->topLeft.getY());
    int zRange = (this->bottomRight.getZ()) - (this->topLeft.getZ());

    if ((abs(other.topLeft.getX() - this->topLeft.getX()) < xRange) ||
        (abs(other.topLeft.getY() - this->topLeft.getY()) < yRange) ||
        (abs(other.topLeft.getZ() - this->topLeft.getZ()) < zRange)) {

        return true;
    }
    else {
        return false;
    }
}

void AABB::generateBox(string material,
                       int offsetPosX,
                       int offsetNegX,
                       int offsetPosY,
                       int offsetNegY,
                       int offsetPosZ,
                       int offsetNegZ,
                       string type) {

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
                this->addBlock(*(new Block(material, pos, type)));
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
                           int offsetNegZ,
                           string type) {

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
        this->addBlock(*(new Block(material, pos, type)));
        n--;
    }
}

string AABB::toTSV() {

    string retval = "";
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
                    retval += to_string(x) + "\t" + to_string(y) + "\t" +
                              to_string(z) + "\t" + (this->material) + "\n";
                }
            }
        }
    }

    for (auto& block : (this->blockList)) {
        retval += (*block).toTSV() + "\n";
    }

    return retval;
}

AABB::~AABB() {}
