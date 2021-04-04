/**
 * @file Pos.cpp
 * @brief This file implements the methods in the Pos class.
 */

#include "Pos.h"
using namespace std;
using json = nlohmann::json;

Pos::Pos() {}

Pos::Pos(int x, int y, int z) : x{x}, y{y}, z{z} {}

int Pos::getX() { return this->x; }

int Pos::getY() { return this->y; }

int Pos::getZ() { return this->z; }

void Pos::setX(int x) { this->x = x; }

void Pos::setY(int y) { this->y = y; }

void Pos::setZ(int z) { this->z = z; }

void Pos::shiftX(int shift) { this->x += shift; }

void Pos::shiftY(int shift) { this->y += shift; }

void Pos::shiftZ(int shift) { this->z += shift; }

json Pos::toSemanticMapJSON() {
    json pos_json;
    pos_json["x"] = this->getX();
    pos_json["y"] = this->getY();
    pos_json["z"] = this->getZ();
    return pos_json;
}

Pos::~Pos() {}
