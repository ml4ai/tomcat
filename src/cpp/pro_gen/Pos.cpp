/**
 * @file Pos.cpp
 * @brief This file implements the methods in the Pos class.
 */

#include "Pos.h"
#include <string>
using namespace std;

Pos::Pos() {}

Pos::Pos(int x, int y, int z) : x{x}, y{y}, z{z} {}

Pos::Pos(const Pos& other) : x{other.x}, y{other.y}, z{other.z} {}

int Pos::getX() { return this->x; }

int Pos::getY() { return this->y; }

int Pos::getZ() { return this->z; }

void Pos::setX(int x) { this->x = x; }

void Pos::setY(int y) { this->y = y; }

void Pos::setZ(int z) { this->z = z; }

void Pos::shiftX(int shift) { this->x += shift; }

void Pos::shiftY(int shift) { this->y += shift; }

void Pos::shiftZ(int shift) { this->z += shift; }

string Pos::toTSV() {
    string retval = to_string(this->x) + "\t" + to_string(this->y) + "\t" +
                    to_string(this->z);
    return retval;
}

Pos::~Pos() {}
