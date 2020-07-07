/**
 * @file Block.cpp
 * @brief This file implements the methods in the Block class.
 */
#include "Block.h"

using namespace std;

Block::Block(string material, Pos* pos, string type)
    : type(type), material(material), pos(*pos) {}

string Block::getType() { return this->type; }

string Block::getMaterial() { return this->material; }

int Block::getX() { return this->pos.getX(); }

int Block::getY() { return this->pos.getY(); }

int Block::getZ() { return this->pos.getZ(); }

string Block::toTSV() {
    string retval = (this->pos).toTSV() + "\t" + (this->material);
    return retval;
}

Block::~Block() {}
