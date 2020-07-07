/**
 * @file Block.cpp
 * @brief This file implements the methods in the Block class.
 */
#include "Block.h"
#include <iostream>
#include <string>

using namespace std;

Block::Block(string blockMaterial, Pos* blockPos, string blockType)
    : type(blockType), material(blockMaterial), pos(*blockPos) {}

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
