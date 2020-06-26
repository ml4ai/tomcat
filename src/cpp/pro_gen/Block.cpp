#include "Block.h"
#include <iostream>
#include <string>

using namespace std;

Block::Block(string blockName, string blockMaterial, Pos* blockPos)
    : name(blockName), material(blockMaterial), pos(*blockPos) {}

string Block::getName() { return this->name; }

string Block::getMaterial() { return this->material; }

int Block::getX() { return this->pos.getX(); }

int Block::getY() { return this->pos.getY(); }

int Block::getZ() { return this->pos.getZ(); }

string Block::toString() {
    string retval = "Name: " + this->name + "\n" +
                    "Material: " + this->material + "\n" +
                    "Pos: " + (this->pos).toString();
    return retval;
}

Block::~Block() {}
