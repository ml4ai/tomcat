/**
 * @file Block.cpp
 * @brief This file implements the methods in the Block class.
 */
#include "Block.h"

using namespace std;
using json = nlohmann::json;

Block::Block(string material, Pos& pos, string type)
    : type(type), material(material), pos(pos) {}

string Block::getType() { return this->type; }

string Block::getMaterial() { return this->material; }

int Block::getX() { return this->pos.getX(); }

int Block::getY() { return this->pos.getY(); }

int Block::getZ() { return this->pos.getZ(); }

json Block::toJSON() {
    json block_json;
    block_json["type"] = this->getType();
    block_json["x"] = to_string(this->getX());
    block_json["y"] = to_string(this->getY());
    block_json["z"] = to_string(this->getZ());
    block_json["material"] = this->getMaterial();
    return block_json;
}

string Block::toTSV() {
    string retval = (this->pos).toTSV() + "\t" + (this->material);
    return retval;
}

Block::~Block() {}
