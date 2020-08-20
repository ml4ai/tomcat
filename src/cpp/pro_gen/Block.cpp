/**
 * @file Block.cpp
 * @brief This file implements the methods in the Block class.
 */
#include "Block.h"

using namespace std;
using json = nlohmann::json;

Block::Block(string material, Pos& pos) : material{material}, pos{pos} {}

string Block::getMaterial() { return this->material; }

int Block::getX() { return this->pos.getX(); }

int Block::getY() { return this->pos.getY(); }

int Block::getZ() { return this->pos.getZ(); }

Pos& Block::getPos() { return this->pos; }

void Block::setX(int x) { this->pos.setX(x); }

void Block::setY(int y) { this->pos.setY(y); }

void Block::setZ(int z) { this->pos.setZ(z); }

void Block::toJSON(json& json_base) {

    json block_json;
    vector<json> coordinate_list;
    coordinate_list.push_back(this->pos.toJSON());

    block_json["bounds"] = {{"type", "block"},
                            {"coordinates", coordinate_list},
                            {"material", this->getMaterial()}};
    json_base["locations"].push_back(block_json);
}

string Block::toTSV() {
    string retval =
        (this->pos).toTSV() + "\t" + "block" + "\t" + (this->material);
    return retval;
}

Block::~Block() {}
