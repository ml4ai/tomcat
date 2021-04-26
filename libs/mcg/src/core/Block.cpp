/**
 * @file Block.cpp
 * @brief This file implements the methods in the Block class.
 */
#include "mcg/Block.h"

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

void Block::shiftX(int shift) { this->pos.shiftX(shift); }

void Block::shiftY(int shift) { this->pos.shiftY(shift); }

void Block::shiftZ(int shift) { this->pos.shiftZ(shift); }

void Block::shift(int shiftX, int shiftY, int shiftZ) {
    this->shiftX(shiftX);
    this->shiftY(shiftY);
    this->shiftZ(shiftZ);
}

void Block::toSemanticMapJSON(json& json_base) {

    json block_json;
    vector<json> coordinate_list;
    coordinate_list.push_back(this->pos.toSemanticMapJSON());

    block_json["bounds"] = {{"type", "block"},
                            {"coordinates", coordinate_list}};
    block_json["material"] = this->getMaterial();

    json_base["locations"].push_back(block_json);
}

void Block::toLowLevelMapJSON(json& json_base) {
    json block_json;

    block_json["material"] = this->getMaterial();
    block_json["x"] = to_string(this->getX());
    block_json["y"] = to_string(this->getY());
    block_json["z"] = to_string(this->getZ());

    json_base["blocks"].push_back(block_json);
}

Block::~Block() {}
