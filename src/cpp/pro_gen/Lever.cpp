#include "Lever.h"
using namespace std;
using json = nlohmann::json;

Lever::Lever(
    Pos& pos, string type, bool powered, string facing)
    : Block("lever", pos, type) {
    this->powered = powered;
    this->facing = facing;
}

json Lever::toJSON() {
    json block_json;
    block_json["type"] = this->getType();
    block_json["x"] = to_string(this->getX());
    block_json["y"] = to_string(this->getY());
    block_json["z"] = to_string(this->getZ());
    block_json["material"] = this->getMaterial();

    if (this->powered) {
        block_json["powered"] = "true";
    }
    else {
        block_json["powered"] = "false";
    }
    block_json["facing"] = this->facing;
    return block_json;
}

Lever::~Lever() {}