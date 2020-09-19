#include "Lever.h"
using namespace std;
using json = nlohmann::json;

Lever::Lever(Pos& pos, bool powered, string facing) : Block("lever", pos) {
    this->powered = powered;
    this->facing = facing;
}

void Lever::toJSON(json& json_base) {
    json block_json;

    string isPowered = "";
    if (this->powered) {
        isPowered = "true";
    }
    else {
        isPowered = "false";
    }

    vector<json> coordinate_list;
    coordinate_list.push_back(this->pos.toJSON());

    block_json["bounds"] = {{"type", "block"},
                            {"coordinates", coordinate_list}};
    block_json["material"] = this->getMaterial();
    block_json["powered"] = isPowered;
    block_json["facing"] = this->facing;

    json_base["locations"].push_back(block_json);
}

void Lever::toAltJSON(json& json_base) {
    json block_json;

    string isPowered = "";
    if (this->powered) {
        isPowered = "true";
    }
    else {
        isPowered = "false";
    }

    block_json["material"] = this->getMaterial();
    block_json["x"] = to_string(this->getX());
    block_json["y"] = to_string(this->getY());
    block_json["z"] = to_string(this->getZ());
    block_json["powered"] = isPowered;
    block_json["facing"] = this->facing;

    json_base["blocks"].push_back(block_json);
}

Lever::~Lever() {}