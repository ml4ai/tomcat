#include "Door.h"
using namespace std;
using json = nlohmann::json;

Door::Door(Pos& pos, bool open, bool powered, string name, string facing)
    : Block(name, pos) {
    this->open = open;
    this->powered = powered;
    this->facing = facing;
}

void Door::toSemanticMapJSON(json& json_base) {
    json block_json;

    string isPowered = "";
    string isOpen = "";

    if (this->powered) {
        isPowered = "true";
    }
    else {
        isPowered = "false";
    }

    if (this->open) {
        isOpen = "true";
    }
    else {
        isOpen = "false";
    }

    vector<json> coordinate_list;
    coordinate_list.push_back(this->pos.toSemanticMapJSON());

    block_json["bounds"] = {{"type", "block"},
                            {"coordinates", coordinate_list}};
    block_json["material"] = this->getMaterial();
    block_json["open"] = isOpen;
    block_json["powered"] = isPowered;
    block_json["facing"] = this->facing;

    json_base["locations"].push_back(block_json);
}

void Door::toLowLevelMapJSON(json& json_base) {
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
    block_json["open"] = this->open;
    block_json["powered"] = isPowered;
    block_json["facing"] = this->facing;

    json_base["blocks"].push_back(block_json);
}

Door::~Door() {}
