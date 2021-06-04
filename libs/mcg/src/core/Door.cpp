#include "mcg/Door.h"
using namespace std;
using json = nlohmann::json;

Door::Door(Pos& pos, bool open, bool powered, string name, string facing, string hinge)
    : Block(name, pos) {
    this->open = open;
    this->powered = powered;
    this->facing = facing;
    this->hinge = hinge;
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
    json block_json_lower;
    json block_json_upper;

    string isPowered = "";
    if (this->powered) {
        isPowered = "true";
    }
    else {
        isPowered = "false";
    }

    block_json_lower["material"] = this->getMaterial();
    block_json_lower["x"] = to_string(this->getX());
    block_json_lower["y"] = to_string(this->getY());
    block_json_lower["z"] = to_string(this->getZ());
    block_json_lower["open"] = this->open;
    block_json_lower["powered"] = isPowered;
    block_json_lower["facing"] = this->facing;
    block_json_upper["half"] = "lower";
    block_json_upper["hinge"] = this->hinge;

    block_json_upper["material"] = this->getMaterial();
    block_json_upper["x"] = to_string(this->getX());
    block_json_upper["y"] = to_string(this->getY());
    block_json_upper["z"] = to_string(this->getZ());
    block_json_upper["open"] = this->open;
    block_json_upper["powered"] = isPowered;
    block_json_upper["facing"] = this->facing;
    block_json_upper["half"] = "upper";
    block_json_upper["hinge"] = this->hinge;



    json_base["blocks"].push_back(block_json_lower);
    json_base["blocks"].push_back(block_json_upper);
}

Door::~Door() {}
