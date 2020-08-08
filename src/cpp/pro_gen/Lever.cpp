#include "Lever.h"
using namespace std;
using json = nlohmann::json;

Lever::Lever(Pos& pos, bool powered, string facing) : Block("lever", pos) {
    this->powered = powered;
    this->facing = facing;
}

json Lever::toJSON() {
    string isPowered = "";
    if (this->powered) {
        isPowered = "true";
    }
    else {
        isPowered = "false";
    }

    json block_json;
    vector<json> coordinate_list;
    coordinate_list.push_back(this->pos.toJSON());
    string powered =

        block_json["bounds"] = {{"type", "block"},
                                {"coordinates", coordinate_list},
                                {"material", this->getMaterial()},
                                {"powered", isPowered},
                                {"facing", this->facing}};
    return block_json;

    block_json["facing"] = this->facing;
    return block_json;
}

Lever::~Lever() {}