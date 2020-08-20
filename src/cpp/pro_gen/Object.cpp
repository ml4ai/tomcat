#include "Object.h"
#include <iostream>

using namespace std;
using json = nlohmann::json;

Object::Object(string id, string type, Block& block)
    : id{id}, type{type}, block{block} {}

void Object::toJSON(json& json_base) {
    json object_json;

    object_json["id"] = this->id;
    object_json["type"] = this->type;

    vector<json> coordinate_list;
    coordinate_list.push_back(this->block.getPos().toJSON());

    object_json["bounds"] = {{"type", "block"},
                             {"coordinates", coordinate_list},
                             {"material", this->block.getMaterial()}};

    json_base["objects"].push_back(object_json);
}

Object::~Object() {}