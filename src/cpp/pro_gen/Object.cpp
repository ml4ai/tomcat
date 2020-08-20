#include "Object.h"

using namespace std;
using json = nlohmann::json;

Object::Object(string id, string type, Block& block)
    : id{id}, type{type}, block{block} {}

void Object::toJSON(json& json_base) {
    json object_json;

    object_json["id"] = this->id;
    object_json["type"] = this->type;
    this->block.toJSON(object_json);
    json_base["objects"].push_back(object_json);
}

Object::~Object() {}