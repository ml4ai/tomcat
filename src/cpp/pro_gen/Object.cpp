#include "Object.h"

using namespace std;
using json = nlohmann::json;

Object::Object(string id, string type, Block& block)
    : id{id}, type{type}, block{block} {}

json Object::toJSON() {
    json object_json;

    object_json["id"] = this->id;
    object_json["type"] = this->type;
    object_json.push_back(this->block.toJSON());
    return object_json;
}

Object::~Object() {}