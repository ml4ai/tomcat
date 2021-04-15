#include "mcglib/Object.h"

using namespace std;
using json = nlohmann::json;

Object::Object(string id, string type, unique_ptr<Block> block)
    : id{id}, type{type} {
    this->block = move(block);
}

void Object::shiftX(int shift) { this->block->shiftX(shift); }

void Object::shiftY(int shift) { this->block->shiftY(shift); }

void Object::shiftZ(int shift) { this->block->shiftZ(shift); }

void Object::shift(int shiftX, int shiftY, int shiftZ) {
    this->shiftX(shiftX);
    this->shiftY(shiftY);
    this->shiftZ(shiftZ);
}

void Object::toSemanticMapJSON(json& json_base) {
    json object_json;

    object_json["id"] = this->id;
    object_json["type"] = this->type;

    vector<json> coordinate_list;
    coordinate_list.push_back((*this->block).getPos().toSemanticMapJSON());

    object_json["bounds"] = {{"type", "block"},
                             {"coordinates", coordinate_list},
                             {"material", (*this->block).getMaterial()}};

    json_base["objects"].push_back(object_json);
}

void Object::toLowLevelMapJSON(json& json_base) {
    (*this->block).toLowLevelMapJSON(json_base);
}

Object::~Object() {}
