#include "Entity.h"
using namespace std;
using json = nlohmann::json;

Entity::Entity(string type, Pos& pos) : type{type}, pos{pos} {}

int Entity::getX(){
    return this->pos.getX();
}

int Entity::getY(){
    return this->pos.getY();
}

int Entity::getZ(){
    return this->pos.getZ();
}

void Entity::setX(int x) { this->pos.setX(x); }

void Entity::setY(int y) { this->pos.setY(y); }

void Entity::setZ(int z) { this->pos.setZ(z); }

string Entity::getType() { return this->type; }

void Entity::setType(string type) { this->type = type; }

json Entity::toJSON(){
    json entity_json;
    entity_json["type"] = this->getType();
    entity_json["x"] = to_string(this->getX());
    entity_json["y"] = to_string(this->getY());
    entity_json["z"] = to_string(this->getZ());
    return entity_json;
}

string Entity::toTSV() {
    string retval = (this->pos).toTSV() + "\t" + (this->type);
    return retval;
}

Entity::~Entity() {}