#include "Entity.h"
using namespace std;

Entity::Entity(string type, Pos& pos) : type{type}, pos{pos} {}

Pos& Entity::getPos() { return this->pos; }

string Entity::getType() { return this->type; }

void Entity::setPos(Pos& pos) { this->pos = pos; }

void Entity::setType(string type) { this->type = type; }

string Entity::toTSV() {
    string retval = (this->pos).toTSV() + "\t" + (this->type);
    return retval;
}

Entity::~Entity() {}