#include "Entity.h"
#include <iostream>
using namespace std;
using json = nlohmann::json;

Entity::Entity(string type,
               Pos& pos,
               int helmet,
               int chestplate,
               int leggings,
               int boots,
               int weapon)
    : type{type}, pos{pos} {
    this->equipment.push_back(helmet);
    this->equipment.push_back(chestplate);
    this->equipment.push_back(leggings);
    this->equipment.push_back(boots);
    this->equipment.push_back(weapon);
}

int Entity::getX() { return this->pos.getX(); }

int Entity::getY() { return this->pos.getY(); }

int Entity::getZ() { return this->pos.getZ(); }

int Entity::getHelmet() { return this->equipment.at(0); }

int Entity::getChestplate() { return this->equipment.at(1); }

int Entity::getLeggings() { return this->equipment.at(2); }

int Entity::getBoots() { return this->equipment.at(3); }

int Entity::getWeapon() { return this->equipment.at(4); }

void Entity::setHelmet(int helmet) { this->equipment[0] = helmet; }

void Entity::setChestplate(int chestplate) { this->equipment[1] = chestplate; }

void Entity::setLeggings(int leggings) { this->equipment[2] = leggings; }

void Entity::setBoots(int boots) { this->equipment[3] = boots; }

void Entity::setWeapon(int weapon) { this->equipment[4] = weapon; }

void Entity::setX(int x) { this->pos.setX(x); }

void Entity::setY(int y) { this->pos.setY(y); }

void Entity::setZ(int z) { this->pos.setZ(z); }

string Entity::getType() { return this->type; }

void Entity::setType(string type) { this->type = type; }

json Entity::toJSON() {
    json entity_json;
    entity_json["type"] = this->getType();
    entity_json["x"] = to_string(this->getX());
    entity_json["y"] = to_string(this->getY());
    entity_json["z"] = to_string(this->getZ());
    entity_json["helmet"] = to_string(this->getHelmet());
    entity_json["chestplate"] = to_string(this->getChestplate());
    entity_json["leggings"] = to_string(this->getLeggings());
    entity_json["boots"] = to_string(this->getBoots());
    entity_json["weapon"] = to_string(this->getWeapon());
    return entity_json;
}

string Entity::toTSV() {
    string retval =
        (this->pos).toTSV() + "\t" + "entity" + "\t" + (this->type) + "\t";

    for (auto& equipment : this->equipment) {
        retval += to_string(equipment) + "\t";
    }
    return retval;
}

Entity::~Entity() {}