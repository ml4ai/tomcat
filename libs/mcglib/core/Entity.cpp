#include "Entity.h"
using namespace std;
using json = nlohmann::json;

Entity::Entity(string mobType,
               Pos& pos,
               string helmet,
               string chestplate,
               string leggings,
               string boots,
               string weapon)
    : mobType{mobType}, pos{pos} {
    this->equipment.push_back(helmet);
    this->equipment.push_back(chestplate);
    this->equipment.push_back(leggings);
    this->equipment.push_back(boots);
    this->equipment.push_back(weapon);
}

int Entity::getX() { return this->pos.getX(); }

int Entity::getY() { return this->pos.getY(); }

int Entity::getZ() { return this->pos.getZ(); }

string Entity::getHelmet() { return this->equipment.at(0); }

string Entity::getChestplate() { return this->equipment.at(1); }

string Entity::getLeggings() { return this->equipment.at(2); }

string Entity::getBoots() { return this->equipment.at(3); }

string Entity::getWeapon() { return this->equipment.at(4); }

void Entity::setHelmet(string helmet) { this->equipment[0] = helmet; }

void Entity::setChestplate(string chestplate) {
    this->equipment[1] = chestplate;
}

void Entity::setLeggings(string leggings) { this->equipment[2] = leggings; }

void Entity::setBoots(string boots) { this->equipment[3] = boots; }

void Entity::setWeapon(string weapon) { this->equipment[4] = weapon; }

void Entity::setX(int x) { this->pos.setX(x); }

void Entity::setY(int y) { this->pos.setY(y); }

void Entity::setZ(int z) { this->pos.setZ(z); }

string Entity::getMobType() { return this->mobType; }

void Entity::setMobType(string mobType) { this->mobType = mobType; }

void Entity::setAllEquipment(vector<string>& equipment) {
    if (equipment.size() == 5) {
        this->setHelmet(equipment.at(0));
        this->setChestplate(equipment.at(1));
        this->setLeggings(equipment.at(2));
        this->setBoots(equipment.at(3));
        this->setWeapon(equipment.at(4));
    }
    else {
        ;
    }
}

void Entity::toSemanticMapJSON(json& json_base) {
    json entity_json;
    vector<json> coordinate_list;
    coordinate_list.push_back(this->pos.toSemanticMapJSON());

    entity_json["bounds"] = {{"coordinates", coordinate_list}};

    entity_json["chestplate"] = this->getChestplate();
    entity_json["leggings"] = this->getLeggings();
    entity_json["boots"] = this->getBoots();
    entity_json["weapon"] = this->getWeapon();

    entity_json["mob_type"] = this->getMobType();
    entity_json["helmet"] = this->getHelmet();

    json_base["entities"].push_back(entity_json);
}

void Entity::toLowLevelMapJSON(json& json_base) {
    json entity_json;

    entity_json["x"] = to_string(this->getX());
    entity_json["y"] = to_string(this->getY());
    entity_json["z"] = to_string(this->getZ());

    entity_json["chestplate"] = this->getChestplate();
    entity_json["leggings"] = this->getLeggings();
    entity_json["boots"] = this->getBoots();
    entity_json["weapon"] = this->getWeapon();

    entity_json["mob_type"] = this->getMobType();
    entity_json["helmet"] = this->getHelmet();

    json_base["entities"].push_back(entity_json);
}

Entity::~Entity() {}
