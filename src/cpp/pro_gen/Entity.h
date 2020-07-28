#pragma once
#include "Pos.h"
#include <nlohmann/json.hpp>

class Entity {

  private:
    std::string type;
    std::vector<int> equipment;
    Pos pos;

  public:
    std::string getType();
    int getX();
    int getY();
    int getZ();
    int getHelmet();
    int getChestplate();
    int getLeggings();
    int getBoots();
    int getWeapon();
    void setHelmet(int helmet);
    void setChestplate(int chestplate);
    void setLeggings(int leggings);
    void setBoots(int boots);
    void setWeapon(int weapon);
    void setX(int x);
    void setY(int y);
    void setZ(int z);
    void setType(std::string type);
    nlohmann::json virtual toJSON();
    std::string virtual toTSV();
    Entity(std::string type,
           Pos& pos,
           int helmet = 0,
           int chestplate = 0,
           int leggings = 0,
           int boots = 0,
           int weapon = 0);
    virtual ~Entity();
};