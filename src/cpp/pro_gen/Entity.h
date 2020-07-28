#pragma once
#include "Pos.h"
#include <nlohmann/json.hpp>

class Entity {

  private:
    std::string type;
    Pos pos;

  public:
    std::string getType();
    int getX();
    int getY();
    int getZ();
    void setX(int x);
    void setY(int y);
    void setZ(int z);
    void setType(std::string type);
    nlohmann::json virtual toJSON();
    std::string virtual toTSV();
    Entity(std::string type, Pos& pos);
    virtual ~Entity();
};