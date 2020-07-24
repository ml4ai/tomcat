#pragma once
#include "Pos.h"

class Entity {

  private:
    std::string type;
    Pos pos;

  public:
    std::string getType();
    Pos& getPos();
    void setType(std::string type);
    void setPos(Pos& pos);
    Entity(std::string type, Pos& pos);
    virtual ~Entity();
};