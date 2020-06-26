#pragma once
#include "Pos.h"
#include <string>

class Block {

  private:
    std::string name;
    std::string material;
    Pos pos;

  public:
    std::string getName();
    std::string getMaterial();
    int getX();
    int getY();
    int getZ();
    Block(std::string, std::string, Pos *);
    ~Block();
};
