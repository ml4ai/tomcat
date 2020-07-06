/**
 * @brief This file defines the members and methods
 * implemented as part of the Block class
 *
 */
#pragma once
#include "Pos.h"
#include <string>

/**
 * @brief This class represents a Minecraft block
 *
 */
class Block {

  private:
    std::string type;
    std::string material;
    Pos pos;

  public:
    std::string getType();
    std::string getMaterial();
    int getX();
    int getY();
    int getZ();
    std::string toTSV();
    Block(std::string, std::string, Pos*);
    ~Block();
};
