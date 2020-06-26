/**
 * @brief This file defines the members and methods
 * implemented as part of the World class
 *
 */
#pragma once

#include "AABB.h"
#include "Block.h"
#include <nlohmann/json.hpp>
#include <string>

/**
 * @brief This class represents a Minecraft world as a
 * list of axis aligned bounding boxes and blocks
 *
 */
class World {
  private:
    std::vector<AABB> aabbList;
    std::vector<Block> blockList;

  public:
    void addAABB(AABB*);
    void addBlock(Block*);
    std::string toJSON();
    std::vector<AABB>* getAABBList();
    std::vector<Block>* getBlockList();
    std::string toString();
    World();
    ~World();
};
