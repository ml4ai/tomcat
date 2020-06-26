#pragma once

#include "AABB.h"
#include "Block.h"
#include <string>
#include <nlohmann/json.hpp>

class World {
  private:
      std::vector<AABB> aabbList;
      std::vector<Block> blockList;

  public:
    void addAABB(AABB *);
    void addBlock(Block *);
    std::string toJSON();
    std::vector<AABB> * getAABBList();
    std::vector<Block> * getBlockList();
    std::string toString();
    World();
    ~World();
};
