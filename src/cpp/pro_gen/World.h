#pragma once

#include "AABB.h"
#include "Block.h"
#include <nlohmann/json.hpp>

class World {
  private:
      std::vector<AABB> aabbList;
      std::vector<Block> blockList;

  public:
    void addAABB(AABB *);
    void addBlock(Block *);
    void toJSON(std::string);
    std::vector<AABB> getAABBList();
    std::vector<Block> getBlockList();
    World();
    ~World();
};
