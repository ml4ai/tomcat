#pragma once
#include "AABB.h"
#include "Block.h"
#include <nlohmann/json.hpp>
using namespace std;

class World {
  private:
    vector<AABB> aabbList;
    vector<Block> blockList;

  public:
    void addAABB(AABB);
    void addBlock(Block);
    void toJSON(string);
    vector<AABB> getAABBList();
    vector<Block> getBlockList();
    World();
    ~World();
};