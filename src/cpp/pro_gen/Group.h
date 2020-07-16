#pragma once
#include "AABB.h"

class Group : public AABB {

  private:
    std::vector<AABB*> aabbList;
    void recalculateGroupBoundaries();

  public:
    void addAABB(AABB& aabb);
    void generateAllDoorsInAABB();
    std::vector<AABB*>& getAABBList();
    AABB* getAABB(int id);
    std::string toTSV();
    Group(int id);
};