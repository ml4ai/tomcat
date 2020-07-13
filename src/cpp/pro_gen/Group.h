#pragma once
#include "AABB.h"

class Group : public AABB{

    private:
        std::vector<AABB> aabbList;
        void recalculateGroupBoundaries();

    public:
        void addAABB(AABB & aabb);
        std::string toTSV();
        Group(int id);
        


};