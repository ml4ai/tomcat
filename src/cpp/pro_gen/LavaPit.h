#pragma once
#include "AABB.h"

class LavaPit : public AABB {

  public:
    void generateLava();
    LavaPit(int, std::string, Pos*, Pos*, bool, bool);
    ~LavaPit();
};