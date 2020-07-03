#pragma once
#include "AABB.h"
#include <string.h>

class Pit : public AABB {

  public:
    void generateLava();
    Pit(int, std::string, Pos*, Pos*);
    void generateFluidSquareAtBase(std::string,
                  int offsetPosX = 1,
                  int offsetNegX = 1,
                  int offsetPosZ = 1,
                  int offsetNegZ = 1);
    ~Pit();
};