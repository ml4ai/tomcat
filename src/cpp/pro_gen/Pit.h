#pragma once
#include "AABB.h"
#include <string.h>

class Pit : public AABB {

  public:
    void generateLava();
    Pit(int, std::string, Pos*, Pos*);

    ~Pit();
};