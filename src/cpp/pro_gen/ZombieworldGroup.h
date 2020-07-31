#pragma once

#include "Group.h"

class ZombieworldGroup : public Group {
  private:
    std::mt19937_64 gen;
    void createAABB(Pos& firstTopLeft, Pos& firstBottomRight);
    void addLights();
    void addLevers();
    void addEntities();
    void decorate(Pos& firstTopLeft, Pos& firstBottomRight);

  public:
    ZombieworldGroup(int id, Pos& firstTopLeft, Pos& firstBottomRight);
    ~ZombieworldGroup();
};
