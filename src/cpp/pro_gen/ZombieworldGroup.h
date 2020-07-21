#pragma once

#include "Group.h"

class ZombieworldGroup : public Group {
  private:
    void createAABB(Pos& firstTopLeft, Pos& firstBottomRight);
    void addLights();
    void addLevers();
    void decorate(Pos& firstTopLeft, Pos& firstBottomRight);

  public:
    ZombieworldGroup(int id, Pos& firstTopLeft, Pos& firstBottomRight);
    ~ZombieworldGroup();
};
