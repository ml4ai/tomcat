#pragma once

#include "../core/World.h"
#include "aabbs/DungeonRoom1.h"
#include "aabbs/DungeonRoom2.h"

class DungeonWorld : public World {
  private:
    int numberOfRoomTypes = 2;

  public:
    /**
     * @brief Construct a new Dungeon World object
     *
     * @param seed The seed for the random object. Defaults to 1. Different
     * values will give different dungeons.
     * @param N The maximum number of rooms that can be placed along the X and Z
     * axes. This is esentially the size of the dungeon
     */
    DungeonWorld(int seed, int N);

    /**
     * @brief Destroy the Dungeon World object
     */
    ~DungeonWorld();
};
