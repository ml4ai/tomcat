#pragma once

#include "World.h"
#include "ZombieworldGroup.h"
#include "ZombieworldPit.h"

class ZombieWorld : public World {
  private:
    int N = 3;
    int sep = 15;
    int AABB_size = 10;

    /**
     * @brief A method to choose the AABB to add based on the idCtr. It
     *        alternates between a Pit and Group type of AABB. Pit objects are
     * further randomly chosen from Lava and Water Pits or Air. Air AABBs are
     * for when nothing is to be added at a certain position.
     *
     * @param idCtr The id to set the Group to. In this case also the count
     *        reached as of this Group.
     * @param topLeft The top left coordinates for the AABB to be added.
     * @param bottomRight The bottom right coordinates for the AABB to be added.
     */
    void chooseZombieworldAABB(int idCtr, Pos& topLeft, Pos& bottomRight);

    /**
     * @brief Calls internal methods to create and place different AABBs at the
     *        right positions.
     */
    void generateAABBGrid();

    /**
     * @brief Generates the AABBs that represent the Bounding wall structures
     *        for this mission map.
     */
    void generateBoundingWalls();

  public:
    /**
     * @brief Construct a new Zombie World Generator object with the internal
     *        random seed initialized to the value passed.
     *
     * @param seed The seed to use for randomness.
     */
    ZombieWorld(int seed = 1);

    /**
     * @brief Destroy the Zombie World Generator object.
     */
    ~ZombieWorld();
};
