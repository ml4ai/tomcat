#pragma once

#include "World.h"
#include <memory>

class GridWorld : public World {

  private:
    int N;
    int sep;
    int AABB_size;
    /**
     * @brief Adds a random green or yellow victim to the given AABB.
     *
     * @param aabb The AABB where the victim must be added.
     * @param pos The position at which the randomly chosen victim must be
     * added.
     * @param greenBias Likelihood for the random victim to be green. Values
     * should be between 0 and 1.
     */
    void addRandomVictim(AABB& aabb, Pos& pos, double greenBias);

    /**
     * @brief Create and place AABBs at the relevant positions
     */
    void generateAABBGrid();

    /**
     * @brief Chooses a random position to add a victim in.
     * @param aabb The AABB where the random victim is to be added.
     */
    void generateVictimInAABB(AABB& aabb);

    /**
     * @brief Generate the individual blocks needed such as doors and victims.
     */
    void generateBlocks();

  public:
    /**
     * @brief Construct a new Gridworld Generator object
     *
     * @param N The number of AABB on each axis
     * @param separation The separation between AABB in the cardinal directions.
     * @param AABB_size The size of each cubic AABB
     * @param seed The random seed used internally.
     */
    GridWorld(int N, int separation, int AABB_size, int seed = 1);

    /**
     * @brief Destroy the Gridworld Generator object
     */
    ~GridWorld();
};
