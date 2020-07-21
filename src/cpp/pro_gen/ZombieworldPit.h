#pragma once
#include "AABB.h"

/**
 * @brief This class represents a special AABB of type = "pit". It is not hollow
 * and is treated as having a roof.
 */
class ZombieworldPit : public AABB {

  private:
    std::mt19937_64 gen;
    std::string fluid;
    void decorateWater();
    void decorateLava();

  public:
    /**
     * @brief Construct a new Pit object
     *
     * @param id The id to assign this Pit AABB
     * @param material The base material the Pit is made of
     * @param topLeft The coordinates of the top left of the AABB from the
     * top view of the X-Z plane. Y coordinate should be lowest here.
     * @param bottomRight The coordinates of the bottom right of the AABB
     * from the top view of the X-Z plane. Y coordinate should be maximum here.
     */
    ZombieworldPit(int id, Pos& topLeft, std::string fluid);

    /**
     * @brief Destroy the Pit object
     */
    ~ZombieworldPit();
};