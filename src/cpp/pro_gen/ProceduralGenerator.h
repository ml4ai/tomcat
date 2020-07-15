/**
 * @brief This file defines the members and methods
 * implemented as part of the Procedural Generator class
 *
 */
#pragma once
#include "World.h"

/**
 * @brief This class represents a Procedural Generator
 * which is meant to serve as a library of general methods
 * that can be used for specific map generating algorithms
 */
class ProceduralGenerator {

  private:
    std::mt19937_64 gen;
    World world;

  public:

    World & getWorld();
    std::mt19937_64 & getRandom();
    void setRandom(int seed);

    /**
     * @brief Construct a new Procedural Generator object
     */
    ProceduralGenerator();

    /**
     * @brief Destroy the Procedural Generator object
     */
    virtual ~ProceduralGenerator();
};