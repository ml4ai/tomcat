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
    /**
     * @brief Get the World object this generator keeprs track of.
     *
     * @return World& Reference to the relevant world
     */
    World& getWorld();

    /**
     * @brief Get the random object used by this generator
     *
     * @return std::mt19937_64& The random object
     */
    std::mt19937_64& getRandom();

    /**
     * @brief Set the seed the random object used by this class is
     * initialized with.
     *
     * @param seed The seed to use.
     */
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