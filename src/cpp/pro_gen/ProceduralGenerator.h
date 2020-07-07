/**
 * @brief This file defines the members and methods
 * implemented as part of the Procedural Generator class
 *
 */
#pragma once
#include "Block.h"
#include "Pos.h"
#include <boost/random/mersenne_twister.hpp>
#include <boost/random/uniform_int_distribution.hpp>
using namespace std;

/**
 * @brief This class represents a Procedural Generator
 * which is meant to serve as a library of general methods
 * that can be used for specific map generating algorithms
 */
class ProceduralGenerator {

  public:
    /**
     * @brief Uses the boost random mt19937 object to randomly
     * choose a green or yellow victim and returns the
     * appropriate block (prismarine or gold)
     *
     * @param pos The position at which the victim must be placed
     * @param greenBias Value between 0 and 1 for how probable it should be for
     * the victim to be green
     * @param gen Object used in random integer distribution for finding a
     * random integer
     * @return Block The victim block
     */
    Block
    getRandomVictim(Pos* pos, double greenBias, boost::random::mt19937* gen);

    /**
     * @brief Construct a new Procedural Generator:: Procedural Generator object
     */
    ProceduralGenerator();

    /**
     * @brief Destroy the Procedural Generator:: Procedural Generator object
     */
    ~ProceduralGenerator();
};