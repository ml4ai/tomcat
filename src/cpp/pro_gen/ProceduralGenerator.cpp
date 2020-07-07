/**
 * @file ProceduralGenerator.cpp
 * @brief This file implements the methods in the ProceduralGenerator class.
 */
#include "ProceduralGenerator.h"
#include <boost/random/mersenne_twister.hpp>
#include <boost/random/uniform_int_distribution.hpp>

/**
 * @brief Construct a new Procedural Generator:: Procedural Generator object
 *
 */
ProceduralGenerator::ProceduralGenerator() {}

/**
 * @brief Uses the boost random mt19937 object to randomly
 * choose a green or yellow victim and returns the
 * appropriate block (prismarine or gold)
 *
 * @param pos The position at which the victim must be placed
 * @param greenBias Value between 0 and 1 for how probable it should be for the
 * victim to be green
 * @param gen Object used in random integer distribution for finding a random
 * integer
 * @return Block The victim block
 */
Block ProceduralGenerator::getRandomVictim(Pos* pos,
                                           double greenBias,
                                           boost::random::mt19937* gen) {
    boost::random::uniform_int_distribution<> dist(1, 100);
    double greenProbability = greenBias * 100;
    int randomInt = dist(*gen);
    if (randomInt <= greenProbability) {
        Block block("prismarine", pos, "victim");
        return block;
    }
    else {
        Block block("gold", pos, "victim");
        return block;
    }
}

/**
 * @brief Destroy the Procedural Generator:: Procedural Generator object
 */
ProceduralGenerator::~ProceduralGenerator() {}
