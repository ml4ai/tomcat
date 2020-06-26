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
 *
 */
class ProceduralGenerator {

  public:
    Block getRandomVictim(Pos*, double, boost::random::mt19937*);
    ProceduralGenerator();
    ~ProceduralGenerator();
};