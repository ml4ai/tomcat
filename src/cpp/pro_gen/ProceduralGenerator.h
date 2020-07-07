/**
 * @brief This file defines the members and methods
 * implemented as part of the Procedural Generator class
 *
 */
#pragma once
#include "Block.h"
#include "Pos.h"
#include <random>
using namespace std;

/**
 * @brief This class represents a Procedural Generator
 * which is meant to serve as a library of general methods
 * that can be used for specific map generating algorithms
 */
class ProceduralGenerator {

  public:
    /**
     * @brief Construct a new Procedural Generator:: Procedural Generator object
     */
    ProceduralGenerator();

    /**
     * @brief Destroy the Procedural Generator:: Procedural Generator object
     */
    ~ProceduralGenerator();
};