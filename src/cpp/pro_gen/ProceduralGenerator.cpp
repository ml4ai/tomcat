/**
 * @file ProceduralGenerator.cpp
 * @brief This file implements the methods in the ProceduralGenerator class.
 */
#include "ProceduralGenerator.h"
using namespace std;

ProceduralGenerator::ProceduralGenerator() {}

World& ProceduralGenerator::getWorld() { return this->world; }

mt19937_64& ProceduralGenerator::getRandom() { return this->gen; }

void ProceduralGenerator::setRandom(int seed) {
    mt19937_64 newGen(seed);
    this->gen = newGen;
}

ProceduralGenerator::~ProceduralGenerator() {}
