#pragma once
#include "Pos.h"
#include "Block.h"
#include <boost/random/mersenne_twister.hpp>
#include <boost/random/uniform_int_distribution.hpp>
using namespace std;

class ProceduralGenerator{

    public:
        Block getRandomVictim(Pos *, double, boost::random::mt19937 *);
        ProceduralGenerator();
        ~ProceduralGenerator();
};