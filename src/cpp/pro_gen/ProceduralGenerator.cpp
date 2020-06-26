#include "ProceduralGenerator.h"
#include <boost/random/mersenne_twister.hpp>
#include <boost/random/uniform_int_distribution.hpp>

ProceduralGenerator::ProceduralGenerator(){}

Block ProceduralGenerator::getRandomVictim(Pos *pos, double greenBias){
    boost::random::mt19937 gen;
    boost::random::uniform_int_distribution<> dist(1, 100);
    double greenProbability = greenBias * 100;
    
    int randomInt = dist(gen);
    if (randomInt <= greenProbability){
        Block block("victim", "prismarine", pos);
        return block;
    }else{
        Block block("victim", "gold", pos);
        return block;
    }

}

ProceduralGenerator::~ProceduralGenerator(){}
