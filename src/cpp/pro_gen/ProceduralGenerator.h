#pragma once
#include "Pos.h"
#include "Block.h"
using namespace std;

class ProceduralGenerator{

    public:
        Block getRandomVictim(Pos *, double);
        ProceduralGenerator();
        ~ProceduralGenerator();
};