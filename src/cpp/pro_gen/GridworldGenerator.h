#pragma once
#include "ProceduralGenerator.h"

class GridworldGenerator : public ProceduralGenerator {

  private:
    int N;
    int sep;
    int AABB_size;
    void addRandomVictim(AABB& aabb, Pos& pos, double greenBias);
    void generateAABBGrid();
    void generateVictimInAABB(AABB& aabb);
    void generateBlocks();
    void generateGridWorld();

  public:
    GridworldGenerator(int N, int separation, int AABB_size, int seed = 1);
    ~GridworldGenerator();
};
