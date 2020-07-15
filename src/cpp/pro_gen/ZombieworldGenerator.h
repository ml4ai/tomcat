#include "Group.h"
#include "Pit.h"
#include "ProceduralGenerator.h"
#include <boost/program_options.hpp>
#include <fstream>
#include <iostream>

class ZombieWorldGenerator : public ProceduralGenerator {
  private:
    int N = 3;
    int sep = 15;
    int AABB_size = 10;
    void addGroupOfAABB(int idCtr, Pos& firstTopLeft, Pos& firstBottomRight);
    void chooseZombieworldAABB(int idCtr, Pos& topLeft, Pos& bottomRight);
    void generateAABBGrid();
    void generateBlocks();
    void generateBoundingWalls();

  public:
    ZombieWorldGenerator(int seed = 1);
};
