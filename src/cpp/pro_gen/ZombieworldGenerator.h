#include "Group.h"
#include "Pit.h"
#include "ProceduralGenerator.h"

class ZombieWorldGenerator : public ProceduralGenerator {
  private:
    int N = 3;
    int sep = 15;
    int AABB_size = 10;
    void addGroupOfAABB(int idCtr, Pos& firstTopLeft, Pos& firstBottomRight);
    void chooseZombieworldAABB(int idCtr, Pos& topLeft, Pos& bottomRight);
    void generateAABBGrid();
    void generateBoundingWalls();
    void decorate();
    void addLevers();
    void addLights(AABB&);

  public:
    ZombieWorldGenerator(int seed = 1);
    ~ZombieWorldGenerator();
};
