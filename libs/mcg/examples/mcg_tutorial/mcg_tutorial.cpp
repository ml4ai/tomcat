#include <memory>
#include "mcg/AABB.h"
#include "mcg/Entity.h"
#include "mcg/World.h"
using namespace std;

class Room : public AABB {
  
  public:
    Room(string id, Pos& topLeft) : AABB(id) {
        // Set the base material to be a log
        this->setMaterial("log");

        // Define the object's boundaries
        // Note that we set the bottom right to be
        // 5,4 and 5 blocks away from the top left on the 
        // X, Y and Z axes respectively
        Pos bottomRight(topLeft);
        bottomRight.shift(5, 4, 5);
        this->setTopLeft(topLeft);
        this->setBottomRight(bottomRight);

        // The floor should be made of planks.
        // Generate a box of planks where the floor should be.
        // These blocks will override the base aabb material present
        // in that region before.
        // The other number arguments denote the number of blocks offset
        this->generateBox("planks", 1, 1, 0, 4, 1, 1);

        // Add windows similar to how we added the flooring
        this->generateBox("glass", 0, 5, 1, 1, 1, 1);
        this->generateBox("glass", 5, 0, 1, 1, 1, 1);
        this->generateBox("glass", 1, 1, 1, 1, 0, 5);

        // Add a roof. The roof will be made out of
        // the base material type. In this case, we get
        // a roof made of logs
        this->hasRoof = true;

        // Add a friend
        mt19937_64 gen; // A random number generator engine
        // We use the engine to get a random position on the floor
        Pos randomPos = this->getRandomPos(gen, 1, 1, 1, 2, 1, 1);
        auto zombie = make_unique<Entity>("zombie", randomPos);
        this->addEntity(move(zombie));
    }

    ~Room(){};
};

class TutorialWorld : public World {

  public:
    TutorialWorld() {
        Pos topLeft(1, 3, 1); // Top left for the first room
        auto room1 = make_unique<Room>("room_1", topLeft);

        // Create the second room in the same spot, then
        // shift the entire room by 5 block in the X axis
        auto room2 = make_unique<Room>("room_2", topLeft);
        room2->shiftX(5);

        // Add both to an AABB that we choose to call a house
        // "house" will auto resize it's own bounds anytime we
        // add an AABB to it's list of children.
        auto house = make_unique<AABB>("house");
        house->addAABB(move(room1));
        house->addAABB(move(room2));
        this->addAABB(move(house));
    };

    ~TutorialWorld(){};
};

/**
 * Create the world and write the JSON and TSV output to file.
 */
int main(int argc, char* argv[]) {
    TutorialWorld world;
    world.writeToFile(
        "semantic_map.json",
        "low_level_map.json"
    );
    return 0;
}
