#include <memory>

#include "mcglib/AABB.h"
#include "mcglib/Entity.h"
#include "mcglib/World.h"

using namespace std;

class Room : public AABB {

  private:
    std::mt19937_64 gen;

  public:
    Room(string id, Pos& topLeft) : AABB(id) {
        // Set the base material to be a log
        this->setMaterial("log");

        // Define the object's boundaries
        Pos bottomRight(topLeft);
        bottomRight.shift(5, 4, 5);
        this->setTopLeft(topLeft);
        this->setBottomRight(bottomRight);

        this->shiftX(5);
        // The floor should be made of planks
        this->generateBox("planks", 1, 1, 0, 4, 1, 1);

        // Add windows
        this->generateBox("glass", 0, 5, 1, 1, 1, 1);
        this->generateBox("glass", 5, 0, 1, 1, 1, 1);
        this->generateBox("glass", 1, 1, 1, 1, 0, 5);

        // Add a roof
        this->hasRoof = true;

        // Add a friend
        Pos randomPos = this->getRandomPos(this->gen, 1, 1, 1, 2, 1, 1);
        auto villager = make_unique<Entity>("villager", randomPos);
        this->addEntity(move(villager));
    }

    ~Room(){};
};

class TutorialWorld : public World {

  public:
    TutorialWorld() {
        Pos topLeft(1, 3, 1);
        auto room1 = make_unique<Room>("room_1", topLeft);
        auto room2 = make_unique<Room>("room_2", topLeft);

        auto house = make_unique<AABB>("house");
        house->addAABB(move(room1));
        house->addAABB(move(room2));
        this->addAABB(move(house));
    };

    ~TutorialWorld(){};
};

/**
 * @brief Create the world and write the JSON and TSV output to file.
 */
int main(int argc, char* argv[]) {
    TutorialWorld world;
    world.writeToFile("semantic_map.json", "low_level_map.json");
    return 0;
}
