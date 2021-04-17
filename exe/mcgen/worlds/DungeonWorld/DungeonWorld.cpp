#include "DungeonWorld.h"
using namespace std;

DungeonWorld::DungeonWorld(int seed, int N) {
    this->setRandom(seed);

    // Create a 2D array of size NxN and fill it with random
    // values in the inclusive range [0,2]. 0 will mean no room,
    // 1 will mean place room type 1 and 2 will mean place room type
    // 2
    mt19937_64& gen = this->getRandom();
    uniform_int_distribution<> dist(0, this->numberOfRoomTypes);
    int reference_arr[N][N];
    for (int i = 0; i < N; i++) {
        for (int j = 0; j < N; j++) {
            int randInt = dist(gen);
            reference_arr[i][j] = randInt;
        }
    }

    // Create an arbitrary enclosing boundary
    Pos temp(0, 0, 0); // Value doesn't matter since it it set to auto-resize
    // Boundary is made of material "blank", so nothing will be placed
    auto enclosing_boundary = make_unique<AABB>("enclosing_boundary",
                                                "blank_canvas",
                                                "blank",
                                                temp,
                                                temp,
                                                true,
                                                false,
                                                true);
    this->addAABB(move(enclosing_boundary));

    // Get the reference to the enclosing boundary back so we can add our
    // dungeon rooms as sub-aabs to this
    auto& enclosing_boundary_aabb = this->getAABBList().front();

    int ctr = 1;         // Room number
    Pos curPos(3, 3, 3); // Start placing buildings from here

    // For every position in our reference array, place buildings according to
    // the number in that index. Then nextPos is a set amount away from the
    // current room. At this next position we repeat the process and place the
    // next building based on the element at the next index of our reference
    // array
    for (int i = 0; i < N; i++) {
        for (int j = 0; j < N; j++) {

            // Choosing a room to place
            if (reference_arr[i][j] == 1) {
                auto room = make_unique<DungeonRoom1>(
                    "dungeon_room_" + to_string(ctr), curPos);
                enclosing_boundary_aabb->addAABB(move(room));
            }
            else if (reference_arr[i][j] == 2) {
                auto room = make_unique<DungeonRoom2>(
                    "dungeon_room_" + to_string(ctr), curPos);
                enclosing_boundary_aabb->addAABB(move(room));
            }
            else {
                ; // If it isn't 1 or 2, don't place anything
            }

            // If the position immediately to the left isn't 0,
            // that is, there is a building to the left, then add a corridoor
            // from the left side of the current building to that one
            if (reference_arr[i][j] != 0 && (j >= 1 && j <= N - 2) &&
                reference_arr[i][j - 1] != 0) {

                // Where the corridor starts and ends
                Pos coridoorStart(curPos);
                Pos corridorEnd(coridoorStart);
                corridorEnd.shiftZ(7);
                corridorEnd.shiftY(4);
                coridoorStart.shiftX(-7);
                coridoorStart.shiftZ(3);

                // Make the corridor
                auto corridor = make_unique<AABB>(
                    "dungeon_room_" + to_string(ctr) + "_corridor",
                    "corridor",
                    "stonebrick",
                    coridoorStart,
                    corridorEnd,
                    true,
                    false,
                    false);

                // Ends of the corridor are changed to air
                // so they are open
                corridor->generateBox("air", 0, 7, 1);
                corridor->generateBox("air", 7, 0, 1);

                // Add this to the overall boundary
                enclosing_boundary_aabb->addAABB(move(corridor));
            }

            // Go to the next position. We do this even if we placed nothing
            // otherwise we'd end up with a grid
            Pos nextPos(curPos);
            nextPos.shiftX(11 + 7);
            curPos = nextPos;
            ctr++;
        }

        // Finished the current row, so we move onto the next one
        Pos nextPos(3, 3, 3);
        nextPos.setZ((i + 1) * 11 + 3);
        curPos = nextPos;
    }
}

DungeonWorld::~DungeonWorld() {}
