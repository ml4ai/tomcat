#include "TutorialWorld.h"
#include <memory>

using namespace std;


TutorialWorld::TutorialWorld(){
    
    Pos topLeft(1,3,1);
    auto room1 = make_unique<House>("room_1", topLeft);
    room1->init();

    auto room2 = make_unique<House>("room_2", topLeft);
    room2->shiftX(5);
    room2->init();

    auto enclosing_aabb = make_unique<AABB>("enclosing_aabb");
    enclosing_aabb->addAABB(move(room1));
    enclosing_aabb->addAABB(move(room2));
    
    this->addAABB(move(enclosing_aabb));

}

TutorialWorld::~TutorialWorld(){}
