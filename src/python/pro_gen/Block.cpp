#include "Block.h"
#include <iostream>

using namespace std;

Block::Block(string name, string material, Pos pos1){
    this -> name = name;
    this -> material = material;
    this -> pos = pos1;

}

string Block::getName(){
    return this -> name;
}

string Block::getMaterial(){
    return this -> material;
}

int Block::getBlockX(){
    return this -> pos.getX();
}

int Block::getBlockY(){
    return this -> pos.getY();
}

int Block::getBlockZ(){
    return this -> pos.getZ();
}

Block::~Block(){}

int main(){
    Pos pos(10,20,30);
    Block block("door", "oak_door", pos);
    cout << block.getMaterial() << endl;
    return 0;
}
