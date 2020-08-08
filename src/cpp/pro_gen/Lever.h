#pragma once
#include "Block.h"

class Lever : public Block{
    private:
        bool powered;
        std::string facing;

    public:
        nlohmann::json virtual toJSON();
        Lever(Pos& pos, bool powered = false, std::string facing = "null");
        ~Lever();

};