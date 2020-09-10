#pragma once
#include "Block.h"

class Object {

  private:
    std::string id;
    std::string type;
    Block block;

  public:
    void virtual toJSON(nlohmann::json& json_base);
    void virtual toAltJSON(nlohmann::json& json_base);
    Object(std::string id, std::string type, Block& block);
    ~Object();
};