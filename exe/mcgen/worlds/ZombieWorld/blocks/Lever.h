#pragma once

#include "mcglib/Block.h"

class Lever : public Block {
  private:
    bool powered;
    std::string facing;

  public:
    /**
     * @brief Adds the JSON representation of this object to the
     *        "locations" lists of the base json
     *
     * @return nlohmann::json The base json
     */
    void virtual toSemanticMapJSON(nlohmann::json& json_base);

    /**
     * @brief Adds the alternate JSON representation of this object to the
     *        "blocks" lists of the base json
     *
     * @return nlohmann::json The base json
     */
    void virtual toLowLevelMapJSON(nlohmann::json& json_base);

    /**
     * @brief Construct a new Lever object
     *
     * @param pos The position of the block in the Minecraft world
     * @param powered Should the lever be powered upon placement
     * @param facing Which direction the lever should face
     */
    Lever(Pos& pos, bool powered = false, std::string facing = "null");

    /**
     * @brief Destroy the Lever object
     */
    ~Lever();
};
