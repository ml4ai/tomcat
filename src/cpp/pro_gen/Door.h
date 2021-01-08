#pragma once
#include "Block.h"

class Door : public Block {
  private:
    bool powered;
    bool open;
    std::string facing;

  public:
    /**
     * @brief Adds the JSON representation of this object to the
     *        "locations" lists of the base json.
     *
     * @return nlohmann::json The base json.
     */
    void virtual toSemanticMapJSON(nlohmann::json& json_base);

    /**
     * @brief Adds the alternate JSON representation of this object to the
     *        "blocks" lists of the base json.
     *
     * @return nlohmann::json The base json.
     */
    void virtual toLowLevelMapJSON(nlohmann::json& json_base);

    /**
     * @brief Construct a new Door object.
     *
     * @param pos The position of the block in the Minecraft world.
     * @param open Is the door open.
     * @param powered Should the door be powered upon placement.
     * @param facing Which direction the door should face.
     */
    Door(Pos& pos,
         bool open = false,
         bool powered = false,
         std::string name = "dark_oak_door",
         std::string facing = "south");

    /**
     * @brief Destroy the Lever object.
     */
    ~Door();
};
