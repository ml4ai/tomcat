/**
 * @brief This file defines the members and methods
 * implemented as part of the Block class
 */
#pragma once
#include "Pos.h"
#include <nlohmann/json.hpp>

/**
 * @brief This class represents a Minecraft block
 */
class Block {

  protected:
    std::string material;
    Pos pos;

  public:
    /**
     * @brief Get the material of the block
     *
     * @return string The block's material
     */
    std::string getMaterial();

    /**
     * @brief Get the X coordinate of the block
     *
     * @return int The x coordinate
     */
    int getX();

    /**
     * @brief Get the Y coordinate of the block
     *
     * @return int The y coordinate
     */
    int getY();

    /**
     * @brief Get the Z coordinate of the block
     *
     * @return int The z coordinate
     */
    int getZ();

    /**
     * @brief Get the Pos object representation of the coordinates of the block
     *
     * @return Pos& The pos representation
     */
    Pos& getPos();

    /**
     * @brief Set the x value of this object
     *
     * @param x The value to set to
     */
    void setX(int x);

    /**
     * @brief Set the y value of this object
     *
     * @param y The value to set to
     */
    void setY(int y);

    /**
     * @brief Set the z value of this object
     *
     * @param z The value to set to
     */
    void setZ(int z);

    /**
     * @brief Adds the JSON representation of this object to the
     *        "locations" list of the base json
     *
     * @return nlohmann::json The base json
     */
    void virtual toSemanticMapJSON(nlohmann::json& json_base);

    /**
     * @brief Adds the alternate JSON representation of this
     * object to the "blocks" list of the base json.
     *
     * @return nlohmann::json The base json
     */
    void virtual toLowLevelMapJSON(nlohmann::json& json_base);

    /**
     * @brief Construct a new Block object
     *
     * @param material The material the block is made of
     * @param pos The position of the block in the Minecraft world
     */
    Block(std::string material, Pos& pos);

    /**
     * @brief Destroy the Block object
     */
    virtual ~Block();
};
