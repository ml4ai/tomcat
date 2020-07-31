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

  private:
    std::string type;
    std::string material;
    Pos pos;

  public:
    /**
     * @brief Get the type of the block
     *
     * @return string The block's type
     */
    std::string getType();

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
     * @brief Gets the JSON representation of the various
     * fields and values stored in an instance
     *
     * @return nlohmann::json The JSON representation
     */
    nlohmann::json virtual toJSON();

    /**
     * @brief Gets a string representation of the various
     * fields and values stored in an instance as a TSV
     *
     * @return string The TSV representation
     */
    std::string virtual toTSV();

    /**
     * @brief Construct a new Block object
     *
     * @param material The material the block is made of
     * @param pos The position of the block in the Minecraft world
     * @param type The semantic type for this block which is not the same as
     * its material. Defaults to "normal".
     */
    Block(std::string material, Pos& pos, std::string type = "normal");

    /**
     * @brief Destroy the Block object
     */
    virtual ~Block();
};
