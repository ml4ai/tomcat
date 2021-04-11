/**
 * @brief This file defines the members and methods
 *        implemented as part of the Pos class
 */
#pragma once

#include <memory>
#include <nlohmann/json.hpp>
#include <string>
/**
 * @brief This class represents a point in a 3D integer lattice.
 */
class Pos {

  private:
    int x;
    int y;
    int z;

  public:
    /**
     * @brief Get the X coordinate of this object
     *
     * @return int The x coordinate
     */
    int getX();

    /**
     * @brief Get the Y coordinate of this object
     *
     * @return int The y coordinate
     */
    int getY();

    /**
     * @brief Get the Z coordinate of this object
     *
     * @return int The z coordinate
     */
    int getZ();

    /**
     * @brief Set the x value of this Pos object
     *
     * @param x The value x is to be set to
     */
    void setX(int x);

    /**
     * @brief Set the y value of this Pos object
     *
     * @param y The value y is to be set to
     */
    void setY(int y);

    /**
     * @brief Set the z value of this Pos object
     *
     * @param z The value z is to be set to
     */
    void setZ(int z);

    /**
     * @brief Shift the x value by a given amount
     *
     * @param shift The amount to shift by which may be positive or negative
     */
    void shiftX(int shift);

    /**
     * @brief Shift the y value by a given amount
     *
     * @param shift The amount to shift by which may be positive or negative
     */
    void shiftY(int shift);

    /**
     * @brief Shift the z value by a given amount
     *
     * @param shift The amount to shift by which may be positive or negative
     */
    void shiftZ(int shift);

    /**
     * @brief Gets the JSON representation of the various
     *        fields and values stored in an instance
     *
     * @return nlohmann::json The JSON representation
     */
    nlohmann::json virtual toSemanticMapJSON();

    /**
     * @brief Construct a new Pos object
     */
    Pos();

    /**
     * @brief Construct a new Pos object
     *
     * @param x The x coordinate
     * @param y The y coordinate
     * @param z The z coordinate
     */
    Pos(int x, int y, int z);

    /**
     * @brief Construct a new Pos object as a copy
     *        of the given Pos object
     *
     * @param other The object whose fields are to be copied
     */

    /**
     * @brief Destroy the Pos object
     */
    ~Pos();
};
