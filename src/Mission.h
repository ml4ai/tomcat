#pragma once

#include "MissionSpec.h"
#include <string>

namespace tomcat {

/**
 * The Mission interface represents an abstract Minecraft mission
 */
class Mission {
  public:
  /**
   * Build the world with all the features and elements the mission must have.
   * This method must be overwritten by the subclasses of Mission to specify the
   * details of a concrete mission.
   */
  virtual void buildWorld();

  /**
   * Retrieves the object of class MissionSpec of the mission
   */
  malmo::MissionSpec getMissionSpec();

  /**
   * Defines the time limit for the mission
   * @param timeInSeconds - Time (in seconds) to the end of the mission
   */
  void setTimeLimitInSeconds(int timeInSeconds);

  /**
   * Requests video from MissionSpec
   * @param width - width of the video
   * @param height - height of the video
   */
  void requestVideo(unsigned int width, unsigned int height);
  
  void observeRecentCommands();

  void observeHotBar();

  void observeFullInventory();

  void observeChat();

  protected:
  /**
   * Retrieves the content of an XML which defines the skeleton of the world for
   * the Save and Rescue mission
   * @return
   */
  virtual std::string getWorldSkeletonFromXML() = 0;

  /**
   *
   * @param fromX - Initial position of the stairs on the x axis
   * @param fromZ - Initial position of the stairs on the z axis
   * @param fromY - Height of the ground level
   * @param width - Width of each step
   * @param height - Height of the stairs (number os steps)
   * @param orientation - South to North, North to South, West to East or East
   * to West
   * @param blockType - Type of block which the stairs are made out of
   */
  void drawStairs(int fromX,
                  int fromZ,
                  int fromY,
                  int width,
                  int height,
                  int orientation,
                  std::string blockType = "stone");

  /**
   *
   * @param fromX - Initial position of the wall on the x axis
   * @param fromZ - Initial position of the wall on the z axis
   * @param fromY - Height of the ground level
   * @param length - Length of the wall
   * @param height - Height of the wall
   * @param orientation - South to North, North to South, West to East or East
   * to West
   * @param blockType - Type of block which the wall is made out of
   */
  void drawWall(int fromX,
                int fromZ,
                int fromY,
                int length,
                int height,
                int orientation,
                std::string blockType = "stone");

  /**
   * Draw a Minecraft entity in the world.
   * @param x The east-west location.
   * @param y The up-down location.
   * @param z The north-south location.
   * @param entityType A string corresponding to one of the Minecraft entity
   * types.
   */
  void drawEntity(int x, int y, int z, const std::string& entityType);

  /** Draw a tree
   * Draw a tree in the world.
   * @param x The east-west location.
   * @param z The north-south location.
   * @param fromY y-position of the base of the tree
   */
  void drawTree(int x, int z, int fromY = 0);

  /**
   * @param fromX - Initial position of the plane on the x axis
   * @param fromZ - Initial position of the plane on the z axis
   * @param fromY - Initial position of the plane on the y axis
   * @param width - Width of the plane
   * @param height - Height of the plane
   * @param depth - Depth of the plane
   * @param blockType - Type of block which the plane is made out of
   */
  void drawPlane(int fromX,
                 int fromZ,
                 int fromY,
                 int width,
                 int height,
                 int depth,
                 std::string blockType);

  /**
   *
   * @param fromX - Southwest corner of the room on the x axis
   * @param fromZ - Southwest corner of the room on the z axis
   * @param fromY - Height of the ground level
   * @param width - Width of the room
   * @param height - Height of the walls in the room
   * @param depth - Depth of the room
   * @param withRoof - Option to include a roof in the room
   * @param blockType - Type of block which the room is made out of
   */
  void drawRoom(int fromX,
                int fromZ,
                int fromY,
                int width,
                int height,
                int depth,
                bool withRoof = true,
                std::string blockType = "stone");

  /**
   *
   * @param fromX - Initial position of the roof on the x axis
   * @param fromZ - Initial position of the roof on the z axis
   * @param groundFloorLevel - Ground floor level
   * @param width - Width of the roof
   * @param width - Relative height of the roof regarding the ground floor level
   * @param depth - Depth of the roof
   * @param blockType - Type of block which the roof is made out of
   */
  void drawRoof(int fromX,
                int fromZ,
                int groundFloorLevel,
                int width,
                int height,
                int depth,
                std::string blockType = "stone");

  /**
   *
   * @param fromX - Initial position of the hole on the x axis
   * @param fromZ - Initial position of the hole on the z axis
   * @param fromY - Initial position of the hole on the y axis
   * @param width - Width of the hole
   * @param height - Height of the hole
   * @param depth - Depth of the hole
   */
  void
  makeHole(int fromX, int fromZ, int fromY, int width, int height, int depth);

  enum orientation {
    west_east = 1,
    east_west = 2,
    south_north = 3,
    north_south = 4
  };
  malmo::MissionSpec missionSpec;
  int timeLimitInSeconds;


};
} // namespace tomcat
