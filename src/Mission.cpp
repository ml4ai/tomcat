
#include "Mission.h"

using namespace malmo;
using namespace std;

namespace tomcat {

    void Mission::buildWorld() {
        string xml = this->getWorldSkeletonFromXML();
        this->missionSpec = MissionSpec(xml, true);
        this->missionSpec.forceWorldReset();
    }

    //string Mission::getWorldSkeletonFromXML() { return ""; }

    MissionSpec Mission::getMissionSpec() {
        return this->missionSpec;
    }
        
    void Mission::setTimeLimitInSeconds(int timeInSeconds) {
        this->missionSpec.timeLimitInSeconds(timeInSeconds);
    }

    void Mission::drawStairs(int fromX, int fromZ, int fromY, int width, int height, int orientation, string blockType) {
        for (int i = 0; i < height; i++) {
            switch (orientation) {
                case Mission::orientation::west_east:
                    this->drawWall(fromX - i, fromZ, fromY, width, i + 1, orientation::south_north, blockType);
                    break;
                case Mission::orientation::east_west:
                    this->drawWall(fromX + i, fromZ, fromY, width, i + 1, orientation::south_north, blockType);
                    break;
                case Mission::orientation::south_north:
                    this->drawWall(fromX, fromZ + i, fromY, width, i + 1, orientation::west_east, blockType);
                    break;
                case Mission::orientation::north_south:
                    this->drawWall(fromX, fromZ - i, fromY, width, i + 1, orientation::west_east, blockType);
                    break;
            }
        }
    }

    void Mission::drawWall(int fromX, int fromZ, int fromY, int length, int height, int orientation, string blockType) {
        switch (orientation) {
            case Mission::orientation::west_east:
                this->drawPlane(fromX, fromZ, fromY, length, height, 1, blockType);
                break;
            case Mission::orientation::east_west:
                this->drawPlane(fromX + length - 1, fromZ, fromY, length, height, 1, blockType);
                break;
            case Mission::orientation::south_north:
                this->drawPlane(fromX, fromZ, fromY, 1, height, length, blockType);
                break;
            case Mission::orientation::north_south:
                this->drawPlane(fromX, fromZ - length + 1 , fromY, 1, height, length, blockType);
                break;
        }
    }

    void Mission::drawPlane(int fromX, int fromZ, int fromY, int width, int height, int depth, string blockType) {
        for (int w = fromX; w > fromX - width; w--) {
            for (int d = fromZ; d < fromZ + depth; d++) {
                for (int h = fromY; h < fromY + height; h++) {
                    this->missionSpec.drawBlock(w, h, d, blockType);
                }
            }
        }
    }

    void Mission::drawRoom(int fromX, int fromZ, int fromY, int width, int height, int depth, bool withRoof, string blockType) {
        this->drawWall(fromX, fromZ, fromY, depth, height, Mission::orientation::south_north, blockType);
        this->drawWall(fromX - 1, fromZ + depth - 1, fromY, width, height, Mission::orientation::west_east, blockType);
        this->drawWall(fromX - width - 1, fromZ + depth - 1, fromY, depth, height, Mission::orientation::north_south,
                       blockType);
        this->drawWall(fromX - width, fromZ, fromY, width, height, Mission::orientation::east_west, blockType);

        if (withRoof) {
            this->drawRoof(fromX - 1, fromZ + 1, fromY, width, height, depth - 2);
        }
    }

    void Mission::drawRoof(int fromX, int fromZ, int groundFloorLevel, int width, int height, int depth, string blockType) {
        this->drawPlane(fromX, fromZ, groundFloorLevel + height - 1, width, 1, depth, blockType);
    }

    void Mission::makeHole(int fromX, int fromZ, int fromY, int width, int height, int depth) {
        this->drawPlane(fromX, fromZ, fromY, width, height, depth, "air");
    }

} // namespace tomcat
