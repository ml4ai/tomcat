package edu.arizona.tomcat.World;

import java.util.ArrayList;
import java.util.List;
import net.minecraft.util.math.BlockPos;

/**
 * This file contains the class MultiRoomBuilding which extends the
 * class Building. The instances of this class are initialized with
 * x,y,z coordinates for the main room of the building.
 * <p>
 * At the time of creation
 * the MultiRoomBuilding object only has the one main room. Additional rooms can
 * be added through the addRoom method. The additional rooms added in this
 * manner are called additional rooms and are separate from the main room the
 * object is initialized with.
 */
public class MultiRoomBuilding extends Building {

    private List<BlockPos> additionalRooms;
    private List<BlockPos> filledAdditionalRooms;

    /**
     * This constructor requires the x,y and z coordinates of the main room of
     * the building.
     *
     * @param x - The x coordinate of the building as an integer
     * @param y - The y coordinate of the building as an integer
     * @param z - The z coordinate of the building as an integer
     */
    public MultiRoomBuilding(int x, int y, int z) {
        super(x, y, z);
        this.additionalRooms = new ArrayList<BlockPos>();
        this.filledAdditionalRooms = new ArrayList<BlockPos>();
    }

    /**
     * This method will add a room with the coordinates x,y,z to the
     * MultiRoomBuilding. This new room is called an additional room.
     *
     * @param x - The x coordinate of the building as an integer
     * @param y - The y coordinate of the building as an integer
     * @param z - The z coordinate of the building as an integer
     */
    public void addRoom(int x, int y, int z) {
        this.additionalRooms.add(new BlockPos(x, y, z));
    }

    /**
     * This method will return a list of the additional rooms in the building
     * as a list of coordinates where each coordinate corresponds to one
     * additional room. Each coordinate is a BlockPos object.
     * <p>
     * The main room is not included in this list. Use the superclass method for
     * that.
     *
     * @return List<BlockPos> - A list of BlockPos representing the coordinates
     *     of
     * the additional rooms
     */
    public List<BlockPos> getAdditionalRooms() { return this.additionalRooms; }

    /**
     * This method will return a list of the filled additional rooms in the
     * building as a list of coordinates where each coordinate corresponds to
     * one additional room. Each coordinate is a BlockPos object. <p> The main
     * room is never included in this list. Use the superclass method for that.
     *
     * @return List<BlockPos> - A list of BlockPos representing the coordinates
     *     of
     * the filled additional rooms
     */
    public List<BlockPos> getFilledAdditionalRooms() {
        return this.filledAdditionalRooms;
    }

    /**
     * This method will return the number of additional rooms this building has
     * apart from the main room.
     *
     * @return int - Number of additional rooms.
     */
    public int getNumberOfAdditionalRooms() {
        return this.additionalRooms.size();
    }

    /**
     * When given a set of coordinates as an integer array x,y,z, this method
     * will mark the room of the building with that coordinate as filled. If the
     * coordinate passed is that of the main room, then that room will be marked
     * as filled. <p> If the coordinates passed are not of the main room or any
     * of the additional rooms, then nothing will change.
     *
     * @param coordinate - BlockPos object of coordinates
     */
    @Override
    public void markRoomAsFilled(BlockPos coordinate) {
        BlockPos mainRoomCoord = this.getMainRoomCoordinates();

        if (coordinate.equals(mainRoomCoord)) {
            this.fillMainRoom();
        }
        else if (this.additionalRooms.contains(coordinate)) {
            this.filledAdditionalRooms.add(coordinate);
        }
        else {
            ;
        }
    }
}
