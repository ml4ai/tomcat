package edu.arizona.tomcat.World;

import net.minecraft.util.math.BlockPos;

/**
 * This file contains the Building class which can be instantiated to represent
 * a single room building in Minecraft. At the time it is created, the instance
 * must be given the location of the building x,y,z as integers.
 * <p>
 * Since the building only has a single room, the passed parameters will be the
 * coordinates of that single room (the inside to be specific).
 * <p>
 * This single room is also called the main room.
 */
public class Building {

    private BlockPos mainRoomCoordinates;
    private boolean mainRoomFilled;

    /**
     * This constructor requires the x,y and z coordinates of the room of the
     * building.
     *
     * @param x - The x coordinate of the building as an integer
     * @param y - The y coordinate of the building as an integer
     * @param z - The z coordinate of the building as an integer
     */
    public Building(int x, int y, int z) {
        this.mainRoomCoordinates = new BlockPos(x, y, z);
        this.mainRoomFilled = false;
    }

    /**
     * This method will return coordinates of the main room as a a
     * BlockPos object.
     *
     * @return BlockPos - Three point coordinate of the main room
     */
    public BlockPos getMainRoomCoordinates() {
        return this.mainRoomCoordinates;
    }

    /**
     * This method will designate the main room as a
     * filled room.
     */
    public void fillMainRoom() { this.mainRoomFilled = true; }

    /**
     * This method is meant to be overridden. When it is called from an extended
     * class it will fill the indicated room, otherwise if it is called from an
     * instance of Building, it simply marks the main room as filled.
     *
     * @param coordinate - BlockPos coordinates of room to be filled
     */
    public void markRoomAsFilled(BlockPos coordinate) { this.fillMainRoom(); }

    /**
     * This method will designate the main room as an empty
     * room.
     */
    public void emptyMainRoom() { this.mainRoomFilled = false; }

    /**
     * This method will return true if the main room is designated as full, and
     * it will return false if the room is empty.
     *
     * @return boolean - true or false depending on if the main room is filled
     *     or
     * not.
     */
    public boolean mainRoomIsFilled() { return this.mainRoomFilled; }
}
