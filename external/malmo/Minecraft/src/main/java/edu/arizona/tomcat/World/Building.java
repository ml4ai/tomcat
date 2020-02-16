package edu.arizona.tomcat.World;

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

    private int[] mainRoomCoordinates;
    private boolean mainRoomFilled;

    /**
     * This constructor requires the x,y and z coordinates of the room of the building.
     *
     * @param x - The x coordinate of the building as an integer
     * @param y - The y coordinate of the building as an integer
     * @param z - The z coordinate of the building as an integer
     */
    public Building(int x, int y, int z) {
        this.mainRoomCoordinates = new int[3];
        this.mainRoomCoordinates[0] = x;
        this.mainRoomCoordinates[1] = y;
        this.mainRoomCoordinates[2] = z;
        this.mainRoomFilled = false;
    }

    /**
     * This method will return the x coordinate of the main room
     *
     * @return int - coordinate on x axis
     */
    public int getX() {
        return this.mainRoomCoordinates[0];
    }

    /**
     * This method will return the y coordinate of the main room
     *
     * @return int - coordinate on y axis
     */
    public int getY() {
        return this.mainRoomCoordinates[1];
    }

    /**
     * This method will return the z coordinate of the main room
     *
     * @return int - coordinate on z axis
     */
    public int getZ() {
        return this.mainRoomCoordinates[2];
    }

    /**
     * This method will designate the main room as a
     * filled room.
     */
    public void fillMainRoom() {
        this.mainRoomFilled = true;
    }

    /**
     * This method will designate the main room as an empty
     * room.
     */
    public void emptyMainRoom() {
        this.mainRoomFilled = false;
    }

    /**
     * This method will return true if the main room is designated as full, and it
     * will return false if the room is empty.
     *
     * @return boolean - true or false depending on if the main room is filled or not.
     */
    public boolean mainRoomIsFilled() {
        return this.mainRoomFilled;
    }
}
