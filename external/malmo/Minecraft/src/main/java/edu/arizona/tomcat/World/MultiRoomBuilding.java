package edu.arizona.tomcat.World;

import java.util.ArrayList;
import java.util.List;

/**
 * This file contains the class MultiRoomBuilding which extends the
 * class Building. The instances of this class are initialized with
 * x,y,z coordinates for the main room of the building.
 * <p>
 * At the time of creation
 * the MultiRoomBuilding object only has the one main room. Additional rooms can be
 * added through the addRoom method. The additional rooms added in this manner are called additional
 * rooms and are separate from the main room the object is initialized with.
 */
public class MultiRoomBuilding extends Building {

    private List<int[]> additionalRooms;
    private List<int[]> filledAdditionalRooms;

    /**
     * This constructor requires the x,y and z coordinates of the main room of the building.
     *
     * @param x - The x coordinate of the building as an integer
     * @param y - The y coordinate of the building as an integer
     * @param z - The z coordinate of the building as an integer
     */
    public MultiRoomBuilding(int x, int y, int z) {
        super(x, y, z);
        this.additionalRooms = new ArrayList<int[]>();
        this.filledAdditionalRooms = new ArrayList<int[]>();
    }

    /**
     * This method will add a room with the coordinates x,y,z to the MultiRoomBuilding.
     * This new room is called an additional room and is the second room in the building
     * after the main room.
     *
     * @param x - The x coordinate of the building as an integer
     * @param y - The y coordinate of the building as an integer
     * @param z - The z coordinate of the building as an integer
     */
    public void addRoom(int x, int y, int z) {
        int[] newRoom = {x, y, z};
        this.additionalRooms.add(newRoom);
    }

    /**
     * This method will return a list of the additional rooms in the building
     * as a list of coordinates. each integer array in the returned list
     * is of size 3 and stores x,y,z coordinates for an additional room.
     * <p>
     * The main room is not included in this list.Use the superclass method for that.
     *
     * @return List<int [ ]> - A list of size 3 arrays representing the coordinates of the additional rooms
     */
    public List<int[]> getAdditionalRooms() {
        return this.additionalRooms;
    }

    /**
     * This method will return a list of the filled additional rooms in the building
     * as a list of coordinates. each integer array in the returned list
     * is of size 3 and stores x,y,z coordinates for a filled additional room.
     * <p>
     * The main room is never included in this list. Use the superclass method for that.
     *
     * @return List<int [ ]> - A list of size 3 arrays representing the coordinates of the additional rooms
     */
    public List<int[]> getFilledAdditionalRooms() {
        return this.filledAdditionalRooms;
    }

    /**
     * This method will return the number of additional rooms this building has apart from the main room.
     *
     * @return int - Number of additional rooms.
     */
    public int getNumberOfAdditionalRooms() {
        return this.additionalRooms.size();
    }

    /**
     * When given a set of coordinates as an integer array x,y,z, this method will
     * mark the room of the building with that coordinate as filled. If the coordinate
     * passed is that of the main room, then that room will be marked as filled.
     * <p>
     * If the coordinates passed are not of the main room or any of the additional rooms, then
     * nothing will change.
     *
     * @param coordinate - int[x,y,z], an array of size 3 representing a 3 point coordinate for
     *                   the room to be marked as filled.
     */
    public void markRoomAsFilled(int[] coordinate) {
        int[] mainRoomCoord = {this.getX(), this.getY(), this.getZ()};
        if (coordinate.equals(mainRoomCoord)) {
            this.fillMainRoom();
        } else if (this.additionalRooms.contains(coordinate)) {
            this.filledAdditionalRooms.add(coordinate);
        } else {
            ;
        }
    }

}
