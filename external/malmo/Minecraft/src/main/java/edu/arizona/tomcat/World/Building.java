package edu.arizona.tomcat.World;

import java.util.ArrayList;
import java.util.Iterator;

import net.minecraft.util.math.BlockPos;

public class Building {

	private ArrayList<BlockPos> roomsCoordinates;
	private ArrayList<Boolean> occupation;

	/**
	 * This constructor requires the x,y and z coordinates of the main room of the
	 * building.
	 *
	 * @param x - The x coordinate of the room as an integer
	 * @param y - The y coordinate of the room as an integer
	 * @param z - The z coordinate of the room as an integer
	 */
	public Building(int x, int y, int z) {
		this.roomsCoordinates = new ArrayList<BlockPos>();
		this.occupation = new ArrayList<Boolean>();
		this.roomsCoordinates.add(new BlockPos(x, y, z));
		this.occupation.add(false);
	}

	/**
	 * Indicates whether it's a single-room building or not
	 * @return
	 */
	public boolean isSingleRoom() {
		return this.roomsCoordinates.size() == 1;
	}

	/**
	 * Add a new room to the building
	 * @param x - The x coordinate of the room as an integer
	 * @param y - The y coordinate of the room as an integer
	 * @param z - The z coordinate of the rrom as an integer
	 */
	public void addRoom(int x, int y, int z) {
		this.roomsCoordinates.add(new BlockPos(x, y, z));
		this.occupation.add(false);
	}

	/**
	 * Marks room as occupied
	 * @param room
	 */
	public void setRoomOccupied(int room) {
		this.occupation.set(room, true);
	}

	/**
	 * Retrieves the coordinates of all the rooms 
	 * @return
	 */
	public Iterator<BlockPos> getRooms() {
		return this.roomsCoordinates.iterator();
	}

	/**
	 * Retrieves the coordinates of all the unoccupied rooms 
	 * @return
	 */
	public Iterator<BlockPos> getUnoccupiedRooms() {
		ArrayList<BlockPos> unoccupiedRooms = new ArrayList<BlockPos>();

		for(int i = 0; i < this.roomsCoordinates.size(); i++) {
			if(!this.occupation.get(i)) {
				unoccupiedRooms.add(this.roomsCoordinates.get(i));
			}
		}

		return unoccupiedRooms.iterator();
	}

	/**
	 * Retrieves the coordinates of a room 
	 * @return
	 */
	public BlockPos getRoomCoordinates(int room) {
		return this.roomsCoordinates.get(room);
	}

	/**
	 * Retrieves the number of rooms in the building
	 * @return
	 */
	public int getNumberOfRooms() {
		return this.roomsCoordinates.size();
	}

}
