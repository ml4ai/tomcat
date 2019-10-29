package edu.arizona.tomcat.World;

import com.microsoft.Malmo.Schemas.BlockType;

public class Room extends CompositeDrawingObject{
	
	private static final int THICKNESS_OF_THE_WALLS = 1;
		
	/**
	 * Constructor
	 * @param x - Bottom-left position of the room in the x axis
	 * @param y - Bottom-left position of the room in the y axis
	 * @param z - Bottom-left position of the room in the z axis
	 * @param width - Width of the room
	 * @param height - Height of the room
	 * @param depth - Depth of the room
	 * @param type - Type of the block
	 */
	public Room(int x, int y, int z, int width, int height, int depth, BlockType type, boolean withRoof) {
		super();
		this.createRoom(x, y, z, width, height, depth, type, withRoof);
	}
	
	/**
	 * Creates a room and adds it to the list of objects of the drawing
	 * @param x - Bottom-left position of the room in the x axis
	 * @param y - Bottom-left position of the room in the y axis
	 * @param z - Bottom-left position of the room in the z axis
	 * @param width - Width of the room
	 * @param height - Height of the room
	 * @param depth - Depth of the room
	 * @param type - Type of the block
	 * @return
	 */
	public void createRoom(int x, int y, int z, int width, int height, int depth, BlockType type, boolean withRoof) {
		Plane west_wall = new Plane(x, y, z, THICKNESS_OF_THE_WALLS, height, depth + 2, type);
		Plane north_wall = new Plane(x - 1, y, z + depth + 1, width, height, THICKNESS_OF_THE_WALLS, type);
		Plane east_wall = new Plane(x - width - 1, y, z, THICKNESS_OF_THE_WALLS, height, depth + 2, type);
		Plane south_wall = new Plane(x - 1, y, z, width, height, THICKNESS_OF_THE_WALLS, type);
		
		this.mergeWith(west_wall);
		this.mergeWith(north_wall);
		this.mergeWith(east_wall);
		this.mergeWith(south_wall);
		
		if (withRoof) {
			Plane roof = new Plane(x, y + height, z, width + 2, THICKNESS_OF_THE_WALLS, depth,type);
			this.mergeWith(roof);
		}			
	}

}
