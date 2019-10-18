package edu.arizona.tomcat.World;

public class Stairs extends CompositeDrawingObject {
	
	/**
	 * Constructor
	 * @param xBase - Position of the left corner in the x axis
	 * @param zBase - Position of the left corner in the z axis
	 * @param yBase - Height of the base of the stairs
	 */
	public Stairs(int xBase, int zBase, int yBase) {
		super();
		this.createBlocks(xBase, zBase, yBase);
	}
	
	/**
	 * Creates blocks that form the stairs
	 * @param xBase - Position of the left corner in the x axis
	 * @param zBase - Position of the left corner in the z axis
	 * @param yBase - Height of the base of the stairs
	 */
	private void createBlocks(int xBase, int zBase, int yBase) {
		
	}	

}
