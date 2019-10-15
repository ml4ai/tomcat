package edu.arizona.tomcat.World;

import com.microsoft.Malmo.Schemas.DrawItem;
import com.microsoft.Malmo.Schemas.ItemType;

public class Item extends DrawingObject{
	
	/**
	 * Constructor
	 * @param x - Position of the item in the x axis
	 * @param y - Position of the item in the y axis
	 * @param z - Position of the item in the z axis
	 * @param type - Type of the item
	 */
	public Item(int x, int y, int z, ItemType type) {
		super();
		this.createItem(x, y, z, type);
	}
	
	/**
	 * Creates an item and adds it to the list of objects of the drawing
	 * @param x - Position of the item in the x axis
	 * @param y - Position of the item in the y axis
	 * @param z - Position of the item in the z axis 
	 * @param type - Type of the item
	 * @return
	 */
	public void createItem(int x, int y, int z, ItemType type) {
		DrawItem item = new DrawItem();
		item.setX(x);
		item.setY(y);
		item.setZ(z);
		item.setType(type.value());
		this.malmoDrawObjects.add(item);	
	}

}
