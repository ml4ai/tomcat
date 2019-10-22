package edu.arizona.tomcat.World;

import java.util.ArrayList;
import java.util.Iterator;

public class Drawing {
	
	private ArrayList<CompositeDrawingObject> objects;
	
	/**
	 * Constructor
	 */
	public Drawing() {
		this.objects = new ArrayList<CompositeDrawingObject>();
	}
	
	/**
	 * List of objects in the drawing
	 * @return
	 */
	public Iterator<CompositeDrawingObject> getObjects() {
		return this.objects.iterator();
	}
	
	/**
	 * Adds a new object to the drawing
	 * @param object
	 */
	public void addObject(CompositeDrawingObject object) {
		this.objects.add(object);
	}
	
	/**
	 * Removes an object to the drawing
	 * @param object
	 */
	public void removeObject(CompositeDrawingObject object) {
		this.objects.remove(object);
	}

}
