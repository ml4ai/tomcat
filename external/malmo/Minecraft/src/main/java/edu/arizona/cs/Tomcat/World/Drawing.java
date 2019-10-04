package edu.arizona.cs.Tomcat.World;

import java.util.ArrayList;
import java.util.Iterator;

public class Drawing {
	
	private ArrayList<DrawingObject> objects;
	
	/**
	 * Constructor
	 */
	public Drawing() {
		this.objects = new ArrayList<DrawingObject>();
	}
	
	/**
	 * List of objects in the drawing
	 * @return
	 */
	public Iterator<DrawingObject> getObjects() {
		return this.objects.iterator();
	}
	
	/**
	 * Adds a new object to the drawing
	 * @param object
	 */
	public void addObject(DrawingObject object) {
		this.objects.add(object);
	}
	
	/**
	 * Removes an object to the drawing
	 * @param object
	 */
	public void removeObject(DrawingObject object) {
		this.objects.remove(object);
	}

}
