package edu.arizona.cs.Tomcat.World;

import java.util.ArrayList;
import java.util.Iterator;

import com.microsoft.Malmo.Schemas.DrawObjectType;

public abstract class DrawingObject {
	
	protected ArrayList<DrawObjectType> malmoDrawObjects;
	
	/**
	 * Constructor
	 */
	public DrawingObject() {
		this.malmoDrawObjects = new ArrayList<DrawObjectType>();
	}
	
	/**
	 * Retrieves the list of Malmo draw objects that form this drawing object
	 * @return
	 */
	public Iterator<DrawObjectType> getMalmoDrawObjects() {
		return malmoDrawObjects.iterator();
	}
	

}
