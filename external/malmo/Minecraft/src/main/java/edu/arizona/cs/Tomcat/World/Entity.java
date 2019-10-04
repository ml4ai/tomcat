package edu.arizona.cs.Tomcat.World;

import java.math.BigDecimal;

import com.microsoft.Malmo.Schemas.DrawEntity;
import com.microsoft.Malmo.Schemas.EntityTypes;
import com.microsoft.Malmo.Schemas.SpawnableTypes;

public class Entity extends DrawingObject{
	
	/**
	 * Constructor
	 * @param x - Position of the entity in the x axis
	 * @param z - Position of the entity in the z axis
	 * @param y - Position of the entity in the y axis
	 * @param type - Type of the entity
	 */
	public Entity(double x, double z, double y, EntityTypes type) {
		super();
		this.createEntity(x, z, y, type);
	}
	
	/**
	 * Creates an entity and adds it to the list of objects of the drawing
	 * @param x - Position of the entity in the x axis
	 * @param z - Position of the entity in the z axis
	 * @param y - Position of the entity in the y axis
	 * @param type - Type of the entity
	 * @return
	 */
	public DrawEntity createEntity(double x, double z, double y, EntityTypes type) {
		DrawEntity entity = new DrawEntity();
		SpawnableTypes entityType = new SpawnableTypes();
		entityType.setValue(type.value());
		entity.setType(entityType);
		entity.setX(new BigDecimal(x));
		entity.setZ(new BigDecimal(z));
		entity.setY(new BigDecimal(y));
		this.malmoDrawObjects.add(entity);		
		return entity;
	}

}
