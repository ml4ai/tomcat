package edu.arizona.tomcat.World;

import com.microsoft.Malmo.Schemas.DrawEntity;
import com.microsoft.Malmo.Schemas.EntityTypes;
import com.microsoft.Malmo.Schemas.SpawnableTypes;
import java.math.BigDecimal;
import java.util.UUID;

public class TomcatEntity extends CompositeDrawingObject {

  /**
   * Constructor
   * @param uniqueId - Id that uniquely identifies the entity
   * @param x - Position of the entity in the x axis
   * @param y - Position of the entity in the y axis
   * @param z - Position of the entity in the z axis
   * @param type - Type of the entity
   */
  public TomcatEntity(UUID uniqueId, int x, int y, int z, EntityTypes type) {
    super();
    this.createEntity(uniqueId, x, y, z, type);
  }

  /**
   * Constructor
   * @param x - Position of the entity in the x axis
   * @param y - Position of the entity in the y axis
   * @param z - Position of the entity in the z axis
   * @param type - Type of the entity
   */
  public TomcatEntity(int x, int y, int z, EntityTypes type) {
    super();
    this.createEntity(null, x, y, z, type);
  }

  /**
   * Creates an entity and adds it to the list of objects of the drawing
   * @param x - Position of the entity in the x axis
   * @param y - Position of the entity in the y axis
   * @param z - Position of the entity in the z axis
   * @param type - Type of the entity
   * @return
   */
  public void
  createEntity(UUID uniqueId, int x, int y, int z, EntityTypes type) {
    DrawEntity entity = new DrawEntity();
    SpawnableTypes entityType = new SpawnableTypes();
    entityType.setValue(type.value());

    if (uniqueId != null) {
      entity.setUniqueId(uniqueId.toString());
    }
    entity.setType(entityType);
    entity.setX(new BigDecimal(x));
    entity.setY(new BigDecimal(y));
    entity.setZ(new BigDecimal(z));
    this.malmoDrawObjects.add(entity);
  }
}
