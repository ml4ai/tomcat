package edu.arizona.tomcat.Events;

import net.minecraft.entity.EntityLivingBase;
import net.minecraftforge.event.entity.living.LivingDeathEvent;
import java.util.UUID;

public class EntityDeath extends Event {

  private String entityName;
  private UUID entityId;

  public EntityDeath(LivingDeathEvent event) {
    this.eventType = "entity_death";

    EntityLivingBase entity = event.getEntityLiving();
    this.entityId = entity.getUniqueID();
    this.entityName = entity.getName();
  }
}