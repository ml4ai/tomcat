package edu.arizona.tomcat.Events;

import java.util.UUID;

import net.minecraft.entity.Entity;
import net.minecraftforge.event.entity.living.LivingDeathEvent;

public class EntityDeath extends Event {

    private String entityType;
    private String entityName;
    private UUID entityId;

    public EntityDeath(LivingDeathEvent event) {
        this.eventType = "entity_death";

        Entity entity = event.getEntity();
        this.entityId = entity.getUniqueID();
        this.entityName = entity.getName();
        this.entityType = entity.getClass().getName();
    }
}
