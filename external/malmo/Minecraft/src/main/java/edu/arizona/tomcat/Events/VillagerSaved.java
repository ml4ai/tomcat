package edu.arizona.tomcat.Events;

import edu.arizona.tomcat.World.Position;
import net.minecraft.entity.Entity;

import java.util.UUID;

public class VillagerSaved extends Event {

    UUID villagerId;
    Position villagerPos;

    public VillagerSaved(Entity villager) {

        this.villagerId = villager.getUniqueID();
        this.villagerPos = new Position(villager);
    }
}
