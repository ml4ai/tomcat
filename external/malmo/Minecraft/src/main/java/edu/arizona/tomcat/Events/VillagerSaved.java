package edu.arizona.tomcat.Events;

import edu.arizona.tomcat.World.Position;
import net.minecraft.entity.Entity;

import java.util.UUID;

public class VillagerSaved extends Event {

    UUID villagerID;
    Position villagerPos;

    public VillagerSaved(Entity villager) {

        this.villagerID = villager.getUniqueID();
        this.villagerPos = new Position(villager);
    }
}
