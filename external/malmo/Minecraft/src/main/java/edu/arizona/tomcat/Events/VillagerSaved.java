package edu.arizona.tomcat.Events;

import edu.arizona.tomcat.World.Position;
import java.util.UUID;
import net.minecraft.entity.Entity;

public class VillagerSaved extends Event {

    UUID villagerId;
    Position villagerPos;

    public VillagerSaved(Entity villager) {

        this.villagerId = villager.getUniqueID();
        this.villagerPos = new Position(villager);
    }
}
