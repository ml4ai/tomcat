package edu.arizona.tomcat.Events;
import net.minecraftforge.fml.common.gameevent.PlayerEvent.ItemPickupEvent;
import net.minecraftforge.event.entity.player.EntityItemPickupEvent;

public class ItemPickedUp extends Event {
    private String playerName;
    private String item;

    /** A constructor for general block interaction events. */
    public ItemPickedUp(EntityItemPickupEvent event) {
        this.playerName = event.getEntityPlayer().getDisplayNameString();
        this.item = event.getItem().getName();
    }
}
