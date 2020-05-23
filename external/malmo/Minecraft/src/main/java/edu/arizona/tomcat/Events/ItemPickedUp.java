package edu.arizona.tomcat.Events;
import net.minecraftforge.fml.common.gameevent.PlayerEvent.ItemPickupEvent;

public class ItemPickedUp extends Event {
    private String playerName;
    private String item;

    /** A constructor for general block interaction events. */
    public ItemPickedUp(ItemPickupEvent event) {
        this.playerName = event.player.getDisplayNameString();
        this.item = event.pickedUp.getName();
    }
}
