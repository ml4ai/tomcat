package edu.arizona.tomcat.Events;

import net.minecraft.block.Block;
import net.minecraft.block.BlockDoor;
import net.minecraft.block.properties.PropertyBool;
import net.minecraftforge.event.entity.player.PlayerInteractEvent;

public class DoorInteraction extends BlockInteraction {

    /**
     * Returns true if the door was originally open (prior to the player
     * right-clicking it), and false otherwise.
     */
    private Boolean wasOpen;

    /** A constructor for door opening/closing events. */
    public DoorInteraction(PlayerInteractEvent event) {
        super(event);
        this.wasOpen = BlockDoor.isOpen(event.getWorld(), event.getPos());
    }
}
