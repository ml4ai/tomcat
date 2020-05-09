package edu.arizona.tomcat.Events;

import net.minecraft.block.Block;
import net.minecraft.block.BlockLever;
import net.minecraft.block.properties.PropertyBool;
import net.minecraftforge.event.entity.player.PlayerInteractEvent;

public class LeverFlip extends BlockInteraction {

    /**
     * Returns true if the lever was originally powered (prior to the player
     * right-clicking it), and false otherwise.
     */
    private Boolean wasPowered;

    /** A constructor for lever flipping events. */
    public LeverFlip(PlayerInteractEvent.RightClickBlock event) {
        super(event);
        this.wasPowered =
            this.getBlockState(event).getValue(BlockLever.POWERED);
    }
}
