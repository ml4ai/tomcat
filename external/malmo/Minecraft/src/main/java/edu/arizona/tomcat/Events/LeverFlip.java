package edu.arizona.tomcat.Events;

import net.minecraft.block.properties.PropertyBool;
import net.minecraft.block.BlockLever;
import net.minecraftforge.event.entity.player.PlayerInteractEvent;
import net.minecraft.block.Block;

public class LeverFlip extends BlockInteraction {

    /** Returns true if the lever was originally on (prior to the player
     * right-clicking it), and false otherwise. */
    private Boolean powered;

    /** A constructor for lever flipping events. */
    public LeverFlip(PlayerInteractEvent.RightClickBlock event) {
        super(event);
        this.powered = this.getBlockState(event).getValue(BlockLever.POWERED);
    }
}
