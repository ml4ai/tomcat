package edu.arizona.tomcat.Events;

import net.minecraft.block.properties.PropertyBool;
import net.minecraft.block.BlockLever;
import net.minecraftforge.event.entity.player.PlayerInteractEvent;
import net.minecraft.block.Block;

public class LeverFlip extends BlockInteraction {

    /** Returns true if the lever is powered, false otherwise. */
    private Boolean powered;

    /** A constructor for lever flipping events. */
    public LeverFlip(PlayerInteractEvent.RightClickBlock event) {
        super(event);
        this.eventType = this.getClass().getName();
        this.powered = this.getBlock(event).getBlockState().getBaseState().getValue(BlockLever.POWERED);
    }
}
