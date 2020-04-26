package edu.arizona.tomcat.Events;

import edu.arizona.tomcat.World.Position;
import edu.arizona.tomcat.Events.BlockInteraction;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.event.entity.player.PlayerInteractEvent;
import net.minecraft.block.properties.PropertyBool;

public class LeverFlip extends BlockInteraction {
    /** A constructor for general block interaction events. */
    private PropertyBool powered;

    protected LeverFlip(PlayerInteractEvent.RightClickBlock event) {
        super(event);
        this.eventType = this.getClass().getName();
        this.powered = this.block.blockState.getValue(POWERED).booleanValue();
    }
}
