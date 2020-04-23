package edu.arizona.tomcat.Events;

import edu.arizona.tomcat.World.Position;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.event.entity.player.PlayerInteractEvent;

public class BlockInteraction extends Event {
    private String playerName = null;
    private Position blockPosition;
    private String blockType = "";

    /** A constructor for general block interaction events. */
    public BlockInteraction(PlayerInteractEvent.RightClickBlock event) {
        this.eventType = "block_interaction";
        World world = event.getWorld();
        BlockPos pos = event.getPos();
        this.playerName = event.getEntityPlayer().getDisplayNameString();
        this.blockPosition = new Position(pos);
        this.blockType =
            world.getBlockState(pos).getBlock().getClass().getName();
    }
}
