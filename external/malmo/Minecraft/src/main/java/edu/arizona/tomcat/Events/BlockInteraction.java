package edu.arizona.tomcat.Events;

import edu.arizona.tomcat.World.Position;
import net.minecraft.util.math.BlockPos;
import net.minecraftforge.event.entity.player.PlayerInteractEvent;
import com.google.gson.annotations.Expose;
import net.minecraft.block.Block;

public class BlockInteraction extends Event {
    private String playerName;
    private Position blockPosition;
    private String blockType;

    @Expose(serialize=false)
    protected Block block;

    protected Block getBlock(PlayerInteractEvent.RightClickBlock event) {
        return event.getWorld().getBlockState(event.getPos()).getBlock();
    }

    /** A constructor for general block interaction events. */
    public BlockInteraction(PlayerInteractEvent.RightClickBlock event) {
        this.eventType = "block_interaction";
        this.playerName = event.getEntityPlayer().getDisplayNameString();
        BlockPos pos = event.getPos();
        this.blockPosition = new Position(pos);
        this.blockType = this.getBlock(event).getClass().getName();
    }
}
