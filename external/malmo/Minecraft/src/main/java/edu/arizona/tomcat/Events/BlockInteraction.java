package edu.arizona.tomcat.Events;

import edu.arizona.tomcat.World.Position;
import net.minecraft.util.math.BlockPos;
import net.minecraft.block.state.IBlockState;
import net.minecraftforge.event.entity.player.PlayerInteractEvent;
import com.google.gson.annotations.Expose;
import net.minecraft.block.Block;

public class BlockInteraction extends Event {
    private String playerName;
    private Position blockPosition;
    private String blockType;

    protected IBlockState getBlockState(PlayerInteractEvent.RightClickBlock event) {
        return event.getWorld().getBlockState(event.getPos());
    }

    protected Block getBlock(PlayerInteractEvent.RightClickBlock event) {
        return this.getBlockState(event).getBlock();
    }

    /** A constructor for general block interaction events. */
    public BlockInteraction(PlayerInteractEvent.RightClickBlock event) {
        this.playerName = event.getEntityPlayer().getDisplayNameString();
        BlockPos pos = event.getPos();
        this.blockPosition = new Position(pos);
        this.blockType = this.getBlock(event).getClass().getName();
    }
}
