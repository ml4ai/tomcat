package edu.arizona.tomcat.Events;

import edu.arizona.tomcat.World.Position;
import net.minecraft.util.math.BlockPos;
import net.minecraft.block.state.IBlockState;
import net.minecraftforge.event.world.BlockEvent;
import net.minecraft.block.Block;

public class BlockBreakEvent extends Event {
    private String playerName;
    private Position blockPosition;
    private String blockType;

    protected IBlockState getBlockState(BlockEvent.BreakEvent event) {
        return event.getWorld().getBlockState(event.getPos());
    }

    protected Block getBlock(BlockEvent.BreakEvent event) {
        return this.getBlockState(event).getBlock();
    }


    /** A constructor for general block interaction events. */
    public BlockBreakEvent(BlockEvent.BreakEvent event) {
        this.playerName = event.getPlayer().getDisplayNameString();
        BlockPos pos = event.getPos();
        this.blockPosition = new Position(pos);
        this.blockType = this.getBlock(event).getClass().getName();
    }
}
