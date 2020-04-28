package edu.arizona.tomcat.Events;

import edu.arizona.tomcat.World.Position;
import net.minecraft.block.Block;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraftforge.event.world.BlockEvent;

public class BlockBreakEvent extends Event {
    private String playerName;
    private Position blockPosition;
    private String blockType;
    private String blockMaterial;
    private String itemHeld;

    /** A constructor for general block breaking events. */
    public BlockBreakEvent(BlockEvent.BreakEvent event) {
        this.playerName = event.getPlayer().getDisplayNameString();
        BlockPos pos = event.getPos();
        this.blockPosition = new Position(pos);
        this.blockType = event.getState().getBlock().getClass().getName();
        this.blockMaterial =
            event.getState().getBlock().getRegistryName().toString();
        this.itemHeld =
            event.getPlayer().getHeldItemMainhand().getDisplayName();
    }
}
