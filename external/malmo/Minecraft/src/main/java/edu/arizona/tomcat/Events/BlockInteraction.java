package edu.arizona.tomcat.Events;

import edu.arizona.tomcat.World.Position;
import net.minecraft.util.math.BlockPos;
import net.minecraft.block.state.IBlockState;
import net.minecraftforge.event.entity.player.PlayerInteractEvent;
import net.minecraft.block.material.MapColor;
import net.minecraft.block.Block;

public class BlockInteraction extends Event {
    private String playerName;
    private Position blockPosition;
    private String blockType;
    private String blockMaterial;
    private String clickType;

    protected IBlockState getBlockState(PlayerInteractEvent event) {
        return event.getWorld().getBlockState(event.getPos());
    }

    protected Block getBlock(PlayerInteractEvent event) {
        return this.getBlockState(event).getBlock();
    }

    /** A constructor for general block interaction events. */
    public BlockInteraction(PlayerInteractEvent event) {
        this.playerName = event.getEntityPlayer().getDisplayNameString();
        BlockPos pos = event.getPos();
        this.blockPosition = new Position(pos);
        this.blockType = this.getBlock(event).getClass().getName();
        this.blockMaterial = this.getBlock(event).getRegistryName().toString();
        this.clickType = (event instanceof PlayerInteractEvent.RightClickBlock)?"right":"left";
    }
}
