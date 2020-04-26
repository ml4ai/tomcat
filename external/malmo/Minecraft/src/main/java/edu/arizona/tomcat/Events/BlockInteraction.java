package edu.arizona.tomcat.Events;

import edu.arizona.tomcat.World.Position;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.event.entity.player.PlayerInteractEvent;
import com.google.gson.annotations.Expose;
import net.minecraft.block.Block;

public class BlockInteraction extends Event {
    private String playerName = null;
    private Position blockPosition;
    private String blockType;

    @Expose(serialize=false)
    protected Block block;

    /** A constructor for general block interaction events. */
    public BlockInteraction(PlayerInteractEvent.RightClickBlock event) {
        this.eventType = "block_interaction";
        World world = event.getWorld();
        BlockPos pos = event.getPos();
        this.playerName = event.getEntityPlayer().getDisplayNameString();
        this.blockPosition = new Position(pos);
        this.block = world.getBlockState(pos).getBlock();
        this.blockType = this.block.getClass().getName();
    }
}
