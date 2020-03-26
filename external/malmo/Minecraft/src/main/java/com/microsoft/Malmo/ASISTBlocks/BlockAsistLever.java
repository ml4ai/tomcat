package com.microsoft.Malmo.ASISTBlocks;

import net.minecraft.block.BlockLever;
import net.minecraft.block.state.IBlockState;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.EnumHand;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

import java.util.HashSet;
import java.util.Set;


/**
 * This class defines a new lever that can write event observations
 * whenever it is triggered by the player.
 * <p> The block is a lever, so it
 * contains special methods that define boundaries such that the player must
 * press within the boundaries to achieve the effect of triggering the lever.
 * The lever may show up as a full cube, but only a part of it can be pressed.
 * This can be made more clear through the use of appropriate textures.
 */
public class BlockAsistLever extends BlockLever {

    private static Set<BlockPos> openDoors = new HashSet<BlockPos>();
    private static int counter = 1;

    public BlockAsistLever() {
        setUnlocalizedName("ASIST_Lever");
        setRegistryName(
                "ASIST_Lever"); // The name Minecraft sees. Also used in en_US.lang

        this.setCreativeTab(
                CreativeTabs.REDSTONE); // shows up in redstone tab in creative mode
    }

    /**
     * Called when the block is right clicked by a player.
     */
    @Override
    public boolean onBlockActivated(World worldIn,
                                    BlockPos pos,
                                    IBlockState state,
                                    EntityPlayer playerIn,
                                    EnumHand hand,
                                    EnumFacing facing,
                                    float hitX,
                                    float hitY,
                                    float hitZ) {
        boolean result = super.onBlockActivated(
                worldIn, pos, state, playerIn, hand, facing, hitX, hitY, hitZ);


        /*
        This method is called once by the server and once by the client for the same update, so we use a
        counter to make sure the correct event is measured only once. It ensures we dont get a
        Door Open and Door Close consecutively on pressing the lever once
         */

        if (counter % 2 == 0) {
            DiscreteEventsHelper.printEventOccurence(pos, playerIn, getLeverEvent(pos)); // Used to mark discrete occurence
        }

        counter++;

        return result;
    }

    private String getLeverEvent(BlockPos pos) {
        if (openDoors.contains(pos)) {
            openDoors.remove(pos);
            return "Door Closed";
        } else {
            openDoors.add(pos);
            return "Door Opened";
        }
    }
}

