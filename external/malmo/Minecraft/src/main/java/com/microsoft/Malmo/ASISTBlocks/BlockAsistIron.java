package com.microsoft.Malmo.ASISTBlocks;

import edu.arizona.tomcat.Utils.DiscreteEventsHelper;
import net.minecraft.block.Block;
import net.minecraft.block.BlockAir;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

public class BlockAsistAir extends BlockAir {

    public BlockAsistAir() {

        setUnlocalizedName("ASIST_Air_Block");
        setRegistryName(
                "ASIST_Air_Block"); // The name Minecraft sees. Also used in en_US.lang

        this.setCreativeTab(
                CreativeTabs.REDSTONE); // shows up in redstone tab in creative mode
    }

    @Override
    /**
     * Called by ItemBlocks after a block is set in the world, to allow post-place logic
     */
    public void onBlockPlacedBy(World worldIn, BlockPos pos, IBlockState state, EntityLivingBase placer, ItemStack stack)
    {
        DiscreteEventsHelper.printEventOccurrence(
                pos, null, "Hit-Controlled Door Opened"); // Used to mark discrete occurrence
    }



}
