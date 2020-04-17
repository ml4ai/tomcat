package edu.arizona.tomcat.ASISTBlocks;

import edu.arizona.tomcat.Utils.DiscreteEventsHelper;
import java.util.HashSet;
import java.util.Set;
import net.minecraft.block.BlockLever;
import net.minecraft.block.state.IBlockState;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.EnumHand;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

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

    DiscreteEventsHelper.writeBlockEvent(
        pos, playerIn, "lever_flipped"); // Used to mark discrete occurrence
    counter++;

    return result;
  }
}
