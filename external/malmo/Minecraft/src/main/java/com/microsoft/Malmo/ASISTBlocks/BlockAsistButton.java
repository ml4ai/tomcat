package com.microsoft.Malmo.ASISTBlocks;

import edu.arizona.tomcat.Utils.DiscreteEventsHelper;
import javax.annotation.Nullable;
import net.minecraft.block.BlockButton;
import net.minecraft.block.state.IBlockState;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.init.SoundEvents;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.EnumHand;
import net.minecraft.util.SoundCategory;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

/**
 * This class defines a new basic button that can write event observations
 * whenever it is pressed by the player.
 * <p>
 * The block is a button, so it
 * contains special methods that define boundaries such that the player must
 * press within the boundaries to achieve the effect of "pressing" the button.
 * The button may show up as a full cube, but only a part of it can be pressed.
 * This can be made more clear through the use of appropriate textures.
 * <p>
 * Technically buttons and doors aren't related, but checking the doors when we
 * press the button helps us avoid polling for the door at every tick.
 */
public class BlockAsistButton extends BlockButton {

  public BlockAsistButton() {
    super(false); // Stone button. An unimportant detail necessary for the
    // superclass constructor.

    setUnlocalizedName("ASIST_Button");
    setRegistryName(
        "ASIST_Button"); // The name Minecraft sees. Also used in en_US.lang

    this.setDefaultState(this.blockState.getBaseState()
                             .withProperty(FACING, EnumFacing.NORTH)
                             .withProperty(POWERED, Boolean.valueOf(false)));
    this.setTickRandomly(true);
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

    DiscreteEventsHelper.printEventOccurrence(
        pos, playerIn, "Button Pressed"); // Used to mark discrete occurence

    return result;
  }

  // The methods below this simply set some button properties

  protected void
  playClickSound(@Nullable EntityPlayer player, World worldIn, BlockPos pos) {
    worldIn.playSound(player,
                      pos,
                      SoundEvents.BLOCK_STONE_BUTTON_CLICK_ON,
                      SoundCategory.BLOCKS,
                      0.3F,
                      0.6F);
  }

  protected void playReleaseSound(World worldIn, BlockPos pos) {
    worldIn.playSound(null,
                      pos,
                      SoundEvents.BLOCK_STONE_BUTTON_CLICK_OFF,
                      SoundCategory.BLOCKS,
                      0.3F,
                      0.5F);
  }
}