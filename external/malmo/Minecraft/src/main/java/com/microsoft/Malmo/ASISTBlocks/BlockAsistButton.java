package com.microsoft.Malmo.ASISTBlocks;

import com.google.gson.internal.LinkedTreeMap;
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

import javax.annotation.Nullable;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Map;

/**
 * This class defines a new basic button that can write event observations
 * whenever it is pressed by the player. Some deprecated methods are being
 * overridden in this class, but that seems to be necessary for the block to
 * achieve the same functionality as a normal Minecraft button, and also for it
 * to be rendered properly inside the world. <p> The block is a button, so it
 * contains special methods that define boundaries such that the player must
 * press within the boundaries to achieve the effect of "pressing" the button.
 * The button may show up as a full cube, but only a part of it can be pressed.
 * This can be made more clear through the use of appropriate textures.
 */
public class BlockAsistButton extends BlockButton {

  protected BlockAsistButton() {
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

    printEventOccurence(pos, playerIn); // Used to mark discrete occurence

    return result;
  }

  /**
   * When called, this method will print the occurence of a button press to the
   * terminal. The Blockpos passed is the coordinate at which the button was
   * pressed, and the playerIn is the player who pressed the button.
   *
   * @param pos      - Position of button
   * @param playerIn -  The player who pressed the button
   */
  private void printEventOccurence(BlockPos pos, EntityPlayer playerIn) {
    int x = pos.getX(), y = pos.getY(), z = pos.getZ(); // button coordinates
    String coordinates = "X: " + x + " "
                         + "Y: " + y + " "
                         + "Z: " + z;

    DateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
    Date date = new Date();
    String dateTime = dateFormat.format(date); // Date and Time

    Map<String, String> output = new LinkedTreeMap<String, String>();

    output.put("Event Type", "Button Pressed");
    output.put("Event caused by", playerIn.getDisplayNameString());
    output.put("Event Coordinates", coordinates);
    output.put("Occurence Time", dateTime);

    // The output is placed in a map above. The code below is only for temporary
    // printing to terminal.

    System.out.println("+---EVENT REPORT---+");
    for (String key : output.keySet()) {
      System.out.println(key + ": " + output.get(key));
    }
    System.out.println();
    System.out.println();
  }

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