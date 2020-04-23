package edu.arizona.tomcat.ASISTBlocks;

import edu.arizona.tomcat.Events.IronDoorOpened;
import java.util.List;
import net.minecraft.block.Block;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.Minecraft;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.item.ItemStack;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import edu.arizona.tomcat.Messaging.MqttService;

import java.util.Date;
import java.text.DateFormat;
import java.text.SimpleDateFormat;

/**
 * This block will be used as the "door" block for the Hit-Controlled doors.
 * The block will write its event to the file when it is destroyed.
 * <p>
 * It is set to drop nothing when it is destroyed, hence giving the
 * illusion of vanishing. Replacing this block with the /setblock command
 * inside Minecraft without the "destroy" add-on will result in no output.
 * <p>
 * It will also NOT write any output if the player destroys the block by hand
 * because we don't expect the player to do that.
 */
public class BlockAsistIron extends Block {

  // MQTT service
  private MqttService mqttService = MqttService.getInstance();

  public BlockAsistIron() {

    super(Material.IRON);
    setUnlocalizedName("ASIST_Iron_Block");
    //
    // The name Minecraft sees. Also used in en_US.lang
    setRegistryName("ASIST_Iron_Block"); 

    // Shows up in the redstone tab in creative mode
    this.setCreativeTab(CreativeTabs.REDSTONE); 
  }

  @Override
  /**
   * This returns a complete list of items dropped from this block.
   *
   * @param world The current world
   * @param pos Block position in world
   * @param state Current state
   * @param fortune Breakers fortune level
   * @return An ArrayList containing all items this block drops
   */
  public List<ItemStack>
  getDrops(IBlockAccess world, BlockPos pos, IBlockState state, int fortune) {

    // Technically a command block destroys this, so we aren't identifying a
    // player as destroying this block for the sake of the code.
    this.mqttService.publish(new IronDoorOpened(pos), "observations/events/iron_door_opened");

    return new java.util.ArrayList<ItemStack>(); // Drop nothing
  }
}
