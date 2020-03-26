package com.microsoft.Malmo.ASISTBlocks;

import com.google.gson.internal.LinkedTreeMap;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.util.math.BlockPos;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Map;

/**
 * This class holds a static helper to print the discrete event. It current prints the output to terminal.
 */
public class DiscreteEventsHelper {

    /**
     * When called, this method will print the occurence of the event to the
     * terminal. The Blockpos passed is the coordinate at which the event
     * was triggered, and the playerIn is the player who triggered the event.
     *
     * @param pos      - Position of event
     * @param playerIn -  The player who triggered the event
     */
    public static void printEventOccurence(BlockPos pos, EntityPlayer playerIn, String event) {
        int x = pos.getX(), y = pos.getY(), z = pos.getZ(); // event coordinates
        String coordinates = "X: " + x + " "
                + "Y: " + y + " "
                + "Z: " + z;

        DateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
        Date date = new Date();
        String dateTime = dateFormat.format(date); // Date and Time

        Map<String, String> output = new LinkedTreeMap<String, String>();

        output.put("Event Type", event);
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
}
