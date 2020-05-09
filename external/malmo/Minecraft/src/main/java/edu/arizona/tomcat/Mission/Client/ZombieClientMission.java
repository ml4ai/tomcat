package edu.arizona.tomcat.Mission.Client;

import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessage;
import edu.arizona.tomcat.Mission.ZombieMission;
import edu.arizona.tomcat.Mission.gui.GUIOverlayVillagersSaved;
import net.minecraft.client.Minecraft;
import net.minecraft.init.SoundEvents;

public class ZombieClientMission extends ClientMission {

    private int numberOfSavedVillagers;

    public ZombieClientMission() {
        super();
        // Sets indicator of number of villagers saved to be rerendered by a
        // Minecraft event schema.
        GUIOverlayVillagersSaved.register(ZombieMission.NUMBER_OF_VILLAGERS);
    }

    @Override
    public void handleMessageFromServer(TomcatMessage message) {
        super.handleMessageFromServer(message);

        switch (message.getMessageType()) {
        case VILLAGER_SAVED:
            this.numberOfSavedVillagers++;
            // The Thank you sound is in the Pig Death event because there are
            // no pigs in the game, otherwise the sound could not be played if
            // there's a conflict with a sound that is already being played in
            // the game.
            Minecraft.getMinecraft().player.playSound(
                SoundEvents.ENTITY_PIG_DEATH, 1, 1);
            break;

        default:
            break;
        }
    }

    /**
     * Retrieves the total number of villagers saved by the player
     * @param numberOfSavedVillagers - Total number of rescued villagers
     */
    public int getNumberOfSavedVillagers() {
        return this.numberOfSavedVillagers;
    }

    @Override
    public void cleanup() {
        GUIOverlayVillagersSaved.unregister();
    }
}
