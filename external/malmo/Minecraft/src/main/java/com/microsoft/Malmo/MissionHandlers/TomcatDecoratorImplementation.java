package com.microsoft.Malmo.MissionHandlers;

import com.microsoft.Malmo.MissionHandlerInterfaces.IWorldDecorator;
import com.microsoft.Malmo.Schemas.MissionInit;
import com.microsoft.Malmo.Schemas.TomcatDecorator;
import edu.arizona.tomcat.Events.ForgeEventHandler;
import edu.arizona.tomcat.Mission.MissionHandler;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import net.minecraft.client.Minecraft;
import net.minecraft.world.World;

/**
 * WorldBuilder for the ToMCAT missions.
 */
public class TomcatDecoratorImplementation
    extends HandlerBase implements IWorldDecorator {
    TomcatDecorator decorator;
    MissionHandler missionHandler;

    private ForgeEventHandler eventHandler = ForgeEventHandler.getInstance();

    @Override
    public boolean parseParameters(Object params) {
        if (params == null || !(params instanceof TomcatDecorator)) {
            return false;
        }

        this.decorator = (TomcatDecorator)params;
        this.initMissionHandler();

        return true;
    }

    /**
     * Initialize the mission handler object
     */
    private void initMissionHandler() {
        this.missionHandler = new MissionHandler();
        this.missionHandler.setMission(
            this.decorator.getMission().intValue(),
            this.decorator.getTimeLimitInSeconds().intValue(),
            this.decorator.getSelfReportPromptTimeInSeconds().intValue(),
            this.decorator.getLevelOfDifficulty().intValue());

    }

    @Override
    public void buildOnWorld(MissionInit missionInit, World world) {
        this.missionHandler.initMission(world);
    }

    @Override
    public void update(World world) {
        this.eventHandler.updateExtraEvents();
        this.missionHandler.updateMission(world);
    }

    @Override
    public boolean getExtraAgentHandlersAndData(List<Object> handlers,
                                                Map<String, String> data) {
        return false;
    }

    @Override
    public void prepare(MissionInit missionInit) {}

    @Override
    public void cleanup() {}

    @Override
    public boolean targetedUpdate(String nextAgentName) {
        return false; // Does nothing.
    }

    @Override
    public void getTurnParticipants(ArrayList<String> participants,
                                    ArrayList<Integer> participantSlots) {
        // Does nothing.
    }
}
