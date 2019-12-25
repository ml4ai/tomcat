package com.microsoft.Malmo.MissionHandlers;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.microsoft.Malmo.MalmoMod;
import com.microsoft.Malmo.MissionHandlerInterfaces.IWorldDecorator;
import com.microsoft.Malmo.Schemas.MissionInit;
import com.microsoft.Malmo.Schemas.TomcatDecorator;

import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessage;
import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessageHandler;
import edu.arizona.tomcat.Mission.MissionHandler;
import net.minecraft.world.World;
import net.minecraftforge.fml.relauncher.Side;

/**
 * WorldBuilder for the ToMCAT missions.
 */
public class TomcatDecoratorImplementation
extends HandlerBase implements IWorldDecorator {
	TomcatDecorator decorator;
	MissionHandler missionHandler;

	@Override
	public boolean parseParameters(Object params) {
		if (params == null || !(params instanceof TomcatDecorator)) {
			return false;
		}


		this.decorator = (TomcatDecorator) params;
		this.initMissionHandler();

		return true;
	}

	/**
	 * Initialize the mission handler object
	 */
	private void initMissionHandler() {		
		MalmoMod.network.registerMessage(TomcatMessageHandler.class, TomcatMessage.class, 100, Side.CLIENT);
		MalmoMod.network.registerMessage(TomcatMessageHandler.class, TomcatMessage.class, 101, Side.SERVER);
		this.missionHandler = new MissionHandler();
		this.missionHandler.setMission(this.decorator.getMission().intValue(), 
				this.decorator.getTimeLimitInSeconds().intValue(),
				this.decorator.getSelfReportPromptTimeInSeconds().intValue());				
	}

	@Override
	public void buildOnWorld(MissionInit missionInit, World world) {
		this.missionHandler.initMission(world);
	}

	@Override
	public void update(World world) {
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
