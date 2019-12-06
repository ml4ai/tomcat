package edu.arizona.tomcat.Mission;

import com.microsoft.Malmo.MissionHandlerInterfaces.IWantToQuit;
import com.microsoft.Malmo.MissionHandlers.HandlerBase;
import com.microsoft.Malmo.Schemas.MissionInit;

public class QuitProducer extends HandlerBase implements IWantToQuit, MissionListener {
	
	private boolean hasMissionEnded;
	private String exitCode;
	
	/**
	 * Constructor
	 */
	public QuitProducer() {
		this.hasMissionEnded = false;
	}

	@Override
	public boolean doIWantToQuit(MissionInit missionInit) {
		return this.hasMissionEnded;
	}

	@Override
	public void prepare(MissionInit missionInit) { }

	@Override
	public void cleanup() {	}

	@Override
	public String getOutcome() {
		return this.exitCode;
	}
	
	@Override
	public void missionEnded(String exitCode) {
		this.hasMissionEnded = true;
		this.exitCode = exitCode;
	}

}
