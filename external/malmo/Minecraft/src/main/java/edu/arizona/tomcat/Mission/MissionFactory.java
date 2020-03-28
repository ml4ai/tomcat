package edu.arizona.tomcat.Mission;

import edu.arizona.tomcat.Mission.Client.ClientMission;
import edu.arizona.tomcat.Mission.Client.SARClientMission;
import edu.arizona.tomcat.Mission.Client.TutorialClientMission;

public class MissionFactory {

	public static Mission create(int missionID) {
		Mission mission = null;
		
		switch (Mission.ID.values()[missionID]) {
		case TUTORIAL:
			mission = new TutorialMission();
			break;

		case SEARCH_AND_RESCUE:
			mission = new SARMission();
			break;
			
		default:
			break;
		}

		return mission;
	}

	public static ClientMission createClient(int missionID) {
		ClientMission clientMission = null;

		switch (Mission.ID.values()[missionID]) {
		case TUTORIAL:
			clientMission = new TutorialClientMission();
			break;

		case SEARCH_AND_RESCUE:
			clientMission = new SARClientMission();
			break;

		default:
			break;
		}

		return clientMission;
	}
}
