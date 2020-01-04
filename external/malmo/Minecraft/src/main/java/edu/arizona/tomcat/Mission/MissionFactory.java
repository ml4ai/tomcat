package edu.arizona.tomcat.Mission;

public class MissionFactory {
	
	public static final int TUTORIAL = 0;
	public static final int SEARCH_AND_RESCUE = 1;
	public static final int ITEM_CRAFTING = 2;
	public static final int ROOM_ESCAPE = 3;
	
	public static Mission create(int missionID) {
		Mission mission = null;
		
		switch (missionID) {
		case TUTORIAL:
			mission = new TutorialMission();
			break;
			
		case SEARCH_AND_RESCUE:
			mission = new SARMission();
			break;
			
		case ITEM_CRAFTING:
			mission = new SARMission();
			break;
			
		case ROOM_ESCAPE:
			mission = new SARMission();
			break;

		default:
			break;
		}
		
		return mission;
	}

}
