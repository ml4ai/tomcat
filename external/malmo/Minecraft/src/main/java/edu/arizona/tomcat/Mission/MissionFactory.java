package edu.arizona.tomcat.Mission;

import edu.arizona.tomcat.Mission.Client.*;

public class MissionFactory {

    public static Mission create(int missionID) {
        Mission mission = null;

        switch (Mission.ID.values()[missionID]) {
        case TUTORIAL:
            mission = new TutorialMission();
            break;

        case ZOMBIE:
            mission = new ZombieMission();
            break;

        case USAR_SINGLE_PLAYER:
            mission = new USARSinglePlayerMission();
            break;

        case PROCEDURAL:
            mission = new ProceduralGenMission();
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

        case ZOMBIE:
            clientMission = new ZombieClientMission();
            break;

        case USAR_SINGLE_PLAYER:
            clientMission = new USARSinglePlayerClientMission();
            break;

        case PROCEDURAL:
            clientMission = new ProceduralGenClientMission();
            break;

        default:
            break;
        }

        return clientMission;
    }
}
