package edu.arizona.tomcat.Mission.Client;

import edu.arizona.tomcat.Messaging.TomcatMessaging;

public class ProceduralGenClientMission extends ClientMission {

    public ProceduralGenClientMission() { super(); }

    @Override
    public void handleMessageFromServer(TomcatMessaging.TomcatMessage message) {
        super.handleMessageFromServer(message);
    }

    @Override
    public void cleanup() {}
}
