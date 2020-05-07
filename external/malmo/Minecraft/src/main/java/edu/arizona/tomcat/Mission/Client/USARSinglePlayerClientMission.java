package edu.arizona.tomcat.Mission.Client;

import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessage;

public class USARSinglePlayerClientMission extends ClientMission {

    public USARSinglePlayerClientMission() { super(); }

    @Override
    public void handleMessageFromServer(TomcatMessage message) {
        super.handleMessageFromServer(message);
    }

	@Override
	public void cleanup() {
				
	}
}
