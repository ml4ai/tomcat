package edu.arizona.tomcat.Mission.gui;

import java.util.ArrayList;
import java.util.List;

public class SelfReportResponses {

    private List<SelfReportSingleResponse> responses;
    private float duration;

    /**
     * Constructor
     */
    public SelfReportResponses() {
        this.responses = new ArrayList<SelfReportSingleResponse>();
    }

    /**
     * Add a new response to a self-report question
     * @param page
     */
    public void addResponse(SelfReportSingleResponse response) {
        this.duration += response.getDuration();
        this.responses.add(response);
    }

    /**
     * Retrieves the number of seconds the player took to answer all the
     * questions
     */
    public float getDuration() { return duration; }
}
