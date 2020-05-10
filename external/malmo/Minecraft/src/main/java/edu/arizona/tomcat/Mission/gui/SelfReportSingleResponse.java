package edu.arizona.tomcat.Mission.gui;

public class SelfReportSingleResponse {

    private int selectedChoice;
    private float duration;

    public SelfReportSingleResponse(int selectedChoice, float duration) {
        this.selectedChoice = selectedChoice;
        this.duration = duration;
    }

    /**
     * Retrieves the ID of the choice selected by the player
     */
    public int getSelectedChoice() { return selectedChoice; }

    /**
     * Retrieves the number of seconds the player took to answer the question
     */
    public float getDuration() { return duration; }
}
