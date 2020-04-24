package edu.arizona.tomcat.Mission.gui;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class SelfReportQuestion {

    private ArrayList<String> text;
    private ArrayList<String> choices;
    private SelfReportSingleResponse response;

    /**
     * Constructor
     * @param text - Rows of text of a question
     */
    public SelfReportQuestion(List<String> text, List<String> choices) {
        this.text = new ArrayList<String>(text);
        this.choices = new ArrayList<String>(choices);
    }

    /**
     * Replace a placeholder in the text by a string
     * @param placeholderIndex - Index of the placeholder in the text
     * @param text - Text to be replaced
     */
    public void setTextPlaceholder(int placeholderIndex, String text) {
        String placeholder = "{" + placeholderIndex + "}";
        for (int i = 0; i < this.text.size(); i++) {
            String textLine = this.text.get(i);
            this.text.set(i, textLine.replace(placeholder, text));
        }
    }

    /**
     * Retrieves the number of choices in the question
     * @return
     */
    public int getNumberOfChoices() { return this.choices.size(); }

    /**
     * Retrieves the text
     * @return
     */
    public Iterator<String> getText() { return this.text.iterator(); }

    /**
     * Retrieves the choices
     * @return
     */
    public Iterator<String> getChoices() { return this.choices.iterator(); }

    /**
     * Retrieves the player's response to the question
     * @return
     */
    public SelfReportSingleResponse getResponse() { return response; }

    /**
     * Sets player's response to the question
     * @param response
     */
    public void setResponse(SelfReportSingleResponse response) {
        this.response = response;
    }
}
