package edu.arizona.tomcat.Mission.gui;

import com.google.gson.Gson;
import com.microsoft.Malmo.MalmoMod;
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

public class SelfReportContent {

  private String id;
  private String filenameRichContentBeforeQuestions;
  private String filenameRichContentAfterQuestions;
  private List<SelfReportQuestion> questions;
  private float durationInSeconds;
  private RichContent richContentBeforeQuestions;
  private RichContent richContentAfterQuestions;

  /**
   * Constructor
   */
  public SelfReportContent(String id) {
    this.id = id;
    this.questions = new ArrayList<SelfReportQuestion>();
    this.filenameRichContentBeforeQuestions = null;
    this.filenameRichContentAfterQuestions = null;
  }

  /**
   * Add a new question to a self-report
   * @param page
   */
  public void addQuestion(String id, SelfReportQuestion question) {
    this.id = id;
    this.questions.add(question);
  }

  /**
   * Creates a self-report object from a .json file
   * @param json - Json file with self-report questions
   * @return
   */
  public static SelfReportContent createFromJson(String id, String filename) {
    SelfReportContent content = null;
    String path = "assets/" + MalmoMod.MODID + "/self_report/" + filename;
    InputStream inputStream =
        MalmoMod.instance.getClass().getClassLoader().getResourceAsStream(path);
    BufferedReader reader =
        new BufferedReader(new InputStreamReader(inputStream));
    Gson gson = new Gson();
    content = gson.fromJson(reader, SelfReportContent.class);
    content.id = id;
    if (content.filenameRichContentBeforeQuestions != null &&
        !content.filenameRichContentBeforeQuestions.equals("")) {
      content.richContentBeforeQuestions = RichContent.createFromJson(
          content.filenameRichContentBeforeQuestions);
    }
    if (content.filenameRichContentAfterQuestions != null &&
        !content.filenameRichContentAfterQuestions.equals("")) {
      content.richContentAfterQuestions =
          RichContent.createFromJson(content.filenameRichContentAfterQuestions);
    }
    return content;
  }

  /**
   * Replace a placeholder in the text by a string
   * @param placeholderIndex - Index of the placeholder in the text
   * @param text - Text to be replaced
   */
  public void setTextPlaceholder(int placeholderIndex, String text) {
    for (SelfReportQuestion question : this.questions) {
      question.setTextPlaceholder(placeholderIndex, text);
    }
    if (this.richContentBeforeQuestions != null) {
      this.richContentBeforeQuestions.setTextPlaceholder(placeholderIndex,
                                                         text);
    }
    if (this.richContentAfterQuestions != null) {
      this.richContentAfterQuestions.setTextPlaceholder(placeholderIndex, text);
    }
  }

  /**
   * Retrieves a question by index
   * @param question - Self-report question
   * @return
   */
  public SelfReportQuestion getQuestion(int question) {
    return this.questions.get(question);
  }

  /**
   * Retrieves the number of questions of a self-report
   * @return
   */
  public int getNumberOfQuestions() { return this.questions.size(); }

  /**
   * Defines the number of seconds the player took to complete the self-report
   * @param durationInSeconds - duration in seconds
   */
  public void setDurationInSeconds(float durationInSeconds) {
    this.durationInSeconds = durationInSeconds;
  }

  /**
   * Gets the ID of the self-report
   * @return
   */
  public String getId() { return id; }

  /**
   * Gets the number of seconds the player took to complete the self-report
   * @return
   */
  public float getDurationInSeconds() { return durationInSeconds; }

  /**
   * Gets rich content that must be shown before the questions
   * @return
   */
  public RichContent getRichContentBeforeQuestions() {
    return this.richContentBeforeQuestions;
  }

  /**
   * Gets rich content that must be shown after the questions
   * @return
   */
  public RichContent getRichContentAfterQuestions() {
    return this.richContentAfterQuestions;
  }
}
