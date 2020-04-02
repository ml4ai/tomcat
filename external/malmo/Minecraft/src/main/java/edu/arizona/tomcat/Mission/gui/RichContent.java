package edu.arizona.tomcat.Mission.gui;

import com.google.gson.Gson;
import com.microsoft.Malmo.MalmoMod;
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

public class RichContent {

  private List<RichContentPage> pages;

  /**
   * Constructor
   */
  public RichContent() { this.pages = new ArrayList<RichContentPage>(); }

  /**
   * Add a new page to a rich content
   * @param page
   */
  public void addPage(RichContentPage page) { this.pages.add(page); }

  /**
   * Creates a rich content object from a .json file
   * @param json - Json file with rich content
   * @return
   */
  public static RichContent createFromJson(String filename) {
    RichContent content = null;
    String path = "assets/" + MalmoMod.MODID + "/rich_content/" + filename;
    InputStream inputStream =
        MalmoMod.instance.getClass().getClassLoader().getResourceAsStream(path);
    BufferedReader reader =
        new BufferedReader(new InputStreamReader(inputStream));
    Gson gson = new Gson();
    content = gson.fromJson(reader, RichContent.class);
    return content;
  }

  /**
   * Replace a placeholder in the text by a string
   * @param placeholderIndex - Index of the placeholder in the text
   * @param text - Text to be replaced
   */
  public void setTextPlaceholder(int placeholderIndex, String text) {
    for (RichContentPage page : this.pages) {
      page.setTextPlaceholder(placeholderIndex, text);
    }
  }

  /**
   * Retrieves a page by index
   * @param page - Rich content page
   * @return
   */
  public RichContentPage getPage(int page) { return this.pages.get(page); }

  /**
   * Retrieves the number of pages of a rich content
   * @return
   */
  public int getNumberOfPages() { return this.pages.size(); }
}
