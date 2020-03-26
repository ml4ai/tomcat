package edu.arizona.tomcat.Mission.gui;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class RichContentPage {

  private ArrayList<String> text;
  private ArrayList<RichContentImage> images;

  /**
   * Constructor
   * @param text - Rows of text of a rich content
   */
  public RichContentPage(List<String> text) {
    this.text = new ArrayList<String>(text);
    this.images = new ArrayList<RichContentImage>();
  }

  /**
   * Constructor
   * @param text - Rows of text of a rich content
   * @param images - Images of a rich content
   */
  public RichContentPage(List<String> text, List<RichContentImage> images) {
    this.text = new ArrayList<String>(text);
    this.images = new ArrayList<RichContentImage>(images);
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
    for (RichContentImage image : this.images) {
      image.setCaptionPlaceholder(placeholderIndex, text);
    }
  }

  /**
   * Retrieves the sum of the widths of all the images in the page
   * @return
   */
  public int getSumOfImagesWidth() {
    int sum = 0;
    for (RichContentImage image : this.images) {
      sum += image.getWidth();
    }
    return sum;
  }

  /**
   * Retrieves the number of images in the page of a rich content
   * @return
   */
  public int getNumberOfImages() { return this.images.size(); }

  /**
   * Retrieves the max image height among all the images in the page
   * @return
   */
  public int getMaxImageHeight() {
    int maxHeight = Integer.MIN_VALUE;
    for (RichContentImage image : this.images) {
      if (image.getHeight() > maxHeight) {
        maxHeight = image.getHeight();
      }
    }
    return maxHeight;
  }

  /**
   * Retrieves the text
   * @return
   */
  public Iterator<String> getText() { return this.text.iterator(); }

  /**
   * Retrieves the images
   * @return
   */
  public Iterator<RichContentImage> getImages() {
    return this.images.iterator();
  }
}
