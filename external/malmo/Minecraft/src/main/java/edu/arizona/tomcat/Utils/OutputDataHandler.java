package edu.arizona.tomcat.Utils;

import com.google.gson.FieldNamingPolicy;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

public class OutputDataHandler {

  /**
   * Create folder if it doesn't exist yet
   * @param folderName
   */
  public static void createFolder(String folderName) {
    File folder = new File(folderName);
    if (!folder.exists()) {
      folder.mkdir();
    }
  }

  /**
   * Append the content of a java object to a .json file
   * @param folderName - Output folder
   * @param filename - Name of the file
   * @param jsonJavaObject - Java object with content to be saved as a .json
   * @throws IOException
   */
  public static void
  appendToJsonFile(String folderName, String filename, Object jsonJavaObject)
      throws IOException {
    createFolder(folderName);
    String path = String.format("%s/%s", folderName, filename);
    FileWriter fileWriter = new FileWriter(path, true);
    Gson gson =
        new GsonBuilder()
            .setFieldNamingPolicy(FieldNamingPolicy.LOWER_CASE_WITH_UNDERSCORES)
            .create();
    String json = gson.toJson(jsonJavaObject);
    fileWriter.write(json);
    fileWriter.close();
  }
}
