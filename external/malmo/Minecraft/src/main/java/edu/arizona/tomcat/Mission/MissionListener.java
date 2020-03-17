package edu.arizona.tomcat.Mission;

public interface MissionListener {

  /**
   * Notifies listeners when a mission ends
   */
  public void missionEnded(String exitCode);
}
