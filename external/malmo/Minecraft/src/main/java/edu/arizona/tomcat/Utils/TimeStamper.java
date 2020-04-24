package edu.arizona.tomcat.Utils;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

public class TimeStamper {

	/**
	 * Returns the current UTC time in ISO-8601 extended format
	 */
	public static String getTimeStamp() {
		TimeZone timeZone = TimeZone.getTimeZone("UTC");
		DateFormat dateFormat = new SimpleDateFormat(
				"yyyy-MM-dd'T'HH:mm:ss:SSS'Z'"); // Quoted "Z" to indicate UTC, no
		// timezone offset
		dateFormat.setTimeZone(timeZone);
		return dateFormat.format(new Date());
	}
}
