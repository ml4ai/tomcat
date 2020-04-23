package edu.arizona.tomcat.World;

import com.microsoft.Malmo.Schemas.DrawObjectType;
import java.util.ArrayList;
import java.util.Iterator;

public abstract class CompositeDrawingObject {

    protected ArrayList<DrawObjectType> malmoDrawObjects;

    /**
     * Constructor
     */
    public CompositeDrawingObject() {
        this.malmoDrawObjects = new ArrayList<DrawObjectType>();
    }

    /**
     * Retrieves the list of Malmo draw objects that form this drawing object
     * @return
     */
    public Iterator<DrawObjectType> getMalmoDrawObjects() {
        return malmoDrawObjects.iterator();
    }

    /**
     * Merge malmo objects from one drawing with another
     * @param drawingObject - Drawing Object
     */
    public void mergeWith(CompositeDrawingObject drawingObject) {
        Iterator<DrawObjectType> alienMalmoDrawObjects =
            drawingObject.getMalmoDrawObjects();
        while (alienMalmoDrawObjects.hasNext()) {
            this.malmoDrawObjects.add(alienMalmoDrawObjects.next());
        }
    }
}
