(ns ui.main
  (:use core.gl core.util)
  (:import (java.awt Dimension Frame)
	   (javax.media.opengl GLCanvas))
  (:gen-class))

(defn- open-frame 
  "Open an AWT frame with the given GLCanvas added to it."
  [#^String title
   #^GLCanvas canvas
   & props]
  
  (let [frame (new Frame title)]
 
    (.add frame canvas)

    ;; Apply the given properties to the frame
    (apply-properties frame (apply hash-map props))

    (.pack frame)
    (.setVisible frame true)

    frame))

(defn -main [args]
  (let [canvas (create-canvas :doubleBuffered true,
			      :hardwareAccelerated true),
	frame (open-frame "Sparforge"
			  canvas
			  :preferredSize (Dimension. 800 600),
			  :minimumSize (Dimension. 640 480),
			  :resizable true)]
    frame))
