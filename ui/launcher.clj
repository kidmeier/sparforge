(ns ui.launcher
  (:import (java.awt Frame)
	   (javax.media.opengl GL GLAutoDrawable GLCanvas GLEventListener)
	   (com.sun.opengl.util FPSAnimator)))

(defn- open-frame 
  [#^String title 
   #^GLEventListener event-listener]
  
  (let [frame (new Frame title),
	canvas (new GLCanvas)]
 
    (.addGLEventListener canvas event-listener)

    (.add frame canvas)
    (.pack frame)
    (.setSize frame 800 600)
    (.setVisible frame true)

    [frame canvas]))

(defn launch-game [title event-listener]
  (let [[frame canvas] (open-frame title event-listener),
	animator (new FPSAnimator canvas 30)]
    (.start animator)
    animator))

