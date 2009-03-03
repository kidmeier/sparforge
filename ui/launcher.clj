(ns ui.launcher
  (:import (java.awt Frame)
	   (javax.media.opengl GL GLAutoDrawable GLCanvas GLEventListener)
	   (com.sun.opengl.util FPSAnimator)))

(defn open-frame 
  [#^String title 
   #^GLEventListener gl-ev-listener]
  
  (let [frame (new Frame title),
	canvas (new GLCanvas)]
 
    (.addGLEventListener canvas gl-ev-listener)

    (.add frame canvas)
    (.pack frame)
    (.setSize frame 800 600)
    (.setVisible frame true)

    [frame canvas]))
