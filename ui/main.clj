(ns ui.main
  (:use core.event core.prototype ui.launcher ui.gl)
  (:import (javax.media.opengl DebugGL TraceGL GL GLEventListener)
	   (java.awt.event KeyListener MouseListener MouseMotionListener
			   MouseWheelListener))
  (:gen-class))

(def *frame* nil)
(def *canvas* nil)

(defmulti update :symbol)
(defmethod update :default [node t dt] 
  (println "ui:update"))

(defmulti reshape (fn [node x y width height] (proto node)))
(defmethod reshape :default
  [node x y width height]

  (println "ui:reshape: " x y width height)
  (with-gl 
    (.glMatrixMode GL/GL_PROJECTION)
    (.glLoadIdentity)
    (.glOrtho -1 1 -1 1 1 100)))

(def gl-handler 
     (proxy [GLEventListener] []
       (init [dr]
	 
	 (println "gl-handler:init")
	 (bind-gl dr
	   (with-gl 
	     (.glClearColor 0 0 0 1))))
       
       ;; Called when the GL canvas is resized
       (reshape
	 [drw x y width height]
	 
	 (println "gl-handler:reshape " x y width height)
	 (bind-gl drw
	   (alter-scene reshape x y width height)))
       
       ;; Called when the display mode changes
       (displayChanged 
	 [drw modeChanged deviceChanged]
	 
	 (println "gl-handler:displayChanged"))

       ;; Called when the display needs to be updated.
       (display
	 [drw]

	 (println "gl-handler:display")
	 (bind-gl drw
	   (alter-scene render)))))

(def key-handler
     (proxy [KeyListener] []
       
       (keyPressed [ev]
	 (println "keyPressed: " ev)
	 (alter-scene key-down ev))
       
       (keyReleased [ev]
	 (println "keyReleased: " ev)
	 (alter-scene key-up ev))
       
       (keyTyped [ev]
	 (println "keyTyped: " ev)
	 (alter-scene key-pressed ev))))

(def mouse-handler
     (proxy [MouseListener] []

       (mouseClicked [ev]
	 (println "mouseClicked: " ev)
	 (alter-scene mouse-clicked ev))

       (mouseEntered [ev]
	 (println "mouseEntered: " ev)
	 (alter-scene mouse-entered ev))
       
       (mouseExited [ev]
	 (println "mouseExited: " ev)
       	 (alter-scene mouse-exited ev))

       (mousePressed [ev]
	 (println "mousePressed: " ev)
	 (alter-scene mouse-down ev))

       (mouseReleased [ev]
	 (println "mouseReleased: " ev)
	 (alter-scene mouse-up ev))))

(def mouse-wheel-handler
     (proxy [MouseWheelListener] []
       
       (mouseWheelMoved [ev]
	 (println "mouseWheelMoved: " ev)
	 (alter-scene mouse-wheel-moved ev))))

(defn -main [args]
  (let [[frame canvas] (open-frame "Sparforge" gl-handler)]

    ;; Connect handlers
    (.addKeyListener canvas key-handler)
    (.addMouseListener canvas mouse-handler)
    (.addMouseWheelListener canvas mouse-wheel-handler)
    frame))
