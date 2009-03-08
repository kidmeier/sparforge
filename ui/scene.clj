(ns ui.scene
  (:use core.gl core.prototype core.event)
  (:import (javax.media.opengl GLCanvas GLEventListener)
	   (java.awt.event KeyListener MouseListener MouseMotionListener
			   MouseWheelListener)))

(defproto group nil
  :children [])

(defproto leaf nil)

(defproto scene group
  :canvas nil,
  :root nil)

;; Scene graph functions/traversals
(defmulti render proto)
(defmulti reshape (fn [node x y width height] (proto node)))
(defmulti intersect (fn [node _] (proto node)))

(defn create-scene
  "Create a new scene graph. A scene is currently implemented as a 
   ref to a map using the prototype pattern."
  []

  (ref (clone scene)))

(defn bind-scene 
  "Create a new scene reference bound to the given canvas"
  [sc #^GLCanvas canvas scene-update-hook & event-flags]

  ;; Ensure that the canvas is not already bound
  (let [listeners (.getListeners canvas GLEventListener)]
    (if (some #(identical? canvas %1) (seq listeners))
      (throw (IllegalArgumentException. 
	      (str "Canvas is already bound: " canvas)))))

  (let [gl-listener (proxy [GLEventListener] []
		      (init [drw]
			    
			    (println "gl-handler:init")
			    (bind-gl drw
				     (with-gl 
				      (.glClearColor 0 0 0 1))))
		      
		      ;; Called when the GL canvas is resized
		      (reshape
		       [drw x y width height]
		       
		       (println "gl-handler:reshape " x y width height)
		       (bind-gl drw
			 (reshape sc x y width height)))
		      
		      ;; Called when the display mode changes
		      (displayChanged 
		       [drw modeChanged deviceChanged]
		       
		       (println "gl-handler:displayChanged"))
		      
		      ;; Called when the display needs to be updated (rendered)
		      (display
		       [drw]
		       
		       (println "gl-handler:display")
		       (bind-gl drw 
			 (render sc)))),

	key-handler (proxy [KeyListener] []
		      (keyPressed [ev]
				  (println "keyPressed: " ev)
				  (key-down sc ev))
		      
		      (keyReleased [ev]
				   (println "keyReleased: " ev)
				   (key-up sc ev))
		      
		      (keyTyped [ev]
				(println "keyTyped: " ev)
				(key-typed sc ev))),

	mouse-handler (proxy [MouseListener] []
			(mouseClicked [ev]
				      (println "mouseClicked: " ev)
				      (mouse-clicked sc ev))
			
			(mouseEntered [ev]
				      (println "mouseEntered: " ev)
				      (mouse-entered sc ev))
			
			(mouseExited [ev]
				     (println "mouseExited: " ev)
				     (mouse-exited sc ev))
			
			(mousePressed [ev]
				      (println "mousePressed: " ev)
				      (mouse-down sc ev))
			
			(mouseReleased [ev]
				       (println "mouseReleased: " ev)
				       (mouse-up sc ev))),

	mouse-motion-handler (proxy [MouseMotionListener] []

			       (mouseDragged [ev]
					     (println "mouseDragged: " ev)
					     (mouse-dragged sc ev))

			       (mouseMoved [ev]
					   (println "mouseMoved: " ev)
					   (mouse-moved sc ev))),

	mouse-wheel-handler (proxy [MouseWheelListener] []
			      
			      (mouseWheelMoved [ev]
					       (println "mouseWheelMoved: " ev)
					       (mouse-wheel-moved sc ev)))]
    
    ;; Register listeners on the canvas
    (let [{:keys [keys mouse mouse-moves mouse-wheel]} 
	  (reduce #(assoc %1 %2 true) {} event-flags)]

      (when keys
	(.addKeyListener canvas key-handler))
      (when mouse
	(.addMouseListener canvas mouse-handler))
      (when mouse-moves
	(.addMouseMotionListener canvas mouse-motion-handler))
      (when mouse-wheel
	(.addMouseWheelListener canvas mouse-wheel-handler))

      (.addGLEventListener canvas gl-listener)
    
      ;; Bind changes to the scene to the render call-back
      (add-watch sc canvas scene-update-hook))))

(defn unbind-scene
  "Disassociate the given scene from the canvas"
  [sc canvas]

  ;; Stop watching the scene reference
  (remove-watch sc canvas)

  ;; Remove listeners registered on the canvas
  )

(defmethod render :default 
  [node] 
  
  (println "ui.scene:render: " node))

(defmethod reshape :default
  [node x y width height]

  (println "ui:reshape: " x y width height)
  (with-gl 
    (.glMatrixMode GL/GL_PROJECTION)
    (.glLoadIdentity)
    (.glOrtho -1 1 -1 1 1 100)))

(comment
  (let [points [[-1 -1 -1]
		[ 0  1  -1]
		[ 1 -1 -1]]]
 
    (with-gl
      (.glClear GL/GL_COLOR_BUFFER_BIT)
      (.glColor4f 1 1 1 1))
    
    (triangles
      (.glColor3f 1 0 0)
      (.glVertex3f ((points 0) 0) ((points 0) 1) ((points 0) 2))
      
      (.glColor3f 0 1 0)
      (.glVertex3f ((points 1) 0) ((points 1) 1) ((points 1) 2))
      
      (.glColor3f 0 0 1)
      (.glVertex3f ((points 2) 0) ((points 2) 1) ((points 2) 2)))))

(comment
  (def *sg* 
       (clone group :children 
	      [(clone leaf :value "First child"),
	       (clone leaf :value "Second child"),
	       (clone container,
		      :children 
		      [(clone leaf :value "First grandchild"),
		       (clone leaf :value "Second grandchild")]),
	       (clone leaf :value "Fourth child")]))
  )
