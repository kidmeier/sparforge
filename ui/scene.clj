(ns ui.scene
  (:use core.prototype))

(defproto group thing
  :children [])

(defproto leaf thing)

(defproto scene thing
  :camera (free-camera),
  :children [])

;; Scene graph functions/traversals
(defmulti render proto)
(defmulti intersect (fn [node _] (proto node)))

(defn create
  "Create a new scene graph. A scene is currently implemented as a 
   ref to a map using the prototype pattern."
  (ref (clone scene)))

(defn bind-scene 
  "Create a new scene reference bound to the given canvas"
  [sc canvas]

  (add-watcher sc :send canvas render))

(defn unbind-scene
  "Disassociate the given scene from the canvas"
  [sc canvas]
  
  (remove-watcher sc canvas))

(defmethod render :default 
  [node] 
  
  (println "ui.scene:render :default")
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
