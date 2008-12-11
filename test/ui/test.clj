(ns ui.test
  (:use ui.gl
	ui.launcher)
  (:import (javax.media.opengl DebugGL TraceGL GL GLEventListener)))

(def points [[-1 -1 -1]
	     [ 0  1 -1]
	     [ 1 -1 -1]])

(launch-game 
 "Test"
 (proxy [GLEventListener] [] 

   (init 
    [drawable] 

    (with-gl (.getGL drawable)
      (glClearColor 0 0 0 1)))
   
   (reshape 
    [drawable, x, y, width, height]

    (with-gl (.getGL drawable)
     
	     (glMatrixMode GL/GL_PROJECTION)
	     (glLoadIdentity)
	     (glOrtho -1 1 -1 1 1 100)))

   (displayChanged 
    [drawable, modeChanged, deviceChanged]
    nil)

   (display 
    [drawable]

    (let [gl (.getGL drawable)]
      (with-gl gl
	       (glClear GL/GL_COLOR_BUFFER_BIT)
	       (glColor4f 1 1 1 1))

      (triangles gl
	(glColor3f 1 0 0)
	(glVertex3f ((points 0) 0) ((points 0) 1) ((points 0) 2))
	
	(glColor3f 0 1 0)
	(glVertex3f ((points 1) 0) ((points 1) 1) ((points 1) 2))

	(glColor3f 0 0 1)
	(glVertex3f ((points 2) 0) ((points 2) 1) ((points 2) 2)))))))
