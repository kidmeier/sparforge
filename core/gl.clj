(ns core.gl
  (:use core.util)
  (:import (javax.media.opengl GL GLAutoDrawable GLCapabilities GLCanvas 
			       DebugGL TraceGL)))

(def *gl* nil)

(defn create-canvas [& capabilities]
  "Create a GLCanvas with the requested capabilities. The capabilities are
   expected to be keyword - value pairs where the keywords correspond to JavaBean
   property names on the GLCapabilities class. The attributes are applied to a
   GLCapabilties instance which is used when requesting the GLCanvas.

   Returns the GLCanvas object created."
  (let [caps (unbean GLCapabilities (apply hash-map capabilities)),
	#^GLCanvas canvas (GLCanvas. caps)]
    canvas))

(defmacro bind-gl [#^GLAutoDrawable drawable & body]
  `(binding [*gl* (.getGL ~drawable)]
     ~@body))

(defmacro with-gl [& body]
  `(doto *gl*
     ~@body))

(defn trace-gl [#^GLAutoDrawable drawable out]
  (.setGL drawable (new TraceGL (.getGL drawable) out))
  drawable)

(defn debug-gl [#^GLAutoDrawable drawable]
  (.setGL drawable (new DebugGL (.getGL drawable)))
  drawable)

(defmacro with-primitive [prim & body]
  `(doto *gl*
     (.glBegin ~prim)
     ~@body
     (.glEnd)))

(defmacro triangles [& body]
  `(with-primitive GL/GL_TRIANGLES
       ~@body))
