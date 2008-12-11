(ns ui.gl
  (:import (javax.media.opengl GL DebugGL TraceGL)))

(defmacro with-gl [gl & body]
  `(doto ~gl
     ~@body))

(defmacro with-primitive [prim gl & body]
  `(doto ~gl
     (glBegin ~prim)
     ~@body
     (glEnd)))

(defmacro triangles [gl & body]
  `(with-primitive GL/GL_TRIANGLES
       ~gl
     ~@body))

(defn trace-gl [drawable out]
  (.setGL drawable (new TraceGL (.getGL drawable) out)))

(defn debug-gl [drawable]
  (.setGL drawable (new DebugGL (.getGL drawable))))
	   
