(ns ui.gl
  (:import (javax.media.opengl GL DebugGL TraceGL)))

(def *gl* nil)

(defmacro bind-gl [drawable & body]
  `(binding [*gl* (.getGL ~drawable)]
     ~@body))

(defmacro with-gl [& body]
  `(doto *gl*
     ~@body))

(defmacro with-primitive [prim & body]
  `(doto *gl*
     (.glBegin ~prim)
     ~@body
     (.glEnd)))

(defmacro triangles [& body]
  `(with-primitive GL/GL_TRIANGLES
       ~@body))

(defn trace-gl [drawable out]
  (.setGL drawable (new TraceGL (.getGL drawable) out)))

(defn debug-gl [drawable]
  (.setGL drawable (new DebugGL (.getGL drawable))))
	   
