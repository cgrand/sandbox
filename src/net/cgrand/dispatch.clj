;   Copyright (c) Christophe Grand, 2009. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns net.cgrand.dispatch
  (:use [clojure.contrib.test-is :as test-is :only [set-test with-test is]]))

(defn add-route [root path handler]
  (assoc-in root (concat path [:handler]) handler))
  
(defn remove-route [root path]
  (add-route root path nil))

;; default root for the mount/unmount macros and dispatch fn
(def *root* (ref {}))

(defn- step-dispatch [roots segment]
  (mapcat #(when %1
             (map %1 [segment :else])) roots))

(defn dispatch
 "Returns the handler for the specified path (segments) or nil."
 ([segments] (dispatch @*root* segments))
 ([root segments] 
   (when-let [handler (first 
                        (drop-while nil? 
                          (map :handler 
                            (reduce step-dispatch [root] segments))))]
     (handler segments))))
     
(defn dispatch-url
 [#^String location]
  (dispatch (map #(java.net.URLDecoder/decode % "UTF-8") (rest (.split location "/")))))

;; macros
(defmacro mount 
 "Adds a route (a vector of strings and symbols) to the specified root
  (defaults to *root*)."
 ([route form] `(mount *root* ~route ~form))
 ([root route form]
   (let [_ (gensym "_")
         args (map #(if (string? %) _ %) route)
         segments (map #(if (string? %) % :else) route)
         f `(fn [[~@args]] ~form)]
     `(commute ~root add-route [~@segments] ~f))))
       
(defmacro unmount 
 ([route form] `(unmount *root* ~route ~form))
 ([root route form]
  (let [segments (map #(if (string? %) % :else) route)]
    `(commute ~root remove-route [~@segments]))))
    
