(ns net.cgrand.dispatch.ring
  (:require [ring.jetty])
  (:require [net.cgrand.dispatch :as d]))

(defn dispatcher [req]
  (when-let [handler (d/dispatch-url (:uri req))]
    (handler req)))
    
(comment
(def server-thread
  (doto (Thread. #(ring.jetty/run {:port 8080} dispatcher))
    .start))

)