(ns net.cgrand.url-encoded)

(defn- escape [#^String s]
  (java.net.URLEncoder/encode s "UTF-8"))

(defn- key-str [k]
  (escape (if (instance? clojure.lang.Named k)
            (name k)
            (str k))))

(defn- flatten-map
 "Take a nested map (or a nested collection of key-value pairs) and returns a
  sequence of key-value pairs where keys are replaced by the result of calling 
  (reduce f pk path) where path is the path to this key through the nested
  maps."  
 ([f kvs] (flatten-map f nil kvs))
 ([f pk kvs]
   (mapcat (fn [[k v]]
             (if (map? v)
               (flatten-map f (f pk k) v)
               [[(f pk k) v]])) kvs)))

(defn- square-bracketize [pk k]
  (if pk
    (str pk "[" (key-str k) "]")
    (key-str k)))

(defn encode [kvs]
  (apply str (interpose "&" (map (fn [[k v]] (str k "=" (escape (str v)))) (flatten-map square-bracketize kvs)))))

(defn- unescape [#^String s]
  (java.net.URLDecoder/decode s "UTF-8"))

(defn segment-on-brackets [#^String k]
 "Returns a seq of string from a bracketed path.
  Example:
    \"fred[ethel][lucy]\" => (\"fred\" \"ethel\" \"lucy\")"    
  (.split #"[\[\]]+" k))

(defn segment-on-dots [#^String k] 
 "Returns a seq of string from a dot-separated path.
  Example:
    \"fred.ethel.lucy\" => (\"fred\" \"ethel\" \"lucy\")"    
  (.split #"\.+" k))

(defn- decode-kv
 [segment wrap #^String kv default]
  (let [[#^String k v] (.split kv "=" 2)]
    [(map wrap (-> k unescape segment))
     (if v (unescape v) default)]))
    
(defn decode
 "Decode a www-url-encoded query string and returns a map.
  By default, keys in the resulting map are keywords, bracketed keys such as
  key1[key2] yields nested maps.
  If a key has no value (no \\= before the next \\&) it defaults to nil.
  
  Options are key-value pairs and may be one of:
    :default the value to be associated to keys without value, defaults to nil. 
    :wrap    the function that make the actual keys from a string, defaults to
             clojure.core/keyword.
    :segment the function that breaks a key into a path (so, to disable 
             nesting, pass clojure.core/list here), defaults to 
             segment-on-brackets"
   
 [#^String s & opts]
  (let [{:keys [segment default wrap] :or {segment segment-on-brackets default nil wrap keyword}}
          (apply array-map opts)]
    (reduce #(apply assoc-in %1 %2) 
      {} (map #(decode-kv segment wrap % default) (remove #(.isEmpty #^String %) (.split s "&"))))))