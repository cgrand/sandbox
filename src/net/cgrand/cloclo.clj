(ns net.cgrand.cloclo)

(def *locals* {})
(def *no-eval* {})

(defn constant [x]
  (with-meta (list x) {::constant true}))

(defn constant? [x]
  (-> x meta ::constant))

(defn value [x]
  (if (constant? x) (first x) x))

(declare partial-eval)

(defn partial-call [form]
  (let [boxed-vals (map partial-eval form)
        vals (doall (map value boxed-vals))]
    (if (every? constant? boxed-vals)
      (constant (eval vals))
      vals)))

(defn partial-eval-if* [[_ test then & [else]]]
  (let [test (partial-eval test)]
    (if (constant? test)
      (partial-eval (if (value test) then else))
      `(if* ~test ~@(map (comp value partial-eval) [then else])))))
      
(defn partial-eval-do [[_ & body] ]
  (let [vals (map partial-eval body)
        statements (remove constant? (butlast vals))]
    (if (empty? statements)
      (last vals)
      `(do ~@statements ~(value (last vals))))))
 
(defn partial-eval-let* [[_ bindings & body]]
  (binding [*locals* *locals*]
    (loop [bindings (seq bindings) partial-bindings []]
      (if-let [[sym expr & etc] bindings]
        (let [val (partial-eval expr)]
          (if (constant? val)
            (do
              (set! *locals* (assoc *locals* sym val))
              (recur etc partial-bindings))
            (do
              (set! *locals* (assoc *locals* sym sym))
              (recur etc (conj partial-bindings sym val)))))
        (if (empty? partial-bindings)
          (partial-eval-do `(do ~@body))
          `(let* ~partial-bindings ~@(map (comp value partial-eval) body)))))))
    
(defn partial-eval-seq [form]
  (if-let [[op & args] (seq form)]
    (cond 
      (= op 'if*)
        (partial-eval-if* form)
      (= op 'let*)
        (partial-eval-let* form)
      (= op 'do)
        (partial-eval-do form)
      :else
        (if (special-symbol? op)
          (throw (Exception. (str "unimpleted special symbol " op)))
          (partial-call form)))
    form))

(defn- merge-constants [f args]
  (let [vals (map partial-eval args)]
    (if (every? constant? vals)
      (constant (f (map value vals)))
      (f (map value vals)))))

(defn partial-eval 
 [form]
  (cond
    (seq? form)
      (partial-eval-seq (macroexpand form))
    (vector? form)
      (merge-constants vec form)
    (set? form)
      (merge-constants set form)
    (map? form)
      (merge-constants #(into (empty form) (partition 2 %)) (apply concat form))
    (symbol? form)
      (let [o (Object.)
            v (*locals* form o)]
        (if (identical? o v)
          (let [var (resolve form)]
            (or (*no-eval* var) (constant var)))
          v))
    :else
      (constant form)))
      
