(ns net.cgrand.cloclo)

(defn constant [x]
  (with-meta (list x) {::constant true}))

(defn constant? [x]
  (-> x meta ::constant))

(defn value [x]
  (if (constant? x) (first x) x))

(declare partial-eval)

(defn partial-call [form locals]
  (let [boxed-vals (map #(partial-eval % locals) form)
        vals (map value boxed-vals)]
    (if (every? constant? boxed-vals)
      (constant (eval vals))
      vals)))

(defn partial-eval-if* [[_ test then & [else]] locals]
  (let [test (partial-eval test locals)]
    (if (constant? test)
      (partial-eval (if (value test) then else) locals)
      `(if* ~test ~@(map #(value (partial-eval % locals)) [then else])))))
      
(defn partial-eval-do [[_ & body] locals]
  (let [vals (map #(partial-eval % locals) body)
        statements (remove constant? (butlast vals))]
    (if (empty? statements)
      (last vals)
      `(do ~@statements ~(value (last vals))))))
 
(defn partial-eval-let* [[_ bindings & body] locals]
  (loop [bindings (seq bindings) locals locals partial-bindings []]
    (if-let [[sym expr & etc] bindings]
      (let [val (partial-eval expr locals)]
        (if (constant? val)
          (recur etc (assoc locals sym val) partial-bindings)
          (recur etc (assoc locals sym sym) (conj partial-bindings sym val))))
      (if (empty? partial-bindings)
        (partial-eval-do `(do ~@body) locals)
        `(let* ~partial-bindings ~@(map #(value (partial-eval % locals)) body))))))
    
(defn partial-eval-seq [form locals]
  (if-let [[op & args] (seq form)]
    (cond 
      (= op 'if*)
        (partial-eval-if* form locals)
      (= op 'let*)
        (partial-eval-let* form locals)
      (= op 'do)
        (partial-eval-do form locals)
      :else
        (if (special-symbol? op)
          (throw (Exception. (str "unimpleted special symbol " op)))
          (partial-call form locals)))
    form))

(defn- merge-constants [f args locals]
  (let [vals (map #(partial-eval % locals) args)]
    (if (every? constant? vals)
      (constant (f (map value vals)))
      (f (map value vals)))))

(defn partial-eval 
  ([form] (partial-eval form {}))
  ([form locals]
    (cond
      (seq? form)
        (partial-eval-seq (macroexpand form) locals)
      (vector? form)
        (merge-constants vec form locals)
      (set? form)
        (merge-constants set form locals)
      (map? form)
        (merge-constants #(into (empty form) (partition 2 %)) (apply concat form) locals)
      (symbol? form)
        (let [o (Object.)
              v (locals form o)]
          (if (identical? o v)
            (let [var (resolve form)]
              (locals var (constant var))) 
            v))
      :else
        (constant form))))
      
