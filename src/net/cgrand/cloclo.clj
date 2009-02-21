(ns net.cgrand.cloclo)

(def *locals* {})
(def *no-eval* #{})
(def *inlines* {})

(defn constant [x]
  (with-meta (list x) {::constant true}))

(defn constant? [x]
  (-> x meta ::constant))

(defn value [x]
  (if (constant? x) (first x) x))

(declare partial-eval)

(defn find-matching-body [n bodies]
  (when (seq bodies)
    (if (vector? (first bodies))
      (recur n (list bodies))
      (let [[args :as body] (first bodies)
            m (if (some #{'&} args) n (count args))]
        (if (= m n)
          body
          (recur n (rest bodies)))))))  

(defn partial-call [form]
  (let [boxed-vals (map partial-eval form)
        vals (doall (map value boxed-vals))]
    (cond
      (every? constant? boxed-vals)
        (let [var (resolve (first vals))]
          (constant (apply var (rest vals))))
;        (constant (eval vals))
      (and (-> boxed-vals first constant? not) (-> boxed-vals first meta ::inline))
        (recur (cons (-> boxed-vals first meta ::inline) (next form)))
      (and (-> boxed-vals first constant? not) (-> boxed-vals first seq?) (= 'fn* (ffirst boxed-vals)))
        (let [boxed-args (rest boxed-vals)
              [args & body] (find-matching-body (count boxed-args) (nfirst boxed-vals))
              [args [_ rarg]] (split-with #(not= '& %) args)
              rval (drop (count args) boxed-args)
              rval (if (every? constant? rval) (constant (map value rval)) (map value rval))
              locals (into *locals* (map vector args boxed-args))
              locals (assoc locals rarg rval)]
          (binding [*locals* locals]
            (partial-eval-do `(do ~@body))))
      :else vals)))

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
              (set! *locals* (assoc *locals* sym (vary-meta sym assoc ::inline val)))
              (recur etc (conj partial-bindings sym val)))))
        (if (empty? partial-bindings)
          (partial-eval-do `(do ~@body))
          (doall `(let* ~partial-bindings ~@(map (comp value partial-eval) body))))))))

(defn partial-eval-fn*-body 
 [[args & body]]
  (binding [*locals* (into *locals* (for [x args :when (not= x '&)] [x x]))]
    (list args (partial-eval-do `(do ~@body))))) 

(defn partial-eval-seq [form]
  (if-let [[op & args] (seq form)]
    (condp = op 
      'if*
        (partial-eval-if* form)
      'let*
        (partial-eval-let* form)
      'do
        (partial-eval-do form)
      'fn*
        (let [bodies (if (vector? (first args)) (list args) args)]
          (list* 'fn* 
            (doall (map partial-eval-fn*-body bodies))))
      (if (special-symbol? op)
        (throw (Exception. (str "unimplemented special symbol " op)))
        (partial-call form)))
    form))

(defn- merge-constants [f args]
  (let [vals (map partial-eval args)]
    (if (every? constant? vals)
      (constant (f (map value vals)))
      (f (map value vals)))))

(defn resolve-sym [sym]
  (let [o (Object.)
        v (*locals* sym o)]
    (if (identical? v o)
      (if-let [r (resolve sym)]
        (constant (if (class? r)
                    (symbol (.getName r))
                    (symbol (-> r .ns .getName name) (-> r .sym .getName))))
        sym)
      v)))

(defn partial-eval 
 [form]
  (cond
    (seq? form)
      (if (*locals* (first form)) 
        (partial-eval-seq form)
        (let [ex-form (macroexpand-1 form)]
          (if (= form ex-form)
            (partial-eval-seq form)
            (recur ex-form))))
    (vector? form)
      (merge-constants vec form)
    (set? form)
      (merge-constants set form)
    (map? form)
      (merge-constants #(into (empty form) (partition 2 %)) (apply concat form))
    (symbol? form)
      (or (*no-eval* form) (resolve-sym form))
    :else
      (constant form)))
      
(defn distribute [f expr]
  (if (seq? expr)
    (let [[op :as expr] (macroexpand expr)]
      (condp = op
        'do (concat (butlast expr) [(distribute f (last expr))])
        'let* (concat (butlast expr) [(distribute f (last expr))])
        'if* (concat (take 2 expr) (map #(distribute f %) (drop 2 expr)))
        (list f expr)))
    (list f expr)))


;;;;;;;;;;;

(declare interpret)

(defn interpret-call [locals form]
  (let [[f & args] (doall (map #(interpret locals %) form))]
    (apply f args)))

(defn interpret-form-default [locals [op :as form]]
  (if (contains? locals op)
    (interpret-call locals form)
    (let [ex-form (macroexpand form)]
      (if (= form ex-form)
        (interpret-call locals form)
        (interpret locals ex-form)))))
        
  
(def *special-interpreters* {
  'if* 
    (fn [locals [_ test then else]]
      (interpret locals (if (interpret locals test) then else)))
  'do 
    (fn [locals [_ & forms]]
      (interpret-forms locals forms))
  'let* 
    (fn [locals [_ bindings & forms]]
      (interpret-forms 
        (reduce (fn [locals [k v]] (assoc locals k (interpret locals v))) 
          locals (partition 2 bindings))
        forms))   
  'loop* 
    (fn [locals [_ bindings & forms]]
      (let [locs (take-nth 2 bindings)]
        (interpret locals `(let ~bindings ((fn [~@locs] ~@forms) ~@locs)))))  
  'quote 
    (fn [locals [_ & forms]]
      forms)
  'new 
    (fn [locals [_ class & forms]]
      (clojure.lang.Reflector/invokeConstructor (resolve class) (to-array (map #(interpret locals %) forms)))) 
  '. 
    (fn [locals [_ obj member & forms]]
      (if forms
        (clojure.lang.Reflector/invokeInstanceMethod (interpret locals obj) (name member) (to-array (map #(interpret locals %) forms)))
        (clojure.lang.Reflector/invokeNoArgInstanceMember (interpret locals obj) (name member)))) 
  'fn* 
    (fn [locals [_ arglist & forms :as f]]
      (let [name (when (symbol? arglist) arglist)
            arglist (if name (first forms) arglist)
            forms (if name (next forms) forms)
            bodies (if (vector? arglist) [(cons arglist forms)] (cons arglist forms))
            arity-map (into {} (map (fn [[arglist & forms]] (interpret-fn-body locals name arglist forms)) bodies))
            fixmax (apply max (keys arity-map))]
        (fn [& vals]
          (let [n (if (or (neg? fixmax) (seq (drop fixmax vals))) -1 (count vals))] 
            (apply (arity-map n) vals)))))})          

(defn interpret-form [locals form]
  ((*special-interpreters* (first form) interpret-form-default) locals form))

(defn interpret-forms [locals forms]
  (last (map #(interpret locals %) forms)))

(defn interpret-fn-body [locals name arglist forms]
  (let [[fixargs [_ vararg]] (split-with #(not= '& %) arglist)
        n (count fixargs)
        m (if vararg -1 n)]
    [m (fn this [& vals]
         (let [locals (if name (assoc locals name this) locals)
               locals (into locals (zipmap fixargs vals))
               locals (if vararg 
                        (assoc locals vararg (seq (drop n vals)))
                        locals)
               locals (assoc locals 'recur this)]
           (interpret-forms locals forms)))])) 

(defn interpret-seq [locals form]
  (if (seq form)
    (interpret-form locals form)
    form))

(defn interpret
 [locals form]
  (cond
    (seq? form)
      (interpret-seq locals form)
    (vector? form)
      (vec (map #(interpret locals %) form))
    (set? form)
      (set (map #(interpret locals %) form))
    (map? form)
      (into (empty form) (map #(map (partial interpret locals) %) form))
    (symbol? form)
      (if (contains? locals form)
        (locals form)
        (let [r (resolve form)]
          (if (var? r)
            (deref r)
            r)))
    :else
      form))
 