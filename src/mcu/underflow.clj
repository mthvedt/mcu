(ns mcu.underflow
  "Underflow's speed isn't too bad. On my MBA (rev. 3 13 inch,
  1.8Ghz Core 2 Duo) it can recursively sum the first 1000 integers
  in about 110 us. The vanilla recursive version takes about 50 us.
  This is a performance penalty of 60 ns per call.
  The vanilla loop/recur version is only slightly faster. Of course,
  the math is checked...")

(defprotocol Pval
  (process [self cont]))

(defn- process-object [self cont]
  (let [contfn (first cont)]
    (if (nil? contfn)
      [nil self]
      [#(contfn self) (rest cont)])))

(extend Object Pval {:process process-object})
(extend nil Pval {:process process-object})

(defmacro =defn [sym & body]
  (let [=sym (symbol (str "=" sym))]
    `(do
       (defn ~sym ~@body)
       (defmacro ~=sym [~'& args#] `(underflow ~~sym ~@args#)))))

(defn underflow [thefn & args]
  (loop [trampfn #(apply thefn args)
         cont nil]
    (let [result (trampfn)
          [newfn cont2] (process result cont)]
      (if (nil? newfn)
        cont2
        (recur newfn cont2)))))

(defmacro =tailcall [thefn & args]
  `(reify Pval
     (process [self# cont#]
       [#(~thefn ~@args) cont#])))

(defmacro =letone [[tvar tval] & body]
  `(reify Pval
     (process [self# cont#]
       [(fn [] ~tval)
        (cons (fn [~tvar] ~@body) cont#)])))

(defmacro =let [bindings & body]
  (if (empty? bindings)
    `(do ~@body)
    (let [tvar (first bindings)
          tval (second bindings)]
      (if (nil? tval)
        (throw (IllegalArgumentException.
                 "=let requires an even number of forms in binding vector"))
        `(=letone [~tvar ~tval]
                  (=let [~@(rest (rest bindings))] ~@body))))))

(defmacro =bind-cc [tvar & body]
  `(reify Pval
     (process [self# cont#]
       [(fn [] (let [~tvar cont#] ~@body))
        cont#])))

(defmacro =continue [cont rval]
  `(reify Pval
     (process [_ _]
       (if-let [cc# (first ~cont)]
         [(fn [] (cc# ~rval))
          (rest ~cont)]
         [nil ~rval]))))

#_(deftype PTailcall [thefn]
    Pval
    (process [self cont] [thefn cont]))

#_(defmacro =tailcall [thefn & args]
    `(PTailcall. #(~thefn ~@args)))
