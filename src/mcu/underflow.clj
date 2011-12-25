(ns mcu.underflow)

(defprotocol Continuation
  (continue [self rval]))

(defprotocol Continuer
  (process [self cont state]))

(defn underflow [thefn & args]
  (loop [nextstep #(apply thefn args)
         cont (reify Continuation ; the terminating continuation
                (continue [self rval]
                  [rval nil]))
         state {}]
    (let [continuer (nextstep)
          [nextstep1 cont1 state1] (process continuer cont state)]
      (if (nil? cont1) ; nil continuation terminates
        nextstep1
        (recur nextstep1 cont1 state1)))))

(defn- process-object [self cont state]
  (continue cont self))

(extend Object Continuer {:process process-object})
(extend nil Continuer {:process process-object})

(defmacro =defn [sym & body]
  (let [=sym (symbol (str "=" sym))]
    `(do
       (defn ~sym ~@body)
       (defmacro ~=sym [~'& args#] `(underflow ~~sym ~@args#)))))

(defmacro =tailcall [thefn & args]
  `(reify Continuer
     (process [self# cont# state#]
       [#(~thefn ~@args) cont#])))

(defmacro =letone [[tvar tval] & body]
  `(reify Continuer
     (process [self# cont# state#]
       [(fn [] ~tval)
        (reify Continuation
          (continue [self# rval#]
            [#(let [~tvar rval#] ~@body)
             cont#]))])))

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
  `(reify Continuer
     (process [self# cont# state#]
       [(fn [] (let [~tvar cont#] ~@body))
        cont#])))

(defmacro =continue [cont rval]
  `(reify Continuer
     (process [self# cont# state#]
       (continue ~cont ~rval))))
