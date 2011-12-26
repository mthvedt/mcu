(ns mcu.underflow)

(defprotocol Continuer
  (process [self state]))

(defn underflow [thefn & args]
  (loop [nextstep #(apply thefn args)
         state {}]
    (let [continuer (nextstep)
          [nextstep1 state1] (process continuer state)]
      (if (nil? state1) ; nil continuation terminates
        nextstep1
        (recur nextstep1 state1)))))

(defn- process-object [self state]
  (let [cont (get state ::cont)]
    (if-let [nextstep (first cont)]
      [#(nextstep self) (assoc state ::cont (rest cont))]
      [self nil])))

(extend Object Continuer {:process process-object})
(extend nil Continuer {:process process-object})

(defmacro =defn [sym & body]
  (let [=sym (symbol (str "=" sym))]
    `(do
       (defn ~sym ~@body)
       (defmacro ~=sym [~'& args#] `(underflow ~~sym ~@args#)))))

(defmacro =tailcall [thefn & args]
  `(reify Continuer
     (process [_ state#]
       [#(~thefn ~@args) state#])))

(defmacro =do [& body]
  `(reify Continuer
     (process [_ state#]
       [(fn 

(defmacro =letone [[tvar tval] & body]
  `(reify Continuer
     (process [_ state#]
       [(fn [] ~tval)
        (assoc state# ::cont (cons (fn [rval#]
                                     (let [~tvar rval#] ~@body))
                                   (get state# ::cont)))])))

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
     (process [_ state#]
       [#(let [~tvar (get state# ::cont)] ~@body)
        state#])))

(defmacro =continue [cont rval]
  `(reify Continuer
     (process [_ state#]
       (if-let [nextstep# (first ~cont)]
         [#(nextstep# ~rval)
          (assoc state# ::cont (rest ~cont))]
         [~rval nil]))))
