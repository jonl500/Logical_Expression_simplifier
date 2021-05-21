(ns project2v2.core)
;use rest function in actual project (separates the list from the first item)
;? -> ask boolean
;seq is sequence
;use let
(defn lookup
  "look up val i in map m and return the result if
  it exists, otherwise return i"
  [i m]
  (get m i i))


;(a c b d)
;=> (1 c 2 d)

(defn substitute
  "substitute each element in
  l with the value the element is mapped to in m"
  [l m]
  (map #(lookup % m) l))

(defn deep-substitute
  "substitute, but recursive."
  [l m]
  (map (fn [i]
         (if (seq? i)
           (deep-substitute i m)
           (lookup i m)))
       l))



(defn and-simplify [l]
  "takes in a list of ands, simplifies them"
  ;(cond
  ;  (every? true? (rest l)) true
  ;  (some false? l) false
  ;  (some true? (rest l)) (remove true? l)
  ;  )
  (let [restl (rest l)
        AllT (every? true? restl)
        F (some false? restl)
        AllV (filter (fn [i] (not (boolean? i))) restl)]
    (cond
      F false
      AllT true
      (= (count (distinct AllV)) 1) (first (distinct AllV))

      :else (conj (distinct AllV) 'and)
      )
    )
  )

(defn or-simplify [l]
  "take list of or's, simplify to the value"
  (let [restl (rest l)
        AllT (every? true? restl)
        SomeT (some true? restl)
        AllF (every? false? restl)
       ; SomeF (some false? restl)
       AllV (filter (fn [i] (not (boolean? i))) restl)
        SomeV (some (fn [i] (not (boolean? i))) restl)]
    (cond
      AllT true
      SomeT true
      AllF false
      ;SomeF true
      (= (count (distinct AllV)) 1) (first (distinct AllV))
      :else (conj (distinct AllV) 'or)
      )
    )
  )

(defn not-simplify [l]
  "takes not and simplifies it"
  (let [restl (rest l)
       AllT (every? true? restl)

       AllF (every? false? restl)

       AllV (filter (fn [i] (not (boolean? i))) restl)]
    (cond
     AllT false
     AllF true
     (and (seq? (second l))(= (first (second l)) 'not)) (second (second l))
     ;use map for conversion
     (and (seq? (second l))(= (first (second l)) 'and)) (not-simplify(cons 'or (map (fn [n](list 'not n))(rest (second l)))))
     (and (seq? (second l))(= (first (second l)) 'or)) (not-simplify (cons 'and (map (fn [n](list 'not n))(rest (second l))))) ;do another wrapper of the not or exprssn simplify on the the cons
     ;list manipulation look at clojure list
     :else (conj AllV 'not)
      )
  )
  )
;(defn evalexp [exp bindings] (simplify (bind-values bindings exp)))
(defn expression-simplify [l binding]
  (let [subvar (map (fn [x]
                      (cond
                        (seq? x) (expression-simplify x binding)
                        :default
                        (lookup x binding)
                        )

                      )
                    l)]
           (cond
             ;(seq? a)(expression-simplify a bind)
             (= (first subvar) 'and) (and-simplify subvar)
             (= (first subvar) 'or) (or-simplify subvar)
             (= (first subvar) 'not) (not-simplify subvar)
             ))
        )




; 1. Do "dispatch method" - if it's just a boolean or not a sequence - just return it
; 2. Make dispatch recursive - use map refer to deep sub
; 3. Add two missing not cases.


;how do I tie all three functions for the main?
;how do I handle recursion?
;span out recursively
;(defn lookup
;  "look up val i in map m and return the result if
;  it exists, otherwise return i"
;  [i m]
;  (get m i i))
;
;
;;(a c b d)
;;=> (1 c 2 d)
;
;(defn substitute
;  "substitute each element in
;  l with the value the element is mapped to in m"
;  [l m]
;  (map #(lookup % m) l))
;
;(defn deep-substitute
;  "substitute, but recursive."
;  [l m]
;  (map (fn [i]
;         (if (seq? i)
;           (deep-substitute i m)
;           (lookup i m)))
;       l))

