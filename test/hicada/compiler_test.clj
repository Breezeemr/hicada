(ns hicada.compiler-test
  (:refer-clojure :exclude [compile])
  (:require [clojure.test :refer :all]
            [hicada.compiler :refer [compile]]))


(deftest first-one

  (is (= '(js/React.createElement "h1" (js* "{'className':~{}}" "b c a"))
        (compile [:h1.b.c {:class "a"}]) ;; should be "b c a", order preserved
        ))
  (is (= '(js/React.createElement "h1" (js* "{'className':~{}}" "b c a"))
        (compile [:h1.b.c {:className "a"}])))
  (is (= '["div" {:className [a]} "hmm"]
        (compile '[:div {:class [a]} "hmm"]
          {:server-render? true
           :emit-fn (fn [a b c]
                      (into [a b] c))})))
  (is (= (list 'js/React.createElement
           "div"
           nil
           (list 'clojure.core/reduce
             (list 'clojure.core/fn
               'hicada-for-reducer
               ['out-arr__560__auto__ 'x]
               (list '.push
                 'out-arr__560__auto__
                 (list 'js/React.createElement
                   "span"
                   nil
                   (list 'hicada.compiler/interpret-when-necessary 'x)))
               'out-arr__560__auto__)
             (list 'cljs.core/array)
             'xs))
        (compile '[:div (for [x xs]
                          [:span x])]
          {:rewrite-for? true})))
  (is (= '(js/React.createElement
           "div"
           nil
           (js/React.createElement
             "span"
             (js* "{'key':~{}}" "foo")
             (hicada.compiler/interpret-when-necessary a)
             (hicada.compiler/interpret-when-necessary b)
             (hicada.compiler/interpret-when-necessary c))
           (js/React.cloneElement
             x
             (js* "{'key':~{}}" k)
             (hicada.compiler/interpret-when-necessary one)
             (hicada.compiler/interpret-when-necessary two))
           (js/React.cloneElement x (js* "{'key':~{}}" k)))
        ;; Example :clone handler + emitter:
        (compile '[:div
                   [:span {:key "foo"} a b c]
                   [:clone x {:key k} one two]
                   [:clone x {:key k}]]
          {:array-children? false ;; works with both!
           :emit-fn (fn [tag attr children]
                      ;; Now handle the emitter case:
                      (if (and (seq? tag) (= ::clone (first tag)))
                        (list* 'js/React.cloneElement (second tag) attr children)
                        (list* 'js/React.createElement tag attr children)))}
          {:clone (fn [_ node attrs & children]
                    ;; Ensure props + children are in the right position:
                    [(list ::clone node) attrs children])})))
  (is (= '(js/React.createElement
            js/React.Fragment
            (js* "{'key':~{}}" "a")
            (hicada.compiler/interpret-when-necessary a)
            (hicada.compiler/interpret-when-necessary b))
        (compile '[:* {:key "a"} a b])))
  (is (= '(js/React.createElement
            js/React.Fragment
            nil
            (hicada.compiler/interpret-when-necessary a)
            (hicada.compiler/interpret-when-necessary b))
        (compile '[:* a b])))
  (is (= '(js/React.createElement
            "div"
            props
            (hicada.compiler/interpret-when-necessary b))
        (compile '[:> :div props b])))
  (is (= '(js/React.createElement
            X
            (js* "{'kebab-case':~{},'camelCase':~{},'camelCase2':~{}}" y x 8))
        ;; Doesn't convert string keys, but do convert keywords & symbols:
        (compile '[X {"kebab-case" y :camel-case x camel-case-2 8}])))
  (is (= '(js/React.createElement
            Transition
            (js* "{'in':~{}}" in-prop)
            (hicada.compiler/interpret-when-necessary (fn [state])))
        (compile '[Transition {:in in-prop} (fn [state])]) ;; works eq to :>
        ))
  (is (= '[(hicada.compiler/interpret-when-necessary a)
          (hicada.compiler/interpret-when-necessary b)
          (hicada.compiler/interpret-when-necessary c)]
        (compile '[a b c]) ;; We have a coll of ReactNodes. Don't touch
        ))
  (is (= '(hicada.compiler/interpret-when-necessary (some-fn {:in in-prop} (fn [state])))
        (compile '(some-fn {:in in-prop} (fn [state]))) ;; FN call, don't touch
        ))
  (is (= '(js/React.createElement
            Transition
            (js*
              "{'in':~{},'unmountOnExit':~{},'timeout':~{}}"
              in-prop
              true
              (js* "{'enter':~{},'exit':~{}}" 300 100))
            (hicada.compiler/interpret-when-necessary (fn [state])))
        (compile
          '[:> Transition {:in in-prop
                           :unmount-on-exit true
                           :timeout {:enter 300, :exit 100}}
            (fn [state])])))
  (is (= '(js/React.createElement
            "div"
            (js* "{'dangerouslySetInnerHTML':~{}}" (js* "{'__html':~{}}" "<div>hi</div>")))
        ;; Issue #2:
        (compile '[:div {:ihtml "<div>hi</div>"}]
          {:transform-fn (fn [[tag attr ch]]
                           (if-some [html (:ihtml attr)]
                             [tag
                              (-> attr
                                  (dissoc :ihtml)
                                  (assoc :dangerouslySetInnerHTML {:__html html}))
                              ch]
                             [tag attr ch]))})
        ))
  (is (= '(js/React.createElement
            my.rn.native/Text
            nil
            (hicada.compiler/interpret-when-necessary a)
            (hicada.compiler/interpret-when-necessary b))
        (compile '[:Text a b]
          {:no-string-tags? true
           :default-ns 'my.rn.native})))
  (is (= '(js/React.createElement
            "Text"
            nil
            (hicada.compiler/interpret-when-necessary a)
            (hicada.compiler/interpret-when-necessary b))
        (compile '[:rn/Text a b] {})))
  (is (= '(js/React.createElement
           Text
           nil
           (hicada.compiler/interpret-when-necessary a)
           (hicada.compiler/interpret-when-necessary b))
        (compile '[:Text a b] {:no-string-tags? true})))
  (is (= '(js/React.createElement
           "Text"
           (js* "{'style':~{}}" (cljs.core/array (js* "{'borderBottom':~{}}" "2px"))))
        (compile '[:Text {:style [{:border-bottom "2px"}]}])))
  (is (= '(js/React.createElement
            "div"
            nil
            (hicada.compiler/interpret-when-necessary a)
            (hicada.compiler/interpret-when-necessary b))
        (compile '[:div a b] {:array-children? false})))
  (is (= '(js/React.createElement
            "div"
            (js* "{'style':~{}}" (clj->js (assoc {} :width 10))))
        (compile '[:div {:style (assoc {} :width 10)}])))
  (is (= '(js/React.createElement
            "div"
            (js* "{'data-style':~{}}" (clj->js (assoc {} :width 10))))
        (compile '[:div {:data-style (assoc {} :width 10)}])))
  (is (= '(js/React.createElement
            "div"
            nil
            (hicada.compiler/interpret-when-necessary (assoc {} :style {:width 10})))
        (compile '[:div (assoc {} :style {:width 10})]))))



