((clojure-mode . ((eval . (progn
                            (put 'defna 'clojure-doc-string-elt 2)
                            (put 'defne 'clojure-doc-string-elt 2)
                            (define-clojure-indent
                              (matcha '(1 (:defn)))
                              (matche '(1 (:defn)))
                              (matchu '(1 (:defn)))))))))
