(ns interprete.core-test
  (:require [clojure.test :refer :all]
            [interprete.core :refer :all]))

(deftest palabra-reservada?-test
  (testing "Devuelve true si es una palabra reservada"
    (is (= (palabra-reservada? 'REM) true))
    (is (= (palabra-reservada? 'SPACE) false))
    (is (= (palabra-reservada? 'SAVE) true)) 
    ))

(deftest operador?-test
  (testing "Devuelve true si es un operador"
    (is (= (operador? '+) true))
    (is (= (operador? (symbol "%")) false))
    (is (= (operador? (symbol "+")) true))
    ))

(deftest anular-invalidos-test
  (testing "Reemplaza los simbolos invalidos por nil"
    (is (= (anular-invalidos '(IF X & * Y < 12 THEN LET ! X = 0)) '(IF X nil * Y < 12 THEN LET nil X = 0)))
    ))

(deftest cargar_linea-test
  (testing "Carga las lineas en orden en el ambiente"
    (is (= (cargar-linea '(10 (PRINT X)) [() [:ejecucion-inmediata 0] [] [] [] 0 {}]) '[((10 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}]))
    (is (= (cargar-linea '(20 (X = 100)) ['((10 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}]) '[((10 (PRINT X)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}]))
    (is (= (cargar-linea '(15 (X = X + 1)) ['((10 (PRINT X)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}]) '[((10 (PRINT X)) (15 (X = X + 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}]))
    (is (= (cargar-linea '(15 (X = X - 1)) ['((10 (PRINT X)) (15 (X = X + 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}]) '[((10 (PRINT X)) (15 (X = X - 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}]))))

(deftest expandir-nexts-test
  (testing "Expande las sentencias compuestas NEXT a simples"
    (is (= (expandir-nexts  '((PRINT 1) (NEXT A , B))) '((PRINT 1) (NEXT A) (NEXT B))))))

(deftest variable-float?-test
  (testing "Chequea si la variable es un float"
    (is (= (variable-float? 'X) true))
    (is (= (variable-float? 'X%) false))
    (is (= (variable-float? 'X$) false))))

(deftest variable-integer?-test
  (testing "Chequea si la variable es un integer"
    (is (= (variable-integer? 'X) false))
    (is (= (variable-integer? 'X%) true))
    (is (= (variable-integer? 'X$) false))))

(deftest variable-string?-test
  (testing "Chequea si la variable es un string"
    (is (= (variable-string? 'X) false))
    (is (= (variable-string? 'X%) false))
    (is (= (variable-string? 'X$) true))))

(deftest dar-error-test
  (testing "Imprime el error correspondiente y devuelve nil"
    (is (= (dar-error 16 [:ejecucion-inmediata 4]) nil))
    (is (= (with-out-str (dar-error 16 [:ejecucion-inmediata 4])) "?SYNTAX  ERROR"))
    (is (= (dar-error "?ERROR DISK FULL" [:ejecucion-inmediata 4]) nil))
    (is (= (with-out-str (dar-error "?ERROR DISK FULL" [:ejecucion-inmediata 4])) "?ERROR DISK FULL"))
    (is (= (dar-error 16 [100 3]) nil))
    (is (= (with-out-str (dar-error 16 [100 3])) "?SYNTAX  ERROR IN 100"))
    (is (= (dar-error "?ERROR DISK FULL" [100 3]) nil))
    (is (= (with-out-str (dar-error "?ERROR DISK FULL" [100 3])) "?ERROR DISK FULL IN 100"))))

(deftest contar-sentencias?-test
  (testing "Cuenta las sentencias de una determinada linea"
    (is (= (contar-sentencias 10 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}]) 2))
    (is (= (contar-sentencias 15 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}]) 1))
    (is (= (contar-sentencias 20 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}]) 2))))

(deftest buscar-lineas-restantes-test
  (testing "Buscar las lineas restantes"
    (is (= (buscar-lineas-restantes [() [:ejecucion-inmediata 0] [] [] [] 0 {}]) nil))
    (is (= (buscar-lineas-restantes ['((PRINT X) (PRINT Y)) [:ejecucion-inmediata 2] [] [] [] 0 {}]) nil))
    (is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 2] [] [] [] 0 {}]) (list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) ))
    (is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}]) (list '(10 (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) ))
    (is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 0] [] [] [] 0 {}]) (list '(10) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) ))
    (is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 1] [] [] [] 0 {}]) (list '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) ))
    (is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 0] [] [] [] 0 {}]) (list '(15) (list 20 (list 'NEXT 'I (symbol ",") 'J))) ))
    (is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}]) '((20 (NEXT I) (NEXT J))) ))
    (is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 2] [] [] [] 0 {}]) '((20 (NEXT I) (NEXT J))) ))
    (is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 1] [] [] [] 0 {}]) '((20 (NEXT J))) ))
    (is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 0] [] [] [] 0 {}]) '((20)) ))
    (is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 -1] [] [] [] 0 {}]) '((20)) ))
    (is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [25 0] [] [] [] 0 {}]) nil ))))

(deftest extraer-data-test
  (testing "Extrae data"
    (is (= (extraer-data '(())) '()))
    (is (= (extraer-data (list '(10 (PRINT X) (REM ESTE NO) (DATA 30)) '(20 (DATA HOLA)) (list 100 (list 'DATA 'MUNDO (symbol ",") 10 (symbol ",") 20)))) '("HOLA" "MUNDO" 10 20)))))

(deftest preprocesar-expresion-test
  (testing "Extrae data"
    (is (= (preprocesar-expresion '(X$ + " MUNDO" + Z$) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA"}]) '("HOLA" + " MUNDO" + "")))
    (is (= (preprocesar-expresion '(X + . / Y% * Z) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 5 Y% 2}]) '(5 + 0 / 2 * 0)))))

(deftest desambiguar-test
  (testing "Extrae data"
    (is (= (desambiguar (list '- 2 '* (symbol "(") '- 3 '+ 5 '- (symbol "(") '+ 2 '/ 7 (symbol ")") (symbol ")"))) (list '-u 2 '* (symbol "(") '-u 3 '+ 5 '- (symbol "(") 2 '/ 7 (symbol ")") (symbol ")"))))
    (is (= (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ")"))) (list 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ")"))))
    (is (= (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ",") 3 (symbol ")"))) (list 'MID3$ (symbol "(") 1 (symbol ",") 2 (symbol ",") 3 (symbol ")"))))
    (is (= (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") '- 2 '+ 'K (symbol ",") 3 (symbol ")"))) (list 'MID3$ (symbol "(") 1 (symbol ",") '-u 2 (symbol "+") 'K (symbol ",") 3 (symbol ")"))))))

(deftest precedencia-test
  (testing "El valor de precedencia"
    (is (= (precedencia 'OR) 1))
    (is (= (precedencia 'AND) 2))
    (is (= (precedencia '*) 6))
    (is (= (precedencia 'MID$) 8))
    (is (= (precedencia '-u) 7))))

(deftest aridad-test
  (testing "El valor de aridad"
    (is (= (aridad 'THEN) 0))
    (is (= (aridad 'SIN) 1))
    (is (= (aridad '*) 2))
    (is (= (aridad 'MID$) 2))
    (is (= (aridad 'MID3$) 3))))

(deftest eliminar-cero-decimal-test
  (testing "Elimina ceros decimales no significativos"
    (is (= (eliminar-cero-decimal 1.5) 1.5))
    (is (= (eliminar-cero-decimal 1.50) 1.5))
    (is (= (eliminar-cero-decimal 1.0) 1))
    (is (= (eliminar-cero-decimal 'A) 'A))))

(deftest eliminar-cero-entero-test
  (testing "Elimina ceros enteros no significativos"
    (is (= (eliminar-cero-entero nil) nil))
    (is (= (eliminar-cero-entero 0) " 0"))
    (is (= (eliminar-cero-entero 1.5) " 1.5")) 
    (is (= (eliminar-cero-entero 1) " 1"))
    (is (= (eliminar-cero-entero -1) "-1"))
    (is (= (eliminar-cero-entero -1.5) "-1.5"))
    (is (= (eliminar-cero-entero 0.5) " .5"))
    (is (= (eliminar-cero-entero -0.5) "-.5"))
    (is (=  (eliminar-cero-entero 'A) "A"))))

(deftest ejecutar-asignacion-test
  (testing "Ejecuta asignacion"
    (is (=  (ejecutar-asignacion '(X = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 {}]) '[((10 (PRINT X))) [10 1] [] [] [] 0 {X 5}]))
    (is (= (ejecutar-asignacion '(X = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}]) '[((10 (PRINT X))) [10 1] [] [] [] 0 {X 5}]))
    (is (= (ejecutar-asignacion '(X = X + 1) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}]) '[((10 (PRINT X))) [10 1] [] [] [] 0 {X 3}]))
    (is (= (ejecutar-asignacion '(X$ = X$ + " MUNDO") ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA"}]) '[((10 (PRINT X))) [10 1] [] [] [] 0 {X$ "HOLA MUNDO"}]))))

(deftest continuar-linea-test
  (testing "Imprime el error en caso de error y continua linea"
    (is (= (with-out-str (continuar-linea [(list '(10 (PRINT X)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}])) "?RETURN WITHOUT GOSUB  ERROR IN 20"))
    (is (= (continuar-linea [(list '(10 (PRINT X)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}]) ['nil [(list (list 10 (list 'PRINT 'X)) (list 15 (list 'X '= 'X '+ 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}]]))
    (is (= (continuar-linea [(list '(10 (PRINT X)) '(15 (GOSUB 100) (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [[15 2]] [] [] 0 {}]) [':omitir-restante [(list (list 10 (list 'PRINT 'X)) (list 15 (list 'GOSUB 100) (list 'X '= 'X '+ 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 1] [] [] [] 0 {}]]))
    (is (= (with-out-str (continuar-linea [(list '(10 (PRINT X)) '(15 (GOSUB 100) (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [[15 2]] [] [] 0 {}])) ""))))
