;;;; Ficheiro de Procura - Trata da implementação dos algoritmos de procura e suas funções auxiliares
;;;; Projeto IA Fase 1 - Dots and Boxes
;;;; Autores: Guilherme Martins 201802243, Jorge Mimoso (numero)


;;###################################AUXILIARES#############################################
(defun tabuleiro-teste ()
  "Retorna um tabuleiro 3x3 (3 arcos na vertical por 3 arcos na horizontal)"
	'(
		((0 0 0) (0 0 1) (0 1 1) (0 0 1))
		((0 0 0) (0 1 1) (1 0 1) (0 1 1))
	)
)

(defun no-tabuleiro-teste ()
     '(
       (
        ((1 1 1) (1 0 1) (0 1 1) (0 0 1)) 
        ((1 0 0) (0 1 1) (1 0 1) (0 1 1))
       ) 0 nil)
)



(defun no-teste ()
     '(
       (
        ((0 0 0) (0 0 0) (0 0 0) (0 0 1) (0 0 1)) 
        ((0 0 0 0) (0 0 0 0) (0 0 0 1) (0 0 0 1))
       ) 0 nil)
)

(defun no-teste2 ()
     '(
       (
        ((0 0 0 0) (0 0 0 0) (0 0 0 1) (0 0 0 1)) 
        ((0 0 0) (0 0 0) (0 0 0) (0 0 1) (0 0 1))
       ) 0 nil)
)

(defun no-teste3 ()
     '(
       (
        ((0 0 0) (0 0 1) (0 1 1) (0 0 1))
        ((0 0 0) (0 1 0) (0 0 1) (0 1 1))
       ) 0 nil)
)

(defun no-teste4 ()
     '(
       (
        ((0 0 0) (0 1 0) (0 1 0) (0 0 0))
        ((0 0 0) (0 1 0) (0 1 0) (0 0 0))
       ) 0 nil)
)

(defun no-teste5 ()
     '(
       (
        ((0 0 0 0) (0 0 0 0) (0 0 1 0) (0 0 1 0) (0 0 1 0))
        ((0 0 0 0) (0 0 0 0) (0 0 1 1) (0 0 1 1) (0 0 0 0))
       ) 0 nil)
)

(defun no-teste6 ()
     '(
       (
        ((0 0 1 0) (1 1 1 1) (0 0 1 1) (0 0 1 1) (0 0 1 1))
        ((0 0 1 1) (0 0 1 1) (1 1 1 1) (1 0 1 1) (0 1 1 1))
       ) 0 nil)
)

(defun criar-no (tabuleiro &optional (g 0) (pai nil))
     (list tabuleiro g pai)
)

(defun no-estado (no)
     (first no)
)

(defun no-profundidade (no)
     (second no)
)

(defun no-pai (no)
     (third no)
)

;;REVER
(defun caixa-fechada-hrzp (no line col)
     (cond
         ((zerop (get-arco-na-posicao line col (get-arcos-horizontais (no-estado no)))) nil)
         ((and
            (not (zerop (get-arco-na-posicao line col (get-arcos-horizontais (no-estado no)))))
            (not (zerop (get-arco-na-posicao (1+ line) col (get-arcos-horizontais (no-estado no)))))
            (not (zerop (get-arco-na-posicao col line (get-arcos-verticais (no-estado no)))))
            (not (zerop (get-arco-na-posicao (1+ col) line (get-arcos-verticais (no-estado no)))))
          ) t) 
     )
)

(defun caixa-fechada-vrtp (no line col)
     (cond
         ((zerop (get-arco-na-posicao line col (get-arcos-verticais (no-estado no)))) nil)
         ((and
             (not (zerop (get-arco-na-posicao line col (get-arcos-verticais (no-estado no)))))
             (not (zerop (get-arco-na-posicao (1+ line) col (get-arcos-verticais (no-estado no)))))
             (not (zerop (get-arco-na-posicao col line (get-arcos-horizontais (no-estado no)))))
             (not (zerop (get-arco-na-posicao (1+ col) line (get-arcos-horizontais (no-estado no)))))
           ) t)
     )
)

(defun caixa-fechadap (no line col)
     (cond
         ((or (caixa-fechada-hrzp no line col) (caixa-fechada-vrtp no line col)) t)
     )
)

;;Fixed -> actualizei apenas para verificar numa direção (horizontal), se verificarmos as duas direções, conta caixas repetidas
(defun num-caixas-fechadas (no &optional (line 1) (col 1)) 
     (cond
         ((or 
            (= line (length (get-arcos-horizontais (no-estado no))))
            (= line (length (get-arcos-verticais (no-estado no))))
          ) 0)
         ((and 
             (or 
               (= col (length (get-arco-horizontal (no-estado no)))) 
               (= col (length (get-arco-vertical (no-estado no)))) 
             )
             (not (caixa-fechada-hrzp no line col))
          )(+ 0 (num-caixas-fechadas no (1+ line) 1)))
         ((and 
             (or 
               (= col (length (get-arco-horizontal (no-estado no)))) 
               (= col (length (get-arco-vertical (no-estado no)))) 
             )
             (caixa-fechada-hrzp no line col)
          )(+ 1 (num-caixas-fechadas no (1+ line) 1)))
         ((caixa-fechada-hrzp no line col) (+ 1 (num-caixas-fechadas no line (1+ col))))
         (t (+ 0 (num-caixas-fechadas no line (1+ col))))
     )
)


;;; predicado no-solucaop que verifica se um estado e final
;; teste: (no-solucaop (no-teste) nCaixasFechadas)
;; resultado: NIL
(defun no-solucaop(no &optional (solucao 1))
  (cond
    ((= solucao (num-caixas-fechadas  no ))t)
  )
)


(defun operadores ()
 "Cria uma lista com  os operadores do problema."
 (list 'arco-horizontal 'arco-vertical))



(defun novo-sucessor(no op &optional (line 1)(col 1))
   
  (cond
    ((equal op 'arco-horizontal) 
      (cond
          
       ((= (length (car (car(no-estado no )))) col ) (novo-sucessor no 'arco-horizontal (+ 1 line) 1)) 
       ((= (length (car (no-estado no))  ) line) nil)       
       ((equal (arco-horizontal line col (no-estado no))  nil) (novo-sucessor no 'arco-horizontal line (+ 1 col)))         
       (t  (criar-no  (arco-horizontal line col (no-estado no)) (+ (no-profundidade no) 1) (no-estado no)))
                   
       )
    )
     ((equal op 'arco-vertical) 
      (cond
          
       ((= (length (car (car(no-estado no )))) col ) (novo-sucessor no 'arco-vertical (+ 1 line) 1)) 
       ((= (length (car (no-estado no))  ) line) nil)       
       ((equal (arco-vertical line col (no-estado no))  nil) (novo-sucessor no 'arco-vertical line (+ 1 col)))         
       (t  (criar-no  (arco-vertical line col (no-estado no)) (+ (no-profundidade no) 1) (no-estado no)))
                   
       )
    )
)
)
   






;; se nil, coloca no seguinte

;; verificar se indices excedem o array

