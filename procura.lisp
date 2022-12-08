;;;; Ficheiro de Procura - Trata da implementação dos algoritmos de procura e suas funções auxiliares
;;;; Projeto IA Fase 1 - Dots and Boxes
;;;; Autores: Guilherme Martins 201802243, Jorge Mimoso (numero)


;;###################################AUXILIARES#############################################

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

;;esta a retornar 1+ do que suposto
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
             (not (caixa-fechadap no line col))
          )(+ 0 (num-caixas-fechadas no (1+ line) 1)))
         ((and 
             (or 
               (= col (length (get-arco-horizontal (no-estado no)))) 
               (= col (length (get-arco-vertical (no-estado no)))) 
             )
             (caixa-fechadap no line col)
          )(+ 1 (num-caixas-fechadas no (1+ line) 1)))
         ((caixa-fechadap no line col) (+ 1 (num-caixas-fechadas no line (1+ col))))
         (t (+ 0 (num-caixas-fechadas no line (1+ col))))
     )
)

