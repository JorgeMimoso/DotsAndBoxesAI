;;;; Ficheiro de Procura - Trata da implementação dos algoritmos de procura e suas funções auxiliares
;;;; Projeto IA Fase 1 - Dots and Boxes
;;;; Autores: Guilherme Martins 201802243, Jorge Mimoso (numero)


;;###################################AUXILIARES#############################################

(defun no-teste ()
     '(
       (
        ((0 0 0) (0 0 0) (0 0 0) (1 0 0) (1 0 0)) 
        ((0 0 0 0) (0 0 0 0) (1 0 0 0) (1 0 0 0))
       ) 0 nil)
)

(defun no-teste2 ()
     '(
       (
        ((0 0 0 0) (0 0 0 0) (0 0 0 1) (0 0 0 1)) 
        ((0 0 0) (0 0 0) (0 0 0) (0 0 1) (0 0 1))
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
(defun caixa-fechadap (no line col)
     (cond
         ((zerop (get-arco-na-posicao line col (get-arcos-horizontais (no-estado no)))) nil)
         ((and
             (> (length (get-arcos-horizontais (no-estado no))) (length (get-arcos-verticais (no-estado no))))
             (>= line (length (get-arcos-verticais (no-estado no))))
             (not (zerop (get-arco-na-posicao line col (get-arcos-horizontais (no-estado no)))))
             (not (zerop (get-arco-na-posicao (1+ line) col (get-arcos-horizontais (no-estado no)))))
             (not (zerop (get-arco-na-posicao (1- line) (1+ col) (get-arcos-verticais (no-estado no)))))
             (not (zerop (get-arco-na-posicao line (1+ col) (get-arcos-verticais (no-estado no)))))
          ) t)
         ((and
             (> (length (get-arcos-verticais (no-estado no))) (length (get-arcos-horizontais (no-estado no))))
             (= line (1- (length (get-arcos-horizontais (no-estado no)))))
             (not (zerop (get-arco-na-posicao line col (get-arcos-horizontais (no-estado no)))))
             (not (zerop (get-arco-na-posicao (1+ line) col (get-arcos-horizontais (no-estado no)))))
             (not (zerop (get-arco-na-posicao (1+ line) (1- col) (get-arcos-verticais (no-estado no)))))
             (not (zerop (get-arco-na-posicao (+ line 2) (1- col) (get-arcos-verticais (no-estado no)))))
          ) t)
         ((and
             (> (length (get-arcos-horizontais (no-estado no))) (length (get-arcos-verticais (no-estado no))))
             (>= line (length (get-arcos-verticais (no-estado no))))
             (= col 1)
             (not (zerop (get-arco-na-posicao line col (get-arcos-horizontais (no-estado no)))))
             (not (zerop (get-arco-na-posicao (1+ line) col (get-arcos-horizontais (no-estado no)))))
             (not (zerop (get-arco-na-posicao (1- line) col (get-arcos-verticais (no-estado no)))))
             (not (zerop (get-arco-na-posicao line col (get-arcos-verticais (no-estado no)))))
          ) t)
         ((and
             
          )
         ((and
             (not (zerop (get-arco-na-posicao line col (get-arcos-horizontais (no-estado no)))))
             (not (zerop (get-arco-na-posicao (1+ line) col (get-arcos-horizontais (no-estado no)))))
             (not (zerop (get-arco-na-posicao line col (get-arcos-verticais (no-estado no)))))
             (not (zerop (get-arco-na-posicao (1+ line) col (get-arcos-verticais (no-estado no)))))
          ) t)
     )
)

;;(defun num-caixas-fechadas (no line col) )

