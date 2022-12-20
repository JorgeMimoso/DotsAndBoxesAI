;;;; Ficheiro de Procura - Trata da implementação dos algoritmos de procura e suas funções auxiliares
;;;; Projeto IA Fase 1 - Dots and Boxes
;;;; Autores: Guilherme Martins 201802243, Jorge Mimoso (numero)


;;###################################AUXILIARES#############################################
(defun tabuleiro-teste ()
  "Retorna um tabuleiro 3x3 (3 arcos na vertical por 3 arcos na horizontal)"
	'(
		((0 0 0) (0 0 1) (0 1 1) (0 0 1))
		((0 0 0) (0 1 1) (1 0 1) (1 1 1))
	)
)

(defun no-tabuleiro-teste ()
     '(
       (
        ((1 1 1) (1 0 1) (0 1 1) (0 0 1)) 
        ((1 1 1) (1 1 1) (1 1 1) (0 1 1))
       ) 0 nil)
)



(defun no-teste ()
     '(
       (
        ((0 0 0) (0 0 0) (0 0 0) (0 0 1) (0 0 1)) 
        ((0 0 0 0) (0 0 0 0) (0 0 0 1) (0 0 0 1))
       ) 0 nil)
)

(defun no-teste-solucao ()
     '(
       (
        ((1 1 1) (1 1 1) (1 1 1) (1 1 1) (1 1 1)) 
        ((1 1 1 1) (1 1 1 1) (1 1 1 1) (1 1 1 1))
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

(defun no-teste7 ()
   '(
       (
        ((1 1 1) (1 1 1) (1 1 1) (1 1 1) (1 1 1))
        ((1 1 1 1) (1 1 1 1) (1 1 1 1) (1 1 1 1))
       ) 0 nil)
)

(defun no-teste8 ()
     '(
       (
        ((1 1 1 1) (1 1 1 1) (1 1 1 1) (1 1 1 1))
        ((1 1 1) (1 1 1) (1 1 1) (1 1 1) (1 1 1))
       ) 0 nil)
)

;;############################################# CRIAÇÃO DUM NÓ  #############################################

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

;;############################################# FUNÇÕES AUXILIARES #############################################

;;REVER
(defun caixa-fechadap (no line col)
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

;;Fixed -> actualizei apenas para verificar numa direção (horizontal), se verificarmos as duas direções, conta caixas repetidas
(defun num-caixas-fechadas (no &optional (line 1) (col 1)) 
     (cond
         ((= line (length (get-arcos-horizontais (no-estado no)))) 0)
         ((and 
             (= col (length (get-arco-horizontal (no-estado no)))) 
             (not (caixa-fechadap no line col))
          )(+ 0 (num-caixas-fechadas no (1+ line) 1)))
         ((and 
             (= col (length (get-arco-horizontal (no-estado no)))) 
             (caixa-fechadap no line col)
          ) (+ 1 (num-caixas-fechadas no (1+ line) 1)))
         ((caixa-fechadap no line col) (+ 1 (num-caixas-fechadas no line (1+ col))))
         (t (+ 0 (num-caixas-fechadas no line (1+ col))))
     )
)

(defun no-solucaop(no &optional(solucao 1))
  (cond
      ((= (num-caixas-fechadas no) solucao) t)
      (t nil)
  )
)


(defun operadores ()
 "Cria uma lista com  os operadores do problema."
 (list 'arco-horizontal 'arco-vertical)
)



(defun novo-sucessor(no op &optional (line 1)(col 1))   
  (cond
    ((equal op 'arco-horizontal) 
      (cond        
          ((< (length (car (car(no-estado no )))) col ) (novo-sucessor no 'arco-horizontal (+ 1 line) 1)) 
          ((< (length (car (no-estado no))  ) line) nil)       
          ((equal (arco-horizontal line col (no-estado no))  nil) (novo-sucessor no 'arco-horizontal line (+ 1 col)))         
          (t  (criar-no  (arco-horizontal line col (no-estado no)) (+ (no-profundidade no) 1) (no-estado no)))               
      )
    )
     ((equal op 'arco-vertical) 
      (cond          
       ((< (length (car (car(no-estado no )))) line ) (novo-sucessor no 'arco-vertical 1 (+ 1 col))) 
       ((< (length (car (no-estado no))  ) col) nil)       
       ((equal (arco-vertical line col (no-estado no))  nil) (novo-sucessor no 'arco-vertical (+ 1 line)col ))         
       (t  (criar-no  (arco-vertical line col (no-estado no)) (+ (no-profundidade no) 1) (no-estado no)))                  
       )
     )
   )
)

;; Terminado->resultados não são visiveis num nivel de debug   
(defun sucessores(no oplist algoritmo &optional maxprofundidade)
  (cond ((and (equal algoritmo 'dfs) (= maxprofundidade (no-profundidade no))) NIL)
        (T ( mapcar #'(lambda(func) (novo-sucessor no func)) oplist)  )
  )
)

(defun abertos-bfs (abertos sucessores)
     (append abertos sucessores)
)

(defun abertos-dfs (abertos sucessores)
     (append sucessores abertos)
)

(defun abertos-heuristica (abertos sucessores sol)
     (organiza-sucs (abertos-bfs abertos sucessores) sol)
)


;;Implementacao Lab 6
(defun no-existep2 (no lista)
   (not (null (find (no-estado no) (mapcar #'no-estado lista))))
)

(defun no-existep (no no-list &optional algoritmo)
     (cond
         ((null no-list) nil)
         ((or (null (no-estado no)) (null (no-estado (first no-list)))) t)
         ((and (equal (no-estado no) (no-estado (first no-list))) (eq algoritmo 'bfs)) t)
         ((and (equal (no-estado no) (no-estado (first no-list))) (= (no-profundidade (first no-list)))) t)
         (t (no-existep no (rest no-list) algoritmo))
     )
)

;;############################################# ALGORITMOS #############################################

(defun bfs (no sol &optional (abertos '()) (fechados '()))
     (cond
         ((not (null abertos)) 
            (cond
                 ((no-solucaop no sol) no)
                 (
                  (verifica-suc-sol (expande-no-bfs no 'sucessores abertos fechados) sol)
                  (get-suc-sol (expande-no-bfs no 'sucessores abertos fechados) sol)
                 )
                 (
                      (null (verifica-suc-sol (expande-no-bfs no 'sucessores abertos fechados) sol))
                       (bfs 
                         (first (expande-no-bfs no 'sucessores abertos fechados))
                         sol
                         (expande-no-bfs no 'sucessores abertos fechados)
                         (append fechados (list no))
                      ))
           )
         )
         (t nil))
)

(defun dfs (no sol prof &optional (abertos '()) (fechados '()))
     (cond
         ((not (null abertos))
            (cond
                ((no-solucaop no sol) no)
                (
                 (verifica-suc-sol (expande-no-dfs no 'sucessores abertos fechados prof) sol)
                 (get-suc-sol (expande-no-dfs no 'sucessores abertos fechados prof) sol)
                )
                (
                    (null (verifica-suc-sol (expande-no-dfs no 'sucessores abertos fechados prof) sol))
                    (dfs
                       (first (expande-no-dfs no 'sucessores abertos fechados prof))
                       sol
                       prof
                       (expande-no-dfs no 'sucessores abertos fechados prof)
                       (append fechados (list no))
                    ))
            )
        ) (t nil))
)

;;esta a ignorar outros nos potenciais
(defun aheuristica (no sol &optional (abertos '()) (fechados '()))
     (cond
         ((not (null abertos))
            (cond
                ((no-solucaop no sol) no)
                (
                  (verifica-suc-sol (expande-no-heuristica no sol 'sucessores abertos fechados) sol)
                  (get-suc-sol (expande-no-heuristica no sol 'sucessores abertos fechados) sol)
                )
                (
                  (null (verifica-suc-sol (expande-no-heuristica no sol 'sucessores abertos fechados) sol))
                  (aheuristica
                             (first (expande-no-heuristica no sol 'sucessores abertos fechados))
                             sol
                             (expande-no-heuristica no sol 'sucessores abertos fechados)
                             (append fechados (list no))
                  )
                )
            )
     ) (t nil))
)



(defun expande-no-bfs (no fnsuc abertos fechados)
     (verifica-sucs-dupls 
                       (abertos-bfs (rest abertos) (funcall fnsuc no (operadores) 'bfs))
                       (append fechados (list no)) 'bfs)
)

(defun expande-no-dfs (no fnsuc abertos fechados prof)
     (verifica-sucs-dupls
                       (abertos-dfs (rest abertos) (funcall fnsuc no (operadores) 'dfs prof))
                       (append fechados (list no)) 'dfs)
)

(defun expande-no-heuristica (no sol fnsuc abertos fechados)
     (verifica-sucs-dupls
                       (abertos-heuristica (rest abertos) (funcall fnsuc no (operadores) 'bfs) sol)
                       (append fechados (list no)) 'bfs)
)

(defun heuristica (no sol)
     (- sol (num-caixas-fechadas no))
)

(defun organiza-sucs (sucs sol)
     (cond
         ((null sucs) nil)
         (t (append
                  (organiza-sucs (menor-heuristica-sucs (first sucs) (rest sucs) sol) sol)
                  (list (first sucs))
                  (organiza-sucs (maior-heuristica-sucs (first sucs) (rest sucs) sol) sol)
            )
         )
     )
)

(defun menor-heuristica-sucs (no sucs sol)
     (cond
         ((null sucs) nil)
         (t (cond
                ((< (heuristica (first sucs) sol) (heuristica no sol))
                 (cons (first sucs) (menor-heuristica-sucs no (rest sucs) sol)))
                (t (menor-heuristica-sucs no (rest sucs) sol))
            ))
     )
)

(defun maior-heuristica-sucs (no sucs sol)
     (cond
         ((null sucs) nil)
         (t (cond
                ((>= (heuristica (first sucs) sol) (heuristica no sol))
                 (cons (first sucs) (maior-heuristica-sucs no (rest sucs) sol)))
                (t (maior-heuristica-sucs no (rest sucs) sol))
            ))
     )
)



;(defun heuristica-sucs (sucs sol)
;     (let ((lista (organiza-sucs sucs sol)))
;        (list (first lista) (second lista))
;     )
;)

;(defun organiza-sucs (sucs sol)
;     (cond
;         ((null sucs) nil)
;         ((no-menor-quep (first sucs) sucs sol) (cons (first sucs) (organiza-sucs (rest sucs) sol)))
;         (t (organiza-sucs (rest sucs) sol))
;     )
;)

;(defun no-menor-quep (no sucs sol)
;     (cond
;         ((null sucs) t)
;         ((<= (heuristica no sol) (heuristica (first sucs) sol)) (no-maior-quep no (rest sucs) sol))
;         (t nil)
;     )
;)

(defun verifica-sucs-dupls (abertos fechados algoritmo)
     (remove nil (mapcar #'(lambda(x)
                                 (cond
                                     ((no-existep x fechados algoritmo) nil)
                                     (t x)
                                 )) abertos))
)

(defun verifica-suc-sol (sucs sol)
     (cond
         ((null sucs) nil)
         ((no-solucaop (first sucs) sol) t)
         (t (verifica-suc-sol (rest sucs) sol))
     )
)

(defun get-suc-sol (sucs sol)
    (cond 
        ((verifica-suc-sol sucs sol) (first sucs))
        (t (get-suc-sol (rest sucs) sol))
    )
)

(defun verifica-col-line (no line col)
     (cond
         ((< col (length (get-arco-horizontal (no-estado no)))) (list line (1+ col)))
         (t (list (1+ line) 1))
     )
)
