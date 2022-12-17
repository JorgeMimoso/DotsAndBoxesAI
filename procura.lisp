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
         ((= line (length (get-arcos-horizontais (no-estado no)))) 0)
         ((and 
             (or 
               (= col (length (get-arco-horizontal (no-estado no)))) 
               ;;(= col (length (get-arco-vertical (no-estado no)))) 
             )
             (not (caixa-fechada-hrzp no line col))
          )(+ 0 (num-caixas-fechadas no (1+ line) 1)))
         ((and 
             ;;(or 
               (= col (length (get-arco-horizontal (no-estado no)))) 
               ;;(= col (length (get-arco-vertical (no-estado no)))) 
             ;;)
             (caixa-fechada-hrzp no line col)
          ) (+ 1 (num-caixas-fechadas no (1+ line) 1)))
         ((caixa-fechada-hrzp no line col) (+ 1 (num-caixas-fechadas no line (1+ col))))
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

;(defun novo-sucessor (no func &optional (line 1) (col 1))
;     (cond
;         ((eq func 'arco-horizontal) (list (funcall func (no-estado no) line col) (+ (no-profundidade no) 1) no);)
;         ((eq func 'arco-vertical) (list (funcall func (no-estado no) col line) (+ (no-profundidade no) 1) no))
;     )
;)

;(defun sucessores (no ops alg &optional (line 1) (col 1) (g 2))
;     (cond
;         ((null ops) nil)
;         ((= (no-profundidade no) g) nil)
;         ((and (eq alg 'bfs) (not (equal no nil))) (cons (novo-sucessor no (car ops) line col) (sucessores no (cdr ops) alg line col g)))
;         ((and (eq alg 'dfs) (not (equal no nil))) (cons (novo-sucessor no (car ops) line col) (sucessores no (cdr ops) alg line col g)))
;     )
;)

(defun abertos-bfs (abertos sucessores)
  (append abertos sucessores)
)

(defun abertos-dfs (abertos sucessores)
  (append sucessores abertos)
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



;; implementacao lab 6
;(defun bfs (no-inicial solucaop sucessores operadores &optional abertos fechados)
;  (let* (
;         (abertos (abertos-bfs abertos 
;                       (remove nil (mapcar (lambda(x) (cond ((or (no-existep x fechados) (no-existep x abertos)) NIL) (T x))) (sucessores no-inicial operadores 'bfs)))
;                  )
;         )
;         (fechados (cons no-inicial fechados))
;        )
;  (cond ((remove nil (mapcar solucaop abertos)) (find-if solucaop abertos))
;        (T (bfs (car abertos) solucaop sucessores operadores (cdr abertos) fechados))
;  ))
;)

;(defun bfs (fnsuc &optional abertos fechados)
;     (cond
;         ((null abertos) nil)
;         (
;     )
;)


;(defun expande-no-bfs (no abertos)
;     (append abertos (sucessores no (operadores) 'bfs))
;)

;(defun bfs (no sol &optional (abertos '()) (fechados '()) (line 1) (col 1) (prof 1))
;     (cond
;         ((not (null abertos)) 
;            (cond
;                 ((no-solucaop no sol) no)
;                 (
;                  (verifica-suc-sol (expande-no no 'sucessores abertos fechados line col) sol)
;                  (get-suc-sol (expande-no no 'sucessores abertos fechados line col) sol)
;                 )
;                 (
;                  (and 
;                      (null (verifica-suc-sol (expande-no no 'sucessores abertos fechados line col) sol))
;                      (< (no-profundidade no) prof))
;                       (bfs 
;                         (first (expande-no no 'sucessores abertos fechados line col))
;                         sol
;                         (expande-no no 'sucessores abertos fechados line col)
;                         (append fechados (list no))
;                         line
;                         col
;                         prof
;                      ))
;                  (t (bfs (first (expande-no no 'sucessores abertos fechados line col))
;                          sol
;                          (expande-no no 'sucessores abertos fechados line col)
;                          (append fechados (list no))
;                          (first (verifica-col-line no line col))
;                          (second (verifica-col-line no line col))
;                          (1+ prof)
;                  )))
;         )
;         (t nil))
;)

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

;(defun bfs (no fnsol fnsuc &optional (abertos '()) (fechados '()))
;     (let ((novo-abertos (cond
;                        ((not (null no)) (append abertos (list no))
;                        (t abertos))
;                     )))
;        (cond
;            ((not (null (first novo-abertos)))
;               (cond
;                   ((funcall fnsol (first novo-abertos)) no)
;                   (let * (( (novo-fechados (append fechados (list (first novo-abertos))))  
;                             (sucs 
;                          ))                     
;                   )
;               ))
;        )
;     )
;)




