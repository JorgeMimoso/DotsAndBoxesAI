;;;; Ficheiro central do Projeto - Trata da interaÃƒÂ§ÃƒÂ£o com o utilizador, carregando e lendo os ficheiros necessÃƒÂ¡rios
;;;; Projeto IA Fase 1 - Dots and Boxes
;;;; Autores: Guilherme Martins 201802243, Jorge Mimoso 202000695

(defun menu()
"Mostra o menu"
  (progn
    (format t "~%###################################")
    (format t "~%#         DOTS AND BOXES          #")
    (format t "~%#                                 #")
    (format t "~%#     1 - Mostrar problemas       #")
    (format t "~%#     2 - Resolver problemas      #")
    (format t "~%#     3 - Sair                    #")
    (format t "~%#                                 #")
    (format t "~%###################################~%~%>")
  )
)

(defun menu-problemas()
"Mostra o menu inicial"
  (progn
    (format t "~%###################################")
    (format t "~%#         DOTS AND BOXES          #")
    (format t "~%#                                 #")
    (format t "~%#       Escolha o problema        #")
    (format t "~%#                                 #")
    (format t "~%#  1 - Prob A(1) fechar 3 caixas  #")
    (format t "~%#  2 - Prob B(2) fechar 7 caixas  #")
    (format t "~%#  3 - Prob C(3) fechar 10 caixas #")
    (format t "~%#  4 - Prob D(4) fechar 10 caixas #")
    (format t "~%#  5 - Prob E(5) fechar 20 caixas #")
    (format t "~%#  6 - Prob F(6) fechar 35 caixas #")
    (format t "~%#  7 - voltar                     #")   
    (format t "~%#                                 #")
    (format t "~%###################################~%~%>")
  )
   (let ((opt (read)))
           (if (or (not (numberp opt)) (> opt 7) (< opt 1)) (progn (format t "Insira uma opcao valida") (dotsandboxes))
           (ecase opt
              ('1 (menu-algoritmo (criar-no(nth (- opt 1)(read-boards)) )3))               
              ('2 (menu-algoritmo (criar-no(nth (- opt 1)(read-boards)) )7)) 
              ('3 (menu-algoritmo (criar-no(nth (- opt 1)(read-boards)) )10))
              ('4 (menu-algoritmo (criar-no(nth (- opt 1)(read-boards)) )10))
              ('5 (menu-algoritmo (criar-no(nth (- opt 1)(read-boards)) )20))
              ('6 (menu-algoritmo (criar-no(nth (- opt 1)(read-boards)) )35))
              ('7 (dotsandboxes))
              )))
)



(defun menu-algoritmo(no sol)
"Mostra o menu de algoritmos disponiveis"
  (progn
    (format t "~%###################################")
    (format t "~%#         DOTS AND BOXES          #")
    (format t "~%#                                 #")
    (format t "~%#       Escolha o algoritmo       #")
    (format t "~%#                                 #")
    (format t "~%#           1 - BFS               #")
    (format t "~%#           2 - DFS               #")
    (format t "~%#           3 - A* Standard       #")
    (format t "~%#           4 - A* Original       #")
    (format t "~%#           5 - voltar            #")
    (format t "~%#                                 #")  
    (format t "~%###################################~%~%>")
  )
   (let ((opt (read)))  
           (if (or (not (numberp opt)) (> opt 7) (< opt 1)) (progn (format t "Insira uma opcao valida") (dotsandboxes))
           (ecase opt
              ('1 (escreve-resultados (current-time) (no-estado no)  (no-estado( bfs no sol (list no ) )) (current-time) ))       
              ('2 (dfs no sol 100 (list no)))
              ('3 (aheuristica no sol (list no)))
              ('5 (progn (dotsandboxes)))
              )))
)




(defun escreve-resultados(inicio problema    sol fim )
   (with-open-file (file (get-results-path) :direction :output :if-exists :append :if-does-not-exist :create)
     
    
     (format file "~%~t Problema")
     (format file "~%~t  ~a"  problema)

     (format file  "~%~t Solucao")
     (format file  "~%~t ~a" sol) 

     (format file  "~%~t Inicio")
     (format file  "~%~t ~a" inicio) 

     (format file  "~%~t Fim")
     (format file  "~%~t ~a" fim) 
     
     (terpri)
 
  
   )
   (print (format t  "~%~t Problema"))
   (print (format t  "~%  ~a"  problema))

   (print (format t  "~%~t Solucao"))
   (print (format t  "~%~t  ~a"  sol))

   

   

   (dotsandboxes)
)


(defun current-time()
"Retorna o tempo actual com o formato (h m s)"
  ;;HORAS-MINUTOS-SEGUNDOS
  (multiple-value-bind (s m h) (get-decoded-time)
    (list h m s)
   )
)



(defun dotsandboxes()
"Inicia o programa"
  (progn
    (menu)
      (let ((opt (read)))
           (if (or (not (numberp opt)) (> opt 3) (< opt 1)) (progn (format t "Insira uma opcao valida") (dotsandboxes))
           (ecase opt
              ('1 (progn (lista-prob(read-boards))))               
              ('2 (progn (menu-problemas)))
              ('3 (format t "Programa terminado")))))
      )
)

(defun lista-prob(problemas &optional(counter 1))
  
  (cond 
   ((null problemas) (dotsandboxes))
   ((print-board (car problemas)) (print counter)  (terpri) (lista-prob (cdr problemas)(+ 1 counter ) ))
   
   )

)


(defun print-board(board)
  "Mostra um tabuleiro bem formatado"
   (not (null (mapcar #'(lambda(l) (format t "~%~t~t ~a" l)) board)))
)

(defun read-boards ()
"Retorna as boards no ficheiro problemas.dat"
   (with-open-file (file (get-problems-path) :if-does-not-exist nil)
     (do ((result nil (cons next result))
          (next (read file nil 'eof) (read file nil 'eof)))
                ((equal next 'eof) (reverse result))
     )
  )
)





(defun get-problems-path()
"Devolve o path para o ficheiro problemas.dat (C:\lisp\problemas.dat)"
    (make-pathname :host "c" :directory '(:absolute "lisp") :name "problemas" :type "dat")
)


(defun get-results-path()
"Devolve o path para o ficheiro (C:\lisp\log.dat)"
    (make-pathname :host "c" :directory '(:absolute "lisp") :name "log" :type "dat")
)

(defun write-statistics-file ()
  (with-open-file (file (get-results-path) :direction :output :if-exists :append :if-does-not-exist :create)
         (format file "write anything ~%")
 )
)       



(defun get-solution-path(node)
"Retorna uma lista de estados do root ao goal"
  (cond 
   ((null (get-node-parent node)) (list (car node)))
   (T (append (get-solution-path (get-node-parent node)) (list (car node))))
   )
)

(defun get-node-parent (node)
"Devolve o no pai de um no"
	(cadr node)
)
