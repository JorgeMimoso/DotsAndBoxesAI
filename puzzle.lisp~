;;;; Ficheiro do Puzzle - Trata da criação do tabuleiro e funções auxiliares deste
;;;; Projeto IA Fase 1 - Dots and Boxes
;;;; Autores: Guilherme Martins 201802243, Jorge Mimoso (numero)

;;##############################################TABULEIRO####################################################

;;tabuleiro: cria um tabuleiro n*m
(defun tabuleiro (n m)
     (list (criar-arcos-tabuleiro (1+ n) m) (criar-arcos-tabuleiro (1+ m) n)) 
)

;;TODO: Ver se ha melhor maneira de fazer
;;criar-arcos-tabuleiro: cria uma lista de arcos do tabuleiro com value+1 listas
(defun criar-arcos-tabuleiro (count value)
     (cond
         ((not (zerop count)) (cons (criar-arcos value) (criar-arcos-tabuleiro (1- count) value)))
     )
)

;;criar-arcos: cria uma lista de posicoes de um arco do tabuleiro
(defun criar-arcos (count)
     (cond
         ((not (zerop count)) (cons 0 (criar-arcos (1- count))))
     )
)

;;show-tabuleiro: mostra o estado atual do tabuleiro
(defun show-tabuleiro (arcos-hrz arcos-vrt)
     (cond
         ((or (null arcos-hrz) (null arcos-vrt)) nil)
         (t (list arcos-hrz arcos-vrt))
     )
)

;;##############################################SELETORES###################################################

;;get-arcos-horizontais: retorna a lista com os arcos horizontais do tabuleiro
(defun get-arcos-horizontais (tabuleiro)
     (first tabuleiro)
)

;;get-arcos-verticais: retorna a lista com os arcos verticais do tabuleiro
(defun get-arcos-verticais (tabuleiro)
     (second tabuleiro)
)

;;get-arco-na-posicao: retorna o valor do arco numa posicao dada
(defun get-arco-na-posicao (line col arco-list)
     (nth (1- col) (nth (1- line) arco-list))
)

;;############################################AUXILIARES#####################################################

;;substituir: insere um arco no indice fornecido
(defun substituir (i arco-lista &optional (x 1))
     (cond
         ((null arco-lista) nil)
         ((= (1- i) 0) (cons x (rest arco-lista)))
         (t (cons (first arco-lista) (substituir (1- i) (rest arco-lista) x)))
     )
)

;;arco-na-posicao: insere um arco na posicao fornecido numa lista de arcos verticais ou horizontais do tabuleiro
(defun arco-na-posicao (line col arco-list &optional (x 1))
         (cond
             ((null arco-list) nil)
             ((= (1- line) 0) (append (list (substituir col (first arco-list) x)) (rest arco-list)))
             (t (cons (first arco-list) (arco-na-posicao (1- line) col (rest arco-list) x)))
         )
)

;;arco-horizontal: insere um arco na posicao fornecida na lista dos arcos horizontais
(defun arco-horizontal (line col tabuleiro &optional (x 1))
     (cond
         ((or (> line (length (get-arcos-horizontais tabuleiro))) (> col (length (first (get-arcos-horizontais tabuleiro))))) nil)
         ((not (zerop (get-arco-na-posicao line col (get-arcos-horizontais tabuleiro)))) nil)
         (t (list (arco-na-posicao line col (get-arcos-horizontais tabuleiro) x) (get-arcos-verticais tabuleiro)))
     )
)

;;arco-vertical: insere um arco na posicao fornecida na lista dos arcos verticais
(defun arco-vertical (line col tabuleiro &optional (x 1))
     (cond
         ((or (> line (length (get-arcos-verticais tabuleiro))) (> col (length (first (get-arcos-verticais tabuleiro))))) nil)
         ((not (zerop (get-arco-na-posicao line col (get-arcos-verticais tabuleiro)))) nil)
         (t (list (get-arcos-horizontais tabuleiro) (arco-na-posicao line col (get-arcos-verticais tabuleiro) x)))
     )
)