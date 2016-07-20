;
; Entrega 1 Projecto Inteligencia Artificial
; Grupo 24
; Joao Ribeiro  77209
; Fernando Lica 77207  
;

;  ##################################################################################################
; #--------------------------------------------------------------------------------------------------#
; #--------------------------------------------------------------------------------------------------#
; #------------------------PRIMEIRA PARTE DO PROJETO - PROCURAS E HEURISTICAS------------------------#
; #--------------------------------------------------------------------------------------------------#
; #--------------------------------------------------------------------------------------------------#
;  ##################################################################################################

;  ###########
; # TAI ACCAO #
;  ###########

; Construtor
(defun cria-accao (coluna peca) ; inteiro x array : accao
	(cons coluna peca)
)

; Selectores
(defun accao-coluna (accao) ; accao : inteiro
	(first accao)
)

(defun accao-peca (accao) ; accao : array
	(cdr accao)
)

;  ###############
; # TAI TABULEIRO #
;  ############### 

; Construtor
(defun cria-tabuleiro ()
	(make-array '(10 18) :initial-element nil)
)

; Selectores
(defun copia-tabuleiro (tab-antigo)
	(let ((tab-novo (cria-tabuleiro)))
		(dotimes (i 10)
   			(dotimes (j 18)
    			(setf (aref tab-novo i j) (aref tab-antigo i j))
    		)
    	)
    	tab-novo ; Return
	)
)

(defun tabuleiro-preenchido-p (tab linha coluna)
	(aref tab coluna linha) ; Return
)

(defun tabuleiro-altura-coluna (tab coluna)
	(let ((altura 18))
		(dotimes (j 18) 
			
			(when (tabuleiro-preenchido-p tab (- 17 j) coluna ) ; Caso o tabuleiro tiver preenchido na linha actual, sai do loop
				(return)
			)
			(decf altura)
		)
		altura ; Return valor que ficou na altura
	)
)

; Reconhecedores
(defun tabuleiro-linha-completa-p (tab linha)
	(let ((preenchida T)) ; Variavel auxiliar que se inicia a T ate encontrar um valor nil
		(dotimes (col 10)
			(when (not (tabuleiro-preenchido-p tab linha col)) ; Caso posicao atual seja nil 
				(setf preenchida nil) ; Entao a linha nao esta completa
				(return)
			)
		)
		preenchida ; Return
	)
)

(defun tabuleiro-topo-preenchido-p (tab)
	(let ((p nil))
		(dotimes (c 10)
			(when (= (tabuleiro-altura-coluna tab c) 18) ; Se altura da linha = 18
				(setf p T) ; Topo esta preenchido
				(return)
			)
		)
		p ; Return
	)
)

; Modificadores
(defun tabuleiro-preenche! (tab linha coluna)
	(when (and (and (> linha -1) (< linha 18)) (and (> coluna -1) (< coluna 10)))
		(setf (aref tab coluna linha) T)
	)
)

(defun tabuleiro-remove-linha! (tab linha)
	(loop for l from linha to 16 do ; um loop desda linha atual, que vai ser retirada, e as de cima vao cair todas
		(dotimes (c 10)
			(setf (aref tab c l) (aref tab c (+ l 1))) ; Mete na linha actual o valor da linha de cima
		)
	)
	(dotimes (c 10)
		(setf (aref tab c 17) nil) ; Linha do topo com tudo vazio sempre
	)
)

; Testes
(defun tabuleiros-iguais-p (tab1 tab2)
	(let ((iguais T))
		(dotimes (l 18)
			(when iguais
				(dotimes (c 10)
					(when (not (equal (aref tab1 c l) (aref tab2 c l)))
						(setf iguais nil)
						(return)
					)
				)
			)
		)
		iguais ; Return
	)
)

; Transformadores
(defun tabuleiro->array (tab)
	(let ((tab-array (make-array '(18 10) :initial-element nil)))
		(dotimes (i 18)
   			(dotimes (j 10)
    			(setf (aref tab-array i j) (aref tab j i))
    		)
    	)
    	tab-array ; Return
	)
)

(defun array->tabuleiro (tab-array)
	(let ((tab-novo (cria-tabuleiro)))
		(dotimes (i 18)
   			(dotimes (j 10)
    			(setf (aref tab-novo j i) (aref tab-array i j))
    		)
    	)
    	tab-novo ; Return
	)
)


;  ############
; # TAI ESTADO #
;  ############

; Construtor
(defstruct estado
	(pontos 0)
	(pecas-por-colocar '())
	(pecas-colocadas '())
	(Tabuleiro (cria-tabuleiro))
)

(defun copia-estado (estado)
	(let ((estado-novo (make-estado)))
		
		(setf (estado-pontos estado-novo) (estado-pontos estado))
		(setf (estado-pecas-por-colocar estado-novo) (copy-list (estado-pecas-por-colocar estado))) 	; Copiar lista/ELEMENTOS 1 a 1
		(setf (estado-pecas-colocadas estado-novo) (copy-list (estado-pecas-colocadas estado)))		; Copiar lista/ELEMENTOS 1 a 1
		(setf (estado-Tabuleiro estado-novo) (copia-tabuleiro (estado-Tabuleiro estado)))
		
		estado-novo
	)
)

; Reconhecedores
(defun estado-final-p (estado)
	(let ((sem-pecas nil) (topo-atingido nil))
		(setf sem-pecas (equal (estado-pecas-por-colocar estado) '()))
		(setf topo-atingido (tabuleiro-topo-preenchido-p (estado-Tabuleiro estado)))
		(or sem-pecas topo-atingido)
	)
)

; Testes
(defun estados-iguais-p (e1 e2)
	(let ((pontos-iguais nil) (pecas-por-colocar-iguais nil) (pecas-colocadas-iguais nil) (tabs-iguais nil))
		(setf pontos-iguais (= (estado-pontos e1) (estado-pontos e2)))
		(setf pecas-por-colocar-iguais (listas-iguais (estado-pecas-por-colocar e1) (estado-pecas-por-colocar e2)))
		(setf pecas-colocadas-iguais (listas-iguais (estado-pecas-colocadas e1) (estado-pecas-colocadas e2)))
		(setf tabs-iguais (tabuleiros-iguais-p (estado-Tabuleiro e1) (estado-Tabuleiro e2)))
		(and (and pontos-iguais pecas-por-colocar-iguais) (and pecas-colocadas-iguais tabs-iguais))
	)
)

;  ############## 
; # TAI PROBLEMA #
;  ##############

; Construtor
(defstruct problema
	(estado-inicial (make-estado))
	(solucao nil) 
	(accoes '())
	(resultado (make-estado))
	(custo-caminho 0)
)

(defun formulacao-problema (tab pecas-novas)
	(make-problema :estado-inicial (make-estado :tabuleiro tab :pecas-por-colocar pecas-novas))
)

;  #########
; # FUNCOES #
;  #########

(defun solucao (estado)
	(let ((sem-pecas nil) (topo-livre nil))
		; Ver se o tabuleiro tem topo atingido
		; Ver se a lista de pecas por colocar e vazia
		(setf sem-pecas (equal (estado-pecas-por-colocar estado) '()))
		(setf topo-livre (not (tabuleiro-topo-preenchido-p (estado-Tabuleiro estado))))
		(and sem-pecas topo-livre)
	)
)

(defun accoes (estado)
	(let (

		(accoes-possiveis '())

		)

		(when (not (tabuleiro-topo-preenchido-p (estado-Tabuleiro estado)))
		; 1 - Vai ver a lista de pecas por colocar : dotimes length cada peca
			(cond
				((eq (nth 0 (estado-pecas-por-colocar estado)) 'i )
					(dotimes (orient 2)
						(setf accoes-possiveis (append accoes-possiveis (accoes-possiveis-peca (parse-peca 'i orient))))
					)
				)
				((eq (nth 0 (estado-pecas-por-colocar estado)) 'l )
					(dotimes (orient 4)
						(setf accoes-possiveis (append accoes-possiveis (accoes-possiveis-peca (parse-peca 'l orient))))
					)
				)
				((eq (nth 0 (estado-pecas-por-colocar estado)) 'j )
					(dotimes (orient 4)
						(setf accoes-possiveis (append accoes-possiveis (accoes-possiveis-peca (parse-peca 'j orient))))
					)
				)
				((eq (nth 0 (estado-pecas-por-colocar estado)) 'o )
					(dotimes (orient 1)
						(setf accoes-possiveis (append accoes-possiveis (accoes-possiveis-peca (parse-peca 'o orient))))
					)
				)
				((eq (nth 0 (estado-pecas-por-colocar estado)) 's )
					(dotimes (orient 2)
						(setf accoes-possiveis (append accoes-possiveis (accoes-possiveis-peca (parse-peca 's orient))))
					)
				)
				((eq (nth 0 (estado-pecas-por-colocar estado)) 'z )
					(dotimes (orient 2)
						(setf accoes-possiveis (append accoes-possiveis (accoes-possiveis-peca (parse-peca 'z orient))))
					)
				)
				((eq (nth 0 (estado-pecas-por-colocar estado)) 't )
					(dotimes (orient 4)
						(setf accoes-possiveis (append accoes-possiveis (accoes-possiveis-peca (parse-peca 't orient))))
					)
				)
			)
		)
		accoes-possiveis
	)
)

(defun resultado (estado accao)
	(let ((estado-resultado (copia-estado estado)) (peca 'i) (linhas-feitas 0))
		; Tira a primeira peca da lista de pecas por colocar e meta na lista de pecas colocadas
		

		(setf peca (pop (estado-pecas-por-colocar estado-resultado)))
		
		(setf (estado-pecas-colocadas estado-resultado) (push peca (estado-pecas-colocadas estado-resultado)))

		; Coloca a peca no tabuleiro do estado
		(setf (estado-tabuleiro estado-resultado) (coloca-peca-tabuleiro (estado-Tabuleiro estado-resultado) (accao-coluna accao) (accao-peca accao)))
		; Se o topo nao estiver preenchido, calcula pontos

		(when (not (tabuleiro-topo-preenchido-p (estado-tabuleiro estado-resultado)))
			
			(setf linhas-feitas 0)
			
			(dotimes (linha 18)
				(when (tabuleiro-linha-completa-p (estado-tabuleiro estado-resultado) linha)
					(incf linhas-feitas)
					(tabuleiro-remove-linha! (estado-tabuleiro estado-resultado) linha)
					(decf linha)
				)
			)

			; Remove as linhas completas
			; Conta o numero de linhas completas
		)
		(cond 
			((= linhas-feitas 1) (setf (estado-pontos estado-resultado) (+ (estado-pontos estado-resultado) 100)))
			((= linhas-feitas 2) (setf (estado-pontos estado-resultado) (+ (estado-pontos estado-resultado) 300)))
			((= linhas-feitas 3) (setf (estado-pontos estado-resultado) (+ (estado-pontos estado-resultado) 500)))
			((= linhas-feitas 4) (setf (estado-pontos estado-resultado) (+ (estado-pontos estado-resultado) 800)))
			((> linhas-feitas 4) (setf (estado-pontos estado-resultado) (+ (estado-pontos estado-resultado) 800)))

		)
		estado-resultado
	)
)

(defun qualidade (estado)
	(let ((custo 0) (aux 0))
		(setf aux (estado-pontos estado))	; Aux = pontos positivos
		(setf custo (- aux (* aux 2)))		; custo = aux - 2aux <=> (custo = - aux)
		custo
	)
)

(defun custo-oportunidade (estado)
	(let ((custo-op 0) (max-possivel 0) (pontos-obtidos (estado-pontos estado)))
		(dotimes (i (length (estado-pecas-colocadas estado)))
			(cond ((eq (nth i (estado-pecas-colocadas estado)) 'i ) (setf max-possivel (+ max-possivel 800)))
				((eq (nth i (estado-pecas-colocadas estado)) 'j ) (setf max-possivel (+ max-possivel 500)))
				((eq (nth i (estado-pecas-colocadas estado)) 'l ) (setf max-possivel (+ max-possivel 500)))
				((eq (nth i (estado-pecas-colocadas estado)) 's ) (setf max-possivel (+ max-possivel 300)))
				((eq (nth i (estado-pecas-colocadas estado)) 'z ) (setf max-possivel (+ max-possivel 300)))
				((eq (nth i (estado-pecas-colocadas estado)) 't ) (setf max-possivel (+ max-possivel 300)))
				((eq (nth i (estado-pecas-colocadas estado)) 'o ) (setf max-possivel (+ max-possivel 300)))
			)
		)
		(setf custo-op (- max-possivel pontos-obtidos))
		custo-op
	)
)

;  ############
; # AUXILIARES #
;  ############

(defun parse-peca (peca orientacao)
	; Esta funcao recebe uma peca pela letra e a sua orientacao (0, 1, 2, 3)
	; e devolve a peca correspondente do utils
	(cond 
		; i
		((and (eq peca 'i) (= orientacao 0)) peca-i0)
		((and (eq peca 'i) (= orientacao 1)) peca-i1)
		; l
		((and (eq peca 'l) (= orientacao 0)) peca-l0)
		((and (eq peca 'l) (= orientacao 1)) peca-l1)
		((and (eq peca 'l) (= orientacao 2)) peca-l2)
		((and (eq peca 'l) (= orientacao 3)) peca-l3)
		; j
		((and (eq peca 'j) (= orientacao 0)) peca-j0)
		((and (eq peca 'j) (= orientacao 1)) peca-j1)
		((and (eq peca 'j) (= orientacao 2)) peca-j2)
		((and (eq peca 'j) (= orientacao 3)) peca-j3)
		; o
		((and (eq peca 'o) (= orientacao 0)) peca-o0)
		; s
		((and (eq peca 's) (= orientacao 0)) peca-s0)
		((and (eq peca 's) (= orientacao 1)) peca-s1)
		; z
		((and (eq peca 'z) (= orientacao 0)) peca-z0)
		((and (eq peca 'z) (= orientacao 1)) peca-z1)
		; t
		((and (eq peca 't) (= orientacao 0)) peca-t0)
		((and (eq peca 't) (= orientacao 1)) peca-t1)
		((and (eq peca 't) (= orientacao 2)) peca-t2)
		((and (eq peca 't) (= orientacao 3)) peca-t3)
	)
)

(defun coloca-peca-tabuleiro (tab coluna peca)
	
	; Esta funcao recebe um tabuleiro, a coluna onde colocar a peca e a peca em si
	; Cria uma hitbox, que corresponde a uma marca a volta da peca
	; Cria uma variavel altura, que corresponde a base da peca onde sera colocada
	
	(let ((hitbox (make-array (array-dimensions peca) :initial-element nil ))
		(altura 0)
		)
		
		(setf altura (altura-inicial-otimizada tab coluna peca))
		(setf hitbox (join-hitbox hitbox tab coluna altura)) ; inicializa a hitbox na base do tabuleiro

		(loop while (or (not (hitbox-match peca hitbox)) (not (altura-match peca tab coluna altura))) ; Vai subindo ate encaixar a hitbox

			do(setf altura (+ altura 1))
				(setf hitbox (join-hitbox hitbox tab coluna altura))
		)

		
		; Colocar a peca no local onde esta a hitbox
		; ou seja, preencher o tabuleiro
		(dotimes (x (array-dimension hitbox 1))
			(dotimes (y (array-dimension hitbox 0))
				(when (and (aref peca y x) (< (+ altura y) 18))
					(tabuleiro-preenche! tab (+ altura y) (+ coluna x))
				)
			)
		)
		tab
	)
)

(defun altura-inicial-otimizada (tab coluna peca)
	(let (
			(numColunas (array-dimension peca 1))
			(hMin 99)
		)

		(dotimes (i numColunas)
			(when (< (tabuleiro-altura-coluna tab (+ coluna i)))
				(setf hMin (tabuleiro-altura-coluna tab (+ coluna i)))
			)
		)

		(max (- hMin 1) 0)
	)
)

(defun altura-match (peca tab coluna base-hitbox)
	(let (
		(length-peca (array-dimension peca 1))
		(top-peca-abs (+ base-hitbox (array-dimension peca 0)))
		(match T)
		)

		(dotimes (i length-peca)
			(when (> (tabuleiro-altura-coluna tab (+ coluna i)) top-peca-abs)
				(setf match nil)
				(return)
			)
		)
		match
	)
)

(defun join-hitbox (hitbox tab coluna altura)

	; Esta funcao devolve uma hitbox do tabuleiro,
	; Ou seja posicoes do tabuleiro em que vamos testar a peca

	(dotimes (x (array-dimension hitbox 1))
		(dotimes (y (array-dimension hitbox 0))

			(cond ((> (+ altura y) 17) (setf (aref hitbox y x) nil))
				((<= (+ altura y) 17) (setf (aref hitbox y x) (tabuleiro-preenchido-p tab (+ altura y) (+ coluna x))))
			)
		)
	)
	hitbox
)

(defun hitbox-match (peca hitbox)
	(let ((compativel T))
		(dotimes (x (array-dimension peca 1))
			(when compativel
				(dotimes (y (array-dimension peca 0))
					(when (and (aref peca y x) (aref hitbox y x))
						(setf compativel nil)
						(return)
					)
				)
			)
		)
		compativel
	)
)


(defun accoes-possiveis-peca (peca)

	; Uma accao e possivel se a peca for colocada e nao ultrapasse a coluna 9
	; Cada peca pode ser colocada desde a casa 0 ate ao local onde nao passe o fim do tabuleiro
	; ex peca i1 pode ser colocada em 0,1,2,3,4,5,6. Se colocada em 7 a parte mais a direita ficava fora

	(let ((lista-possiveis (list 'lixo)))
		(cond 
			((equal peca peca-i0) 
				(dotimes (i 10)
					(setf lista-possiveis (push-fim lista-possiveis (cria-accao i peca-i0)))
				)
			)
			((equal peca peca-i1)
				(dotimes (i 7)
					(setf lista-possiveis (push-fim lista-possiveis (cria-accao i peca-i1)))
				)
			)
			((equal peca peca-l0)
				(dotimes (i 9)
					(setf lista-possiveis (push-fim lista-possiveis (cria-accao i peca-l0)))
				)
			)
			((equal peca peca-l1)
				(dotimes (i 8)
					(setf lista-possiveis (push-fim lista-possiveis (cria-accao i peca-l1)))
				)
			)
			((equal peca peca-l2) 
				(dotimes (i 9)
					(setf lista-possiveis (push-fim lista-possiveis (cria-accao i peca-l2)))
				)
			)
			((equal peca peca-l3)
				(dotimes (i 8)
					(setf lista-possiveis (push-fim lista-possiveis (cria-accao i peca-l3)))
				)
			)

			((equal peca peca-j0)
				(dotimes (i 9)
					(setf lista-possiveis (push-fim lista-possiveis (cria-accao i peca-j0)))
				)
			)
			((equal peca peca-j1)
				(dotimes (i 8)
					(setf lista-possiveis (push-fim lista-possiveis (cria-accao i peca-j1)))
				)
			)
			((equal peca peca-j2)
				(dotimes (i 9)
					(setf lista-possiveis (push-fim lista-possiveis (cria-accao i peca-j2)))
				)
			)
			((equal peca peca-j3)
				(dotimes (i 8)
					(setf lista-possiveis (push-fim lista-possiveis (cria-accao i peca-j3)))
				)
			)

			((equal peca peca-o0) 
				(dotimes (i 9)
					(setf lista-possiveis (push-fim lista-possiveis (cria-accao i peca-o0)))
				)
			)

			((equal peca peca-s0) 
				(dotimes (i 8)
					(setf lista-possiveis (push-fim lista-possiveis (cria-accao i peca-s0)))
				)
			)
			((equal peca peca-s1) 
				(dotimes (i 9)
					(setf lista-possiveis (push-fim lista-possiveis (cria-accao i peca-s1)))
				)
			)

			((equal peca peca-z0) 
				(dotimes (i 8)
					(setf lista-possiveis (push-fim lista-possiveis (cria-accao i peca-z0)))
				)
			)
			((equal peca peca-z1) 
				(dotimes (i 9)
					(setf lista-possiveis (push-fim lista-possiveis (cria-accao i peca-z1)))
				)
			)	
			((equal peca peca-t0) 
				(dotimes (i 8)
					(setf lista-possiveis (push-fim lista-possiveis (cria-accao i peca-t0)))
				)
			)
			((equal peca peca-t1) 
				(dotimes (i 9)
					(setf lista-possiveis (push-fim lista-possiveis (cria-accao i peca-t1)))
				)
			)
			((equal peca peca-t2) 
				(dotimes (i 8)
					(setf lista-possiveis (push-fim lista-possiveis (cria-accao i peca-t2)))
				)
			)
			((equal peca peca-t3) 
				(dotimes (i 9)
					(setf lista-possiveis (push-fim lista-possiveis (cria-accao i peca-t3)))
				)
			)
		)
		(pop lista-possiveis)
		lista-possiveis
	)
)

(defun listas-iguais (l1 l2)
	(let ((iguais T))
		
		(when (not (= (length l1) (length l2))) (setf iguais nil))
		
		(when iguais
			(dotimes (i (length l1))
				(when (not (equal (nth i l1) (nth i l2))) 
					(setf iguais nil)
					(return)
				)
			)
		)
		
		iguais
	)
)

(defun push-fim (lista elemento)
	(let ((nova-lista lista))
		(if (not nova-lista)
			(setf nova-lista (list elemento))
			(push elemento (cdr (nthcdr (length (cdr nova-lista)) nova-lista)))
		)
		nova-lista
	)
)

;  #################################################################################################
; #-------------------------------------------------------------------------------------------------#
; #-------------------------------------------------------------------------------------------------#
; #------------------------SEGUNDA PARTE DO PROJETO - PROCURAS E HEURISTICAS------------------------#
; #-------------------------------------------------------------------------------------------------#
; #-------------------------------------------------------------------------------------------------#
;  #################################################################################################

; Estrutura no
(defstruct node
	estado
	fn
	gn
	pai
	accaoGeradora
)

; Procura DF
(defun procura-pp (problema)
	(let (
			(haSolucao nil)

			(abertos (list))
			(fechados (list))

			(teste-solucao (problema-solucao problema))
			(custo-caminho (problema-custo-caminho problema))
			(estado-inicial (problema-estado-inicial problema))

			(no-atual nil)
		)

		; Inicializar com no correspondente ao estado inicial
		(setf no-atual (make-node :estado estado-inicial :fn (funcall custo-caminho estado-inicial) :gn (funcall custo-caminho estado-inicial) :pai nil :accaoGeradora nil))
		(push no-atual abertos)

		(loop while abertos
			
			; 1 - Escolha do no
			do(setf no-atual (pop abertos))

			; 2 - Teste solucao
			(when (funcall teste-solucao (node-estado no-atual))
				(setf haSolucao T)
				(return)
			)

			; 3 - Expandir
			(setf abertos (expandeParaListaDeep no-atual problema abertos))
			(push no-atual fechados)

		)

		(if haSolucao
			(getCaminho no-atual)
			nil
		)
	)
)

; Procura Astar
(defun procura-A* (problema heuristica)
	(let (
			(haSolucao nil)
			(abertos (list))
			(fechados (list))

			(teste-solucao (problema-solucao problema))
			(custo-caminho (problema-custo-caminho problema))
			(estado-inicial (problema-estado-inicial problema))

			(index-melhor-no 0)
			(no-atual nil)
		)

		; Inicializar com no correspondente ao estado inicial
		(setf no-atual (make-node :estado estado-inicial :fn (f-A* estado-inicial custo-caminho heuristica) :gn (funcall custo-caminho estado-inicial) :pai nil :accaoGeradora nil))
		(push no-atual abertos)
		
		(loop while abertos

			;1 - Escolha do no
			do(setf index-melhor-no (getBestIndex abertos))

			(setf no-atual (nth index-melhor-no abertos))

			(setf abertos (removeNth index-melhor-no abertos))

			(push no-atual fechados)

			;2 - Teste solucao
			(when (funcall teste-solucao (node-estado no-atual))
				(setf haSolucao T)
				(return)
			)

			;3 - Expandir
			(setf abertos (expandeParaListaAStar no-atual problema heuristica abertos))
			
		)

		(if haSolucao
			(getCaminho no-atual)
			nil
		)
		
	)
)

; Melhor procura do grupo
(defun procura-best (arrayTab listaPecas)
	(let(
		(problema nil)
		(estado-inicial nil)
		)
	
		; 1 - Inicializar o estado e o problema
		(setf estado-inicial (make-estado :tabuleiro (array->tabuleiro arrayTab) :pecas-por-colocar listaPecas))
		(setf problema 
			(make-problema 	:estado-inicial estado-inicial
							:solucao #'solucao
							:accoes #'accoes
							:resultado #'resultado
							:custo-caminho #'custo-oportunidade
			)
		)

		; 2 - Correr procura e devolver accoes
		(procura-A* problema #'heuristica-best) ; heuristica placeholder
	)
)

; Fn = Gn + Hn
(defun f-A* (estado g-n h-n)
		(+ (funcall g-n estado) (funcall h-n estado))
)

; Funcao auxiliar que expande os filhos para a lista de abertos
(defun expandeParaListaAStar (no problema heuristica lista)	
	(let (
			(gera-accoes (problema-accoes problema))
			(gera-filho (problema-resultado problema))
			(g-n (problema-custo-caminho problema))

			(accoes (list))
			(estado-filho-atual nil)
			(no-filho-atual nil)
		)

		(setf accoes (funcall gera-accoes (node-estado no)))
		(when accoes
			
			(dotimes (i (length accoes))

				(setf estado-filho-atual (funcall gera-filho (node-estado no) (nth i accoes)))
				(setf no-filho-atual (make-node :estado estado-filho-atual :fn (f-A* estado-filho-atual g-n heuristica) :gn (funcall g-n estado-filho-atual) :pai no :accaoGeradora (nth i accoes)))
				(push no-filho-atual lista)
			)
		)

		lista ; Return nova lista
	)
)

; Funcao auxiliar que expande os filhos para a lista de abertos
(defun expandeParaListaDeep (no problema lista)	
	(let (
			(gera-accoes (problema-accoes problema))
			(gera-filho (problema-resultado problema))
			(g-n (problema-custo-caminho problema))

			(accoes (list))
			(estado-filho-atual nil)
			(no-filho-atual nil)
		)

		(setf accoes (funcall gera-accoes (node-estado no)))
		(when accoes
			(dotimes (i (length accoes))
				(setf estado-filho-atual (funcall gera-filho (node-estado no) (nth i accoes)))
				(setf no-filho-atual (make-node :estado estado-filho-atual :fn (funcall g-n estado-filho-atual) :gn (funcall g-n estado-filho-atual) :pai no :accaoGeradora (nth i accoes)))
				(push no-filho-atual lista)
			)
		)
		lista ; Return nova lista
	)

)

; Funcao que dada uma lista de nos devolve o indice do no com o melhor fn
(defun getBestIndex (lista)
	(let ((best 999999) (no-atual nil) (bestIndex 0))
		(dotimes (i (length lista))
			(setf no-atual (nth i lista))
			
			(when (< (node-fn no-atual) best)
				(setf best (node-fn no-atual))
				(setf bestIndex i)
			)
		)
		bestIndex
	)
)

; Funcao que dado um no devolve a lista de accoes necessarias para ate ele chegar
(defun getCaminho (no)
	(let ((caminho (list)) (no-atual nil))
		(setf no-atual no)
		(loop while (not (equal (node-accaoGeradora no-atual) nil))
			do(push (node-accaoGeradora no-atual) caminho)
			(setf no-atual (node-pai no-atual))
		)
		caminho
	)
)

; Funcao que dada uma lista e um indice n devolve uma lista nova sem o elemento na posicao n
(defun removeNth (n lista)
	(let ((lista-nova (list)))
		(dotimes (i (length lista))
			
			(when (not (= n i))
				(push (nth i lista) lista-nova)
			)
		)
		(reverse lista-nova)
	)
)

; Funcao que dado um tabuleiro devolve a altura da coluna mais alta do tabuleiro
(defun tabuleiro-altura-max (tab)
	(let ((top 0))
		(dotimes (c 10)
			(when (> (tabuleiro-altura-coluna tab c) top)
				(setf top (tabuleiro-altura-coluna tab c))
			)
		)
		top
	)
)

;  #############
; ###############
; # Heuristicas #
; ###############
;  #############

; Heuristica nula para testar a procura A* como uma greedy
(defun heuristica-zero (estado) 
	(- (estado-pontos estado) (estado-pontos estado))
)

; Peso de cada uma das heuristicas
(defun heuristica-best (estado)
	(let (
		(c0 20)
		(c1 1)
		(c2 1)
		(c3 1)
		(c4 1)
		(c5 1)
		)

		(+ (* (h0 estado) c0) (+ (* (h1 estado) c1) (+ (* (h2 estado) c2) (+ (* (h3 estado) c3) (+ (* (h4 estado) c4) (* (h5 estado) c5))))))	
	;	(+ (* (h0 estado) c0) (+ (* (h1 estado) c1) (+ (* (h2 estado) c2) (* (h5 estado) c5))))
	)	
)

; heuristica 0 : num de buracos no tabuleiro (onde estao as pecas)
(defun h0 (estado)
	(let ((contadorBuracos 0) (tab (estado-Tabuleiro estado)))
		(dotimes (x 10)
			(dotimes (y (tabuleiro-altura-max tab))
				(when (not (tabuleiro-preenchido-p tab y x))
					(incf contadorBuracos)
				)
			)
		)
		contadorBuracos
	)
)

; heuristica 1 : altura da celula preenchida mais alta
(defun h1 (estado)
	(tabuleiro-altura-max (estado-Tabuleiro estado))
)

; heuristica 2 : numero de celulas preenchidas no tabuleiro
(defun h2 (estado)
	(let ((contador 0) (tab (estado-Tabuleiro estado)))
		(dotimes (x 10)
			(dotimes (y (tabuleiro-altura-max tab))
				(when (tabuleiro-preenchido-p tab y x)
					(incf contador)
				)
			)
		)
		contador
	)
)

; Altura da parede que mais alto esta no tabuleiro, p.ex
(defun h3 (estado)
	(let (
			(highestSlope 0)
			(slopePrev 0)
			(slopeNext 0)

			(altura-col-atual 0)
			(altura-col-prev 0)
			(altura-col-next 0)

			(tab (estado-Tabuleiro estado))

		)
		(dotimes (c 10)
			(cond 
				((= c 0) 
					(setf altura-col-atual (tabuleiro-altura-coluna tab c))
					(setf altura-col-next (tabuleiro-altura-coluna tab (+ c 1)))

					(setf slopeNext (- altura-col-next altura-col-atual))

					(when (> slopeNext highestSlope)
						(setf highestSlope slopeNext)
					)
				)
				
				((= c 9)
					(setf altura-col-atual (tabuleiro-altura-coluna tab c))
					(setf altura-col-prev (tabuleiro-altura-coluna tab (- c 1)))

					(setf slopePrev (- altura-col-prev altura-col-atual))

					(when (> slopePrev highestSlope)
						(setf highestSlope slopePrev)
					)
				)
				(T
					(setf altura-col-atual (tabuleiro-altura-coluna tab c))
					(setf altura-col-prev (tabuleiro-altura-coluna tab (- c 1)))
					(setf altura-col-next (tabuleiro-altura-coluna tab (+ c 1)))

					(setf slopePrev (- altura-col-prev altura-col-atual))
					(setf slopeNext (- altura-col-next altura-col-atual))

					(when (> (max slopeNext slopePrev) highestSlope)
						(setf highestSlope (max slopeNext slopePrev))
					)
				)
			)
		)
	highestSlope
	)
)

; Soma das alturas das paredes no tabuleiro
(defun h4 (estado)
	(let (
			(roughness 0)
			(slopePrev 0)
			(slopeNext 0)

			(altura-col-atual 0)
			(altura-col-prev 0)
			(altura-col-next 0)

			(tab (estado-Tabuleiro estado))

		)
		(dotimes (c 10)
			(cond 
				((= c 0) 
					(setf altura-col-atual (tabuleiro-altura-coluna tab c))
					(setf altura-col-next (tabuleiro-altura-coluna tab (+ c 1)))

					(setf slopeNext (- altura-col-next altura-col-atual))

					(setf roughness (+ roughness slopeNext))
				)
				
				((= c 9)
					(setf altura-col-atual (tabuleiro-altura-coluna tab c))
					(setf altura-col-prev (tabuleiro-altura-coluna tab (- c 1)))

					(setf slopePrev (- altura-col-prev altura-col-atual))

					(setf roughness (+ roughness slopePrev))
				)
				(T
					(setf altura-col-atual (tabuleiro-altura-coluna tab c))
					(setf altura-col-prev (tabuleiro-altura-coluna tab (- c 1)))
					(setf altura-col-next (tabuleiro-altura-coluna tab (+ c 1)))

					(setf slopePrev (- altura-col-prev altura-col-atual))
					(setf slopeNext (- altura-col-next altura-col-atual))

					(setf roughness (+ roughness (+ slopeNext slopePrev)))
				)
			)
		)
	roughness
	)
)

; heuristica 5 : numero de celulas preenchidas no tabuleiro
(defun h5 (estado)
	(let ((contador 0) (tab (estado-Tabuleiro estado)))
		(dotimes (x 10)
			(dotimes (y (tabuleiro-altura-max tab))
				(when (tabuleiro-preenchido-p tab y x)
					(setf contador (+ contador (+ y 1)))
				)
			)
		)
		contador
	)
)

(load "utils.fas")