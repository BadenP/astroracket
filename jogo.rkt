; Trabalho 01 - PPLF
; Aluna: Paula Fernandes Torres RA: 123565

#lang racket
; Importação de bibliotecas
(require racket/gui)
(require racket/base)
(require rackunit)
(require rackunit/text-ui)

; Importação da lista contendo a sequência de instruções do jogo
(require "lista-textos.rkt")

; Imagens do personagem Astro 
(define astronauta-correndo-esquerda (list (make-object bitmap% "imagens/astronauta-ce1.png")
                                      (make-object bitmap% "imagens/astronauta-ce2.png")
                                      (make-object bitmap% "imagens/astronauta-ce3.png")
                                      (make-object bitmap% "imagens/astronauta-ce4.png")))

(define astronauta-correndo-direita (list (make-object bitmap% "imagens/astronauta-cr1.png")
                                     (make-object bitmap% "imagens/astronauta-cr2.png")
                                     (make-object bitmap% "imagens/astronauta-cr3.png")
                                     (make-object bitmap% "imagens/astronauta-cr4.png")))

(define astronauta-parado-direita (list (make-object bitmap% "imagens/astronauta-pe1.png")
                                       (make-object bitmap% "imagens/astronauta-pe2.png")
                                       (make-object bitmap% "imagens/astronauta-pe3.png")
                                       (make-object bitmap% "imagens/astronauta-pe3.png")))

(define astronauta-parado-esquerda (list (make-object bitmap% "imagens/astronauta-pr1.png")
                                      (make-object bitmap% "imagens/astronauta-pr2.png")
                                      (make-object bitmap% "imagens/astronauta-pr3.png")
                                      (make-object bitmap% "imagens/astronauta-pr3.png")))

(define racket-icone (make-object bitmap% "imagens/icon.png")) ; Imagem do ícone do Racket
(define background (make-object bitmap% "imagens/background.jpg")) ; Imagem do background
(define yoshi (make-object bitmap% "imagens/yoshi.png")) ; Imagem do Yoshi
(define nave-1 (make-object bitmap% "imagens/nave-1.png")) ; Imagem da nave 1
(define nave-2 (make-object bitmap% "imagens/nave-2.png")) ; Imagem da nave 2
(define nave-3 (make-object bitmap% "imagens/nave-3.png")) ; Imagem da nave 3

; Definições das variáveis

(define astro-largura 50)
(define astro-altura 60)
(define inicio-jogo #t)  ; Flag de início de jogo
(define fim-jogo #f)     ; Flag de fim de jogo
(define texto-canvas "") ; Texto exibido no canvas
(define 2-tentativa? #f) ; Flag de segunda tentativa para acertar pergunta
(define pontos 0)        ; Número de pontos 
(define indice-instrucao-atual 0) ; índice da instrução atual
(define instrucao-atual (first (first lista-textos))) ; instrução atual pra ser exibida na tela
(define resposta-certa #f) ; Flag de resposta certa
(define resposta-errada #f) ; Flag de resposta errada
(define astro-x 10) ; Declaração da posição inicial do Astro
(define astro-y 424) ; Declaração da posição inicial do Astro
(define yoshi-x 880) ; Declaração da posição do Yoshi
(define yoshi-y 425) ; Declaração da posição do Yoshi
(define texto-dinamico "" ) ; Texto digitado caracter por caracter
(define indice-texto 0) ; Índice do texto digitado
(define digita-timer (new timer% [notify-callback (lambda () (texto-digitado instrucao-atual))])) ; Timer para a escrita dinâmica de texto, ao ser iniciado o timer ele dispara a chamada da função texto-digitado
(define sentido 'right) ; Inicialização da direção do Astro e dos frames
(define frame-astro 0)  ; Inicialização do frame do Astro
(define movendo? #f) ; Flag para identificar a movimentação do astro e atualizar os frames
(define proximo-yoshi #f) ; Flag para indicar se está próximo ao Yoshi
(define frame-astro-timer (new timer% [notify-callback (lambda () (atualiza-frame-astro))])) ; Declaração do timer para atualizar o frame do Astro, ao ser iniciado ele dispara a chamada da função atualiza-frame-astro
(send frame-astro-timer start 300) ; Inicialização do timer de frames
 
; Definições das funções

; Void -> Boolean
; Checa se o Astro está próximo ao Yoshi: retorna #t se o Astro estiver próximo ao Yoshi e #f caso contrário.
(define (proximo-ao-yoshi?)
    (< (abs (- yoshi-x astro-x)) 75))

; Void -> Void
; Função que soma pontos ao total de pontos e atualiza a exibição na tela
(define (add-pontos valor)
  (set! pontos (+ pontos valor))
  (send canvas refresh))

; Void -> Void
; Função que atualiza os frames para que ocorra a animação na tela quando o Astro está parado
(define (atualiza-frame-astro)
  (set! frame-astro (modulo (+ frame-astro 1) 4))
  (send canvas refresh))

; String -> Void
; Função que digita caracter por caracter um texto passado como parâmetro em um text-field
(define (texto-digitado texto)
  (cond[(< indice-texto (string-length texto))
        (set! texto-dinamico (substring texto 0 (+ indice-texto 1)))
        (set! indice-texto (+ indice-texto 1))
        (send field set-value texto-dinamico) 
        (send field refresh)]
       [else(send digita-timer stop)]))

; String -> Void
; Função que analisa a resposta do usuário a partir do resultado esperado da instrução atual. Cada resultado esperado de uma instrução é analisado para chamar a função correspondente para corrigir a resposta do usuário.
; Caso o resultado esperado seja "", não é necessário analisar resposta e então a próxima instrução é chamada. Caso contrário, a partir do tamanho da lista que está a instrução atual é possível verificar se é preciso
; chamar uma função para analisar somente respostas literais ou se é necessário a função para testar funções do usuário.
; Caso o índice atual corresponda ao índice da última instrução da lista, não é preciso analisar resposta e é chamada a função para realizar o fim do jogo. 
(define (analise-resposta resposta-field-usuario)
  (cond[(= indice-instrucao-atual (- (length lista-textos) 1)) (inicia-fim-jogo)]
       [(< indice-instrucao-atual (length lista-textos))
        (let* ([elemento-lista (list-ref lista-textos indice-instrucao-atual)]
               [resultado-esperado (list-ref elemento-lista 1)])

          (if (equal? resultado-esperado "")
              (proxima-instrucao)
              (begin
                (let* ([resposta (string-trim resposta-field-usuario)])
                  (if (= (length elemento-lista) 3)
                      (testa-resultado-auxiliar resposta resultado-esperado)
                      (testa-funcao-auxiliar resultado-esperado resposta (list-ref elemento-lista 3)))))))]))
  
; Void -> Void
; Função para iniciar o estágio de fim do jogo
(define (inicia-fim-jogo)
  (altera-flags #f #f)
  (set! fim-jogo #t))

; Void -> void
; Função que atualiza os dados necessários para passar para a próxima instrução da lista de textos
(define (proxima-instrucao)
  (set! indice-instrucao-atual (add1 indice-instrucao-atual))
  (set! 2-tentativa? #f)
  (cond [(< indice-instrucao-atual (length lista-textos))
         (set! instrucao-atual (first (list-ref lista-textos indice-instrucao-atual)))
         (reset-text-field)]))

; String, String -> List
; Função que testa se uma resposta enviada pelo usuário está correta ou não. Recebe como parâmetros a entrada do usuário e o resultado esperado da pergunta atual.
; Retorna uma lista contendo como primeiro elemento um booleano (indicando se está correta ou não). Caso esteja incorreta, a lista poderá ter um segundo elemento com uma
; mensagem de erro caso seja capturada alguma exception.
(define (testa-resultado entrada resultado-esperado)
    (with-handlers
        [(exn:fail? (lambda (e)
                      (list #f e)))]
      (let* ([resultado (read (open-input-string entrada))]
             [resultado-esperado-avaliado (read (open-input-string resultado-esperado))])
        (if (equal? resultado resultado-esperado-avaliado)
            (list #t)
            (list #f "")))))

; String, String -> Void
; Função auxiliar para chamar a função testa-resultado, a partir do primeiro elemento retornado por testa-resultado as funções de resposta correta ou incorreta são chamadas.
(define (testa-resultado-auxiliar entrada resultado-esperado)
    (if (first (testa-resultado entrada resultado-esperado))
        (resposta-correta 5)
        (resposta-incorreta (first (rest (testa-resultado entrada resultado-esperado))))))

; Boolean, Boolean -> Void
; Altera as flags resposta-certa e resposta-errada para os valores passados como parâmetro.
(define (altera-flags resp-certa resp-errada)
  (set! resposta-certa resp-certa)
  (set! resposta-errada resp-errada))

; Número -> Void
; Recebe o número de pontos que o usuário deve ganhar e executa os comandos necessários quando uma resposta está correta.
(define (resposta-correta n)
  (altera-flags #t #f)
  (add-pontos n)
  (proxima-instrucao))

; String -> Void
; Recebe uma mensagem de erro e executa as funções necessárias quando uma resposta está incorreta. A flag 2-tentativa? indica se o usuário está na sua segunda tentativa
; para acertar a pergunta ou não. Caso 2-tentativa? seja #t, o usuário não tem mais chances para responder e é chamada a próxima instrução. Caso contrário, é chamada uma
; função para exibir a mensagem ao usuário junto com a dica da pergunta, e a flag 2-tentativa? é definida como #t.
(define (resposta-incorreta e)
  (let ([dica (list-ref (list-ref lista-textos indice-instrucao-atual) 2)])
    (altera-flags #f #t)
    (cond[2-tentativa? 
          (proxima-instrucao)]
         [else
          (exibe-msg-erro e dica)
          (set! 2-tentativa? #t)])))

; String, String -> Void
; Recebe uma mensagem de erro e uma dica: a mensagem de erro pode ser uma mensagem capturada por uma exception, uma string vazia ou uma string indicando que algum teste
; não passou para uma função enviada pelo usuário. Esta função exibe ao usuário a mensagem de erro e a dica quando ele erra uma pergunta pela primeira vez.
(define (exibe-msg-erro msg-erro dica)
  (cond[(equal? "" msg-erro) (send field set-value (string-append (send field get-value) "\n\n" dica))]
       [(string? msg-erro) (send field set-value (string-append (send field get-value) "\n\n" msg-erro "\n\n" dica))]
       [else(send field set-value (string-append (send field get-value) "\n\nMensagem de erro: " (exn-message msg-erro) "\n\n" dica))])
  (send field refresh))

; Void -> Void
; Restaura os valores necessários para atualizar a escrita dinâmica do text-field e deixa vazio o text-field do usuário para responder próximas perguntas.
(define (reset-text-field)
  (set! texto-dinamico "")
  (set! indice-texto 0)
  (send digita-timer start 20)
  (send field-usuario set-value "")
  (send field-usuario refresh))

; Símbolo, String, Lista -> Void
; É uma função auxiliar para mostrar ao usuário se a avaliação de uma função enviada por ele está correta ou incorreta.
(define (testa-funcao-auxiliar nome entrada casos-de-teste)
  (let ([retorno-funcao  (testa-funcao nome entrada casos-de-teste)])
    (if (first retorno-funcao)
        (resposta-correta 10)
        (resposta-incorreta (first (rest retorno-funcao))))))

; Símbolo, string, list -> List
; Esta função testa dinamicamente uma função parada como pârametro. Para testá-la é necessário usar a função (eval) em um novo namespace, um contexto em que variáveis e funções são
; definidas e identificadas. O uso desse novo namespace foi feito para evitar conflitos com a execução do programa atual. Como há um novo namespace, é necessário importar
; (require racket) para que seja possível reconhecer palavras reservadas da linguagem. Com (eval (read (open-input-string entrada)) namespace) é feito uma primeira avaliação
; da função contida na variável entrada, para que a função seja definida no namespace e posteriormente seja possível resgatar essa função pelo seu nome para realizar os testes. 
; A função retorna uma lista contendo como primeiro elemento um booleano que indica a função testada passou nos testes ou não. Caso seja capturado alguma exception durante a
; avaliação da função ou ela não passe nos testes, o segundo elemento da lista retornada é uma mensagem do erro.

; Para conseguir identificar se a função passou nos testes, é preciso analisar o resultado obtido do for/list dos casos de teste. O for/list retorna uma lista com os retornos obtidos
; para cada teste. Foi definido que se um teste não passou, o que é retornado é uma string com os dados do teste. Se o teste passou, retorna uma string vazia "". A partir disso,
; a função algum-teste-falhou? agrupa todos os elementos dessa lista de resultados e consegue verificar se algum teste falhou. O retorno da função para um teste que falhou
; é uma lista com primeiro elemento sendo #f e o resto sendo as mensagens de erro dos testes. O retorno da função caso os testes passem é uma lista contendo #t.
(define (testa-funcao nome entrada casos-de-teste)
  (if (string-contains? entrada "(exit)")
      (list #f "Mensagem de erro: (exit) não permitido\n")
      (let ([namespace (make-base-namespace)])
        (parameterize ((current-namespace namespace))
          (eval '(require racket) namespace)
          (with-handlers
              [(exn:fail? (lambda (e)
                            (list #f e)))] 
            (eval (read (open-input-string entrada)) namespace)

            (let ([lista-testes (for/list ([caso casos-de-teste])
                                  (let* ([args (first caso)]
                                         [esperado (first (rest caso))]
                                         [funcao (eval nome namespace)]
                                         [resultado (apply funcao args)])
                                    (if (not (equal? esperado resultado)) (format "Testes falharam para ~a. Esperado: ~a. Obtido: ~a\n" args esperado resultado) "")))])
              (if (algum-teste-falhou? lista-testes) (list #f (string-join lista-testes " ")) (list #t))))))))

; Lista -> Boolean
; Função que auxilia a função de testar funções, pois verifica o resultado de um for/list para os casos de teste. Caso o resultado possua as mensagens de falha nos testes retorna
; #t, indicando que houve falha nos testes. Caso contrário, retorna #f. A função string-join permite agrupar todos os elementos da lista passada como parâmetro, e portanto, se
; o resultado não é "", é porque houve falha em no mínimo um teste.
(define (algum-teste-falhou? lst)
  (let ([string-resultante (string-join lst " ")])
    (if (equal? (string-trim string-resultante) "") #f #t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ; INTERFACE GRÁFICA

; Aqui estão declarados os componentes gráficos. Foi necessário definir um canvas da classe canvas, com seus métodos on-char (para captar eventos de teclas pressionadas) e on-paint (para inserir elementos na tela).

; Declaração do canvas
(define my-canvas%
  (class canvas%
    (super-new)

    ; Método quando uma tecla é acionada no teclado
    (define/override (on-char event)
      (let* ([tecla (send event get-key-code)]
             [dx 0]
             [dy 0])
      (cond
        [(eq? tecla 'left) (set! dx -15) (set! sentido 'left) (set! proximo-yoshi #f)]
        [(eq? tecla 'right) (set! dx 15) (set! sentido 'right) (set! proximo-yoshi #f)]
        [(eq? tecla '#\return )
         (cond[(not inicio-jogo) (analise-resposta (send field-usuario get-value))])])

      ; Atualiza os frames se o Astro estiver se movendo
      (cond[(not (and (eq? dx 0) (eq? dy 0)))
            (set! movendo? #t)
            (cond[(and (< (+ astro-x dx) (- yoshi-x astro-largura)) (<= 0 (+ astro-x dx))) ; Esta condição serve para o Astro não ultrapassar o limite do frame e não ultrapassar o Yoshi
                  (set! astro-x (+ astro-x dx))
                  (set! astro-y (+ astro-y dy))
                  (send this refresh)])
            (set! frame-astro (modulo (+ frame-astro 1) 4))]
          [else(set! movendo? #f)])

      ; Void -> Void
      ; Começa a exibir o texto no text-field caso o Astro chegue perto do Yoshi
      (cond[(proximo-ao-yoshi?)
            (set! proximo-yoshi #t)
            (send canvas refresh)
            (send digita-timer start 15)]
           [(and fim-jogo (< (abs (- astro-x 330)) 75))
            (send label set-label (format "\nTotal RacketPoints: ~a\n" pontos))        
            (send dialog show #t)])))
    
    ; Método para desenhar elementos no canvas
    (define/override (on-paint)

      (let ([dc (send this get-dc)])
        ; Desenho das figuras estáticas da tela
        (send dc draw-bitmap background 0 0)
        (send dc draw-bitmap yoshi yoshi-x yoshi-y)
        (send dc draw-bitmap racket-icone 5 1)
        (send dc set-text-foreground "white")
        (send dc set-font (make-object font% 12 'default 'normal 'normal))
        (send dc draw-text (format " ~a" pontos) 20 0)
        (send dc draw-text texto-canvas 320 80)
        
        ; Esta condicional serve para alterar as mensagens que são exibidas ao usuário conforme o estado das flags
        (cond[inicio-jogo
              (send dc set-text-foreground "white")
              (send dc draw-text "Utilize as setas para se mover até o Yoshi" 480 80)]
             [(and proximo-yoshi inicio-jogo)
              (set! inicio-jogo #f)
              (set! texto-canvas "Leia as instruções do Yoshi e ganhe pontos de conhecimento em Racket!")]
             [resposta-certa
              (send dc set-text-foreground "black")
              (send dc draw-text "É isso aí! Você ganhou novas RacketCoins!" (+ yoshi-x -120) (- yoshi-y 40))]
             [resposta-errada
              (send dc set-text-foreground "black")
              (send dc draw-text "Que pena, sua resposta está incorreta. Tente novamente!" (+ yoshi-x -160) (- yoshi-y 40))]
             [fim-jogo
              (send dc set-text-foreground "black")
              (cond[(< pontos 50) (send dc draw-text "É uma pena... Essa nave é tão pequena!" (+ yoshi-x -150) (- yoshi-y 30)) (send dc draw-bitmap nave-3 330 415)]
                   [(and (> pontos 50)(< pontos 85)) (send dc draw-text "Esta nave é suficiente, mas é preciso aprender mais!" (+ yoshi-x -150) (- yoshi-y 30)) (send dc draw-bitmap nave-2 305 360)]
                   [(>= pontos 85) (send dc draw-text "Parabéns, Astro! Você conseguiu a nave perfeita!!" (+ yoshi-x -150) (- yoshi-y 30)) (send dc draw-bitmap nave-1 280 300)])
              (set! texto-canvas "")
              (send dc set-text-foreground "white")
              (send dc draw-text "Vá para sua nave para encerrar sua missão!" 480 80)])

        ; Condição para atualizar as imagens do Astro quando ele está se movendo, causando a animação dele correndo
        (cond[movendo? 
              (let* ([astro-correndo (if (eq? sentido 'right) astronauta-correndo-esquerda astronauta-correndo-direita)]
                     [imagem-atual (list-ref astro-correndo frame-astro)])
                (send dc draw-bitmap imagem-atual astro-x astro-y))]
             [else
              (let* ([astro-parado (if (eq? sentido 'right) astronauta-parado-direita astronauta-parado-esquerda)]
                     [imagem-atual (list-ref astro-parado frame-astro)])
                (send dc draw-bitmap imagem-atual astro-x astro-y))])

        ; Condição para atualizar o texto exibido no canvas após o Astro se aproximar do Yoshi no início do jogo
        (cond[(and proximo-yoshi inicio-jogo)
          (set! inicio-jogo #f)
          (set! texto-canvas "Leia as instruções do Yoshi e ganhe pontos de conhecimento em Racket!")])))))

; Declaração do frame da interface
(define frame (new frame%
                 [label "AstroRacket"]
                 [width 1255]
                 [height 768]))

; Declaração do canvas da interface
(define canvas (new my-canvas% 
                    [parent frame]))

; Declaração de um panel para o field que mostra as falas do Yoshi
(define panel (new vertical-panel% 
                   [parent frame]
                   [min-height 130] 
                   [stretchable-height #f]))

; Declaração de um panel para o field do usuário
(define panel2 (new horizontal-panel% 
               [parent frame]
               [min-height 100] 
               [stretchable-height #f]))

; Declaração de um text-field para exibir o diálogo do Yoshi
(define field (new text-field%
               [label ""]
               [parent panel]
               [style '(multiple)]))

; Declaração do text-field para o usuário inserir respostas
(define field-usuario (new text-field%
               [label "Insira aqui suas respostas:"]
               [parent panel2]
               [style '(multiple)]))

; Declaração do botão para enviar respostas do usuário
(define btn (new button%
               [label "Enviar resposta"]
               [parent panel2]
               [callback (lambda (button event) (cond[(not inicio-jogo) (analise-resposta (send field-usuario get-value))]))]))

; Declaração de um dialog para o fim do jogo
(define dialog (new dialog%
                    [width 200]
                    [height 120]
                    [parent frame]
                    [label "Fim de jogo"]))

; Declaração do label para exibir no dialog
(define label (new message% 
                   [parent dialog] [label (format "\nTotal RacketPoints: ~a\n" pontos)]))

; Declaração do botão para encerrar o jogo
(define btn-fim (new button% 
                      [parent dialog]
                      [label "Encerrar"]
                      [callback (lambda (button event) (exit))]))

; Chamada para exibir a janela
(send frame show #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TESTES UNITÁRIOS

; Nesta parte estão implementados os testes unitários

; Número -> Boolean
; Função auxiliar para conseguir testar a função proximo-ao-yoshi?. A função original não recebe parâmetros pois ela analisa a posição do personagem Astro
; na tela, por isso essa função auxiliar serve para conseguir definir uma posição para o personagem e testar a função.
(define (proximo-ao-yoshi-auxiliar n)
 (set! astro-x n)
 (proximo-ao-yoshi?))

(define proximo-ao-yoshi-auxiliar-testes
  (test-suite "proximo-ao-yoshi-auxiliar-testes"
              (check-equal? (proximo-ao-yoshi-auxiliar 400) #f)
              (check-equal? (proximo-ao-yoshi-auxiliar 1000) #f)
              (check-equal? (proximo-ao-yoshi-auxiliar 850) #t)
              (check-equal? (proximo-ao-yoshi-auxiliar 10) #f)))

(define testa-resultado-testes
  (test-suite "testa-resultado-testes"
              (check-equal? (testa-resultado "(+ 40 3)" "43") (list #f ""))
              (check-equal? (testa-resultado "34" "34") (list #t))
              (check-equal? (testa-resultado "(define raio-terra 6.371)" "(define raio-terra 6.371)") (list #t))
              (check-equal? (testa-resultado "#f" "#f") (list #t))))

(define testa-funcao-testes
  (test-suite "testa-funcao-testes"
              (check-equal? (testa-funcao 'remove-duplicados 
                                          "(define (remove-duplicados lst) 
                                              (cond[(empty? lst) empty]
                                                   [(empty? (rest lst)) lst]
                                                   [(= (first lst) (first (rest lst))) (remove-duplicados (rest lst))]
                                                   [else(cons (first lst) (remove-duplicados (rest lst)))]))" 
                                          '((((1 1 2 2 2)) (1 2)) 
                                           (((1 2 2 3 3 3 4)) (1 2 3 4)) 
                                           (((5 5 5 5 2 2 10 1 1)) (5 2 10 1))))
                            (list #t))
              (check-equal? (testa-funcao 'maior-valor 
                                          "(define (maior-valor a b) (if (< a b) b a))"
                                          '(((1 2) 2)
                                           ((6 6) 6)
                                           ((123 121) 123)
                                           ((-34 -65) -34)
                                           ((4 -5) 4)))
                            (list #t)) 
              (check-equal? (testa-funcao 'maior-valor 
                                          "(define (maior-valor a b) (exit))"
                                          '(((1 2) 2)
                                           ((6 6) 6)
                                           ((123 121) 123)
                                           ((-34 -65) -34)
                                           ((4 -5) 4)))
                            (list #f "Mensagem de erro: (exit) não permitido\n"))
              (check-equal? (testa-funcao 'maior-valor 
                                          "(define (maior-valor a b) (* a b))" 
                                          '(((1 2) 2)
                                           ((6 6) 6)
                                           ((123 121) 123)
                                           ((-34 -65) -34)
                                           ((4 -5) 4))) 
                            '(#f " Testes falharam para (6 6). Esperado: 6. Obtido: 36\n Testes falharam para (123 121). Esperado: 123. Obtido: 14883\n Testes falharam para (-34 -65). Esperado: -34. Obtido: 2210\n Testes falharam para (4 -5). Esperado: 4. Obtido: -20\n"))))

(define algum-teste-falhou?-testes
  (test-suite "algum-teste-falhou?-testes"
              (check-equal? (algum-teste-falhou? (list "" "" "")) #f)
              (check-equal? (algum-teste-falhou? (list "" "Teste falhou para (1 2):" "")) #t)
              (check-equal? (algum-teste-falhou? (list "" "Teste falhou para (8 9):" "" "Teste falhou para (5 6):" "")) #t)
              (check-equal? (algum-teste-falhou? (list "" "" "" "Teste falhou para (1 1):")) #t)))

;; Teste ... -> Void
;; Executa um conjunto de testes.
(define (executa-testes . testes)
  (run-tests (test-suite "Todos os testes" testes))
  (void))

(executa-testes proximo-ao-yoshi-auxiliar-testes
                testa-funcao-testes
                testa-resultado-testes
                algum-teste-falhou?-testes)

