(require (lib "tartaruga.scm" "user-feup"))
(require (lib "simula-desloca.scm" "user-feup"))
(require (lib "tabuleiro.scm" "user-feup"))
(require (lib "swgr.scm" "user-feup"))
(require (lib "audio.scm" "user-feup"))

(define mensagem_introdutoria
  (lambda (lado)
    (let* ((posicao_titulo (list (* lado 70) (* lado 48)))
           (posicao_frase1 (list (* lado 55) (* lado 45)))
           (posicao_frase2 (list (* lado 52) (* lado 43)))
           (posicao_frase3 (list (* lado 55) (* lado 41)))
           (posicao_frase4 (list (* lado 52) (* lado 39)))
           (posicao_frase5 (list (* lado 55) (* lado 37)))
           (posicao_frase6 (list (* lado 55) (* lado 35))))
      (move posicao_titulo) (cor 3)(desenha-txt "A tartaruga escondida")
      (move posicao_frase1) (cor 0)(desenha-txt "Bem vindo ao jogo da tartaruga escondida! O teu objetivo é encontrar uma tartaruga")
      (move posicao_frase2) (desenha-txt "que se decidiu esconder numa posição desconhecida deste tabuleiro.")
      (move posicao_frase3) (desenha-txt "Aquele circulo no tabuleiro representa a tua tartaruga. Sim, também és uma tartaruga!")
      (move posicao_frase4) (desenha-txt "Podes te mexer usando as setas do teclado. Mas só se quiseres...")
      (move posicao_frase5) (desenha-txt "Boa sorte!")
      (move posicao_frase6) (desenha-txt "Ah, quase me esquecia. Tem cuidado onde pisas ;)"))))

(define win
  (lambda (lado)
    (let* ((posicao (list (* lado 65) (* lado 10)))
           (posicao_distancia (list (* lado 55) (* lado 30)))
           (quadrado (list (list (* lado 55) (* lado 32)) (list (* lado 55) (* lado 22))(list (* lado 95) (* lado 22))(list (* lado 95) (* lado 32))(list (* lado 55) (* lado 32))))
           (color-o-meter (list (list (* lado 60) (* lado 29)) (list (* lado 60) (* lado 22))(list (* lado 90) (* lado 22))(list (* lado 90) (* lado 29)))))
      (cor 26) (pinta quadrado)  ;pinta um quadrado branco para dar clear
      (move posicao_distancia)(cor 2)(desenha-txt "Distancia da tartaruga: ")(cor 11)(desenha-txt "I knew you could do it!")(pinta color-o-meter) ;indicador distancia      
      (move posicao) (cor 3)      
      (desenha-txt "Parabéns! Encontraste a tartaruga!")
      (som "yipee")(som "yaahooo")(som "yeehaaa")
      (fecha-janela))))

(define lose
  (lambda (lado)
    (let* ((posicao (list (* lado 65) (* lado 10)))
           (posicao_danger (list (* lado 55) (* lado 20)))
           (quadrado (list (list (* lado 55) (* lado 22)) (list (* lado 55) (* lado 5))(list (* lado 95) (* lado 5))(list (* lado 95) (* lado 22))(list (* lado 55) (* lado 22))))
           (color-o-meter (list (list (* lado 60) (* lado 29)) (list (* lado 60) (* lado 22))(list (* lado 90) (* lado 22))(list (* lado 90) (* lado 29)))))
      (cor 26) (pinta quadrado)  ;pinta um quadrado branco para dar clear
      (move posicao_danger)(cor 2)(desenha-txt "Danger-o-meter: ")(cor 0)(desenha-txt "KABOOOOOOOOOOM!!!!!!")(pinta color-o-meter) ;indicador perigo      
      (move posicao)(cor 18)
      (desenha-txt "Eu avisei para ter cuidado... ")
      (som "bomba1")
      (fecha-janela))))

(define atualiza_UI
  (lambda (lado distancia perigo)
    (let*((posicao_distancia (list (* lado 55) (* lado 30)))
          (posicao_danger (list (* lado 55) (* lado 20)))
          (quadrado (list (list (* lado 55) (* lado 32)) (list (* lado 55) (* lado 5))(list (* lado 95) (* lado 5))(list (* lado 95) (* lado 32))(list (* lado 55) (* lado 32))))
          (color-o-meter (list (list (* lado 60) (* lado 29)) (list (* lado 60) (* lado 22))(list (* lado 90) (* lado 22))(list (* lado 90) (* lado 29))))
          (introducao (mensagem_introdutoria lado))) ;introducao
      (cor 26) (pinta quadrado) ;pinta um quadrado branco para dar clear
      (move posicao_distancia)(cor 2)(desenha-txt "Distancia da tartaruga: ")(cond ((> distancia 10) (cor 1) (desenha-txt "It's over 9000!")(pinta color-o-meter))
                                                                                    ((> distancia 8) (cor 5)(desenha-txt"Definitely not near this place!")(pinta color-o-meter))
                                                                                    ((> distancia 6) (cor 17)(desenha-txt"Well, it could be worse...")(pinta color-o-meter))
                                                                                    ((> distancia 4) (cor 25)(desenha-txt"You are getting closer!")(pinta color-o-meter))
                                                                                    ((> distancia 3) (cor 24)(desenha-txt"That's it! Keep going!")(pinta color-o-meter))
                                                                                    ((= distancia 3) (cor 21)(desenha-txt"Keep your eyes open!")(pinta color-o-meter))
                                                                                    ((= distancia 2) (cor 18)(desenha-txt"Hot hot hot")(pinta color-o-meter))
                                                                                    ((= distancia 1) (cor 9)(desenha-txt"Do you really need my help now?")(pinta color-o-meter)))
      (move posicao_danger)(cor 18) (desenha-txt "Danger-o-meter: ") (desenha-txt (number->string perigo))))) ;indicador perigo


(define tartaruga_escondida
  (lambda (size)
    (letrec ((lado (cond ((< size 8) 8) ((> size 20)20) (else size)))
             (ht (faz-tartaruga (add1 (random lado)) (add1 (random lado)) 51))
             (pt (faz-tartaruga (add1 (random lado)) (add1 (random lado)) 12))
             (mina1 (faz-tartaruga (add1 (random lado)) (add1 (random lado)) 51))
             (mina2 (faz-tartaruga (add1 (random lado)) (add1 (random lado)) 51))
             (mina3 (if (> lado 3) (faz-tartaruga (add1 (random lado)) (add1 (random lado)) 51) #f))
             (mina4 (if (> lado 4) (faz-tartaruga (add1 (random lado)) (add1 (random lado)) 51) #f))
             (mina5 (if (> lado 5) (faz-tartaruga (add1 (random lado)) (add1 (random lado)) 51) #f))
             (mina6 (if (> lado 6) (faz-tartaruga (add1 (random lado)) (add1 (random lado)) 51) #f))
             (mina7 (if (> lado 7) (faz-tartaruga (add1 (random lado)) (add1 (random lado)) 51) #f))
             (mina8 (if (> lado 8) (faz-tartaruga (add1 (random lado)) (add1 (random lado)) 51) #f))
             (mina9 (if (> lado 9) (faz-tartaruga (add1 (random lado)) (add1 (random lado)) 51) #f))
             (mina10(if (> lado 10) (faz-tartaruga (add1 (random lado)) (add1 (random lado)) 51) #f))
             (mina11(if (> lado 11) (faz-tartaruga (add1 (random lado)) (add1 (random lado)) 51) #f))
             (mina12(if (> lado 12) (faz-tartaruga (add1 (random lado)) (add1 (random lado)) 51) #f))
             (lista_minas (list mina1 mina2 mina3 mina4 mina5 mina6 mina7 mina8 mina9 mina10 mina11 mina12))
             (d_pt_mina (apply min (map (lambda (x) (if (not(equal? x #f))(distancia pt x) 99)) lista_minas)))
             (d_ht_mina (apply min (map (lambda (x) (if (not(equal? x #f))(distancia ht x) 99)) lista_minas)))
             (tamanho_celula (/ (* lado 50) lado))
             (espera #t)
             (window (janela (* lado 100) (* lado 50) "Tartaruga Escondida"))
             (tabu (tabuleiro 0 (+ (/ (* lado 50) 2) (* (/ lado 2) tamanho_celula)) tamanho_celula lado lado))
             (posicao_distancia (list (* lado 55) (* lado 30)))
             (display_distancia (begin (move posicao_distancia)(cor 2)(desenha-txt "Distancia da tartaruga: ???"))) ;indicador distancia
             (posicao_danger (list (* lado 55) (* lado 20))) 
             (display_danger (begin (move posicao_danger)(cor 18) (desenha-txt "Danger-o-meter: ???"))) ;indicador perigo
             (introducao (mensagem_introdutoria lado)) ;introducao
             (jogo
              (lambda (t1 t2 jogada)
                (if (= 0 (distancia t1 t2)) ;se encontrar a tartaruga
                    (begin (celula tabu (sub1 (coluna-tar t2)) (sub1 (linha-tar t2)) 'x 9)
                           (celula tabu (sub1 (coluna-tar t2)) (sub1 (linha-tar t2)) '+ 3)(celula tabu (sub1 (coluna-tar t2)) (sub1 (linha-tar t2)) 'c 3)
                           (win lado)) ;ganhar o jogo
                    (letrec ((desenho-tartaruga (begin (celula tabu (sub1 (coluna-tar t1)) (sub1 (linha-tar t1)) '+ 3)(celula tabu (sub1 (coluna-tar t1)) (sub1 (linha-tar t1)) 'c 3))) ;desenha tartaruga
                             (direcao (tecla-pressionada espera))
                             (new_col (cond ((equal? direcao 'left) (- (coluna-tar t1) 1))                     ; definir limite superior e lateral
                                            ((equal? direcao 'right) (+ (coluna-tar t1) 1))
                                            (else (coluna-tar t1))))
                             (new_line (cond ((equal? direcao 'up) (- (linha-tar t1) 1))                     ; definir limite superior e lateral
                                             ((equal? direcao 'down) (+ (linha-tar t1) 1))
                                             (else (linha-tar t1))))
                             (new_t1 (faz-tartaruga (cond ((< new_col 1) 1)((> new_col lado) lado)(else new_col))
                                                    (cond ((< new_line 1) 1)((> new_line lado) lado)(else new_line)) 51))
                             (dist (distancia new_t1 t2))
                             (danger ( - 100 (* 8 (apply min (map (lambda (x) (if (not(equal? x #f))(distancia new_t1 x) 99)) lista_minas))))))                         
                      (cond ((= dist 0) (celula tabu (sub1 (coluna-tar t2)) (sub1 (linha-tar t2)) 'x 9)  ;se encontrar a tartaruga
                                        (celula tabu (sub1 (coluna-tar t2)) (sub1 (linha-tar t2)) '+ 3)(celula tabu (sub1 (coluna-tar t2)) (sub1 (linha-tar t2)) 'c 3)
                                        (celula tabu (sub1 (coluna-tar t1)) (sub1 (linha-tar t1)) 'l 26)(celula tabu (sub1 (coluna-tar t1)) (sub1 (linha-tar t1)) 'p 2)
                                        (win lado)) ;ganhar o jogo
                            ((= danger 100) (celula tabu (sub1 (coluna-tar t1)) (sub1 (linha-tar t1)) 'l 26)(celula tabu (sub1 (coluna-tar t1)) (sub1 (linha-tar t1)) 'p 2) ;se apanhar uma mina
                                            (celula tabu (sub1 (coluna-tar new_t1)) (sub1 (linha-tar new_t1)) '+ 3)(celula tabu (sub1 (coluna-tar new_t1)) (sub1 (linha-tar new_t1)) 'c 3)                                            
                                            (som "oh_no")(celula tabu (sub1 (coluna-tar new_t1)) (linha-tar new_t1) 'x 9)(celula tabu (coluna-tar new_t1) (linha-tar new_t1) 'x 9) ;Desenha cruzes à volta da mina
                                            (celula tabu (coluna-tar new_t1) (sub1 (linha-tar new_t1)) 'x 9)(celula tabu (coluna-tar new_t1) (- (linha-tar new_t1) 2) 'x 9)
                                            (celula tabu (sub1 (coluna-tar new_t1)) (- (linha-tar new_t1) 2) 'x 9)(celula tabu (- (coluna-tar new_t1) 2) (- (linha-tar new_t1) 2) 'x 9)
                                            (celula tabu (- (coluna-tar new_t1) 2) (sub1(linha-tar new_t1)) 'x 9)(celula tabu (- (coluna-tar new_t1) 2) (linha-tar new_t1) 'x 9)                           
                                            (celula tabu (sub1 (coluna-tar new_t1)) (sub1 (linha-tar new_t1)) 'x 9)(lose lado))
                            
                            (else (begin
                                    (som "anda2")
                                    (celula tabu (sub1 (coluna-tar t1)) (sub1 (linha-tar t1)) 'l 26); limpa simbolo da tartaruga da posicao anterior
                                    (celula tabu (sub1 (coluna-tar t1)) (sub1 (linha-tar t1)) 'p 2) ; ponto indicativo onde estava antes
                                    (atualiza_UI lado dist danger)
                                    (jogo new_t1 t2 (add1 jogada))))))))))
      (if(or(zero? (distancia pt ht)) (<= d_pt_mina 1)(<= d_ht_mina 1)) ; Se a posicao das tartarugas coincidir; ou se alguma mina estiver em cima ou imediatamente a seguir de uma das tartarugas, restarts game
         (begin (fecha-janela)(tartaruga_escondida size))
         (jogo pt ht 1)))))


;TODO
; Limitar size do tabuleiro de acordo com UI
; Tamanho letra
; Spawn em cima da tartaruga