#lang racket/gui
;Ignorați următoarele linii de cod. Conțin import-uri și export-uri necesare checker-ului.

(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

(require "abilities.rkt")
(require "constants.rkt")
;---------------------------------------checker_exports------------------------------------------------
(provide next-state)
(provide next-state-bird)
(provide next-state-bird-onspace)
(provide change)

(provide get-pipes)
(provide get-pipe-x)
(provide next-state-pipes)
(provide add-more-pipes)
(provide clean-pipes)
(provide move-pipes)

(provide invalid-state?)
(provide check-ground-collision)
(provide check-pipe-collisions)

(provide draw-frame)

(provide get-initial-state)
(provide get-bird)
(provide get-bird-y)
(provide get-bird-v-y)

; pipe
(provide get-pipes)
(provide get-pipe-x)

; score25
(provide get-score)

(provide get-abilities)
(provide get-abilities-visible)
(provide get-abilities-active)
; variables
(provide get-variables)
(provide get-variables-gravity)
(provide get-variables-momentum)
(provide get-variables-scroll-speed)

; functions
(provide list-size)

;---------------------------------------checker_exports------------------------------------------------
; Checker-ul contine un numar de teste, fiecare cu numele sau. In acest fisier veti gasi comentarii
; care incep cu TODO %nume_test, unde trebuie sa modificati sau sa implementati o functie, pentru
; a trece testul %nume_test.
;
; Primul pas pe care trebuie să îl facem este să creăm starea inițială a jocului.
; Aceasta va fi salvată în (get-initial-state), și trebuie să incapsuleze toate informațiile
; necesare jocului, și anume: informații despre pasăre, despre pipes și, pentru bonus,
; despre powerups și despre variabilele de mediu.
; Recomandăm ca în pasăre, să rețineți, printre altele, informații despre y-ul curent
; și viteză pe y.
; Pe parcursul temei, în state, salvați coordonatele colțurilor din stânga sus ale obiectelor.
; Aceasta va face mai ușoară atât logică mișcării obiectelor, cât și testarea cerințelor.
; Toate coordonatele oferite în comentarii sau în fișierul variables.rkt se referă la
; colțul din stânga sus ale obiectelor!










;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;             TODO 0                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct bird (y speed color))
(define-struct pipe (x y color))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;             TODO 8                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (stream-pipes)
  (stream-cons (pipe scene-width (+ added-number (random random-threshold)) pipe-color) (stream-pipes)))
(define (create-pipe)
  (match-let* ([new_pipe (stream-first (stream-pipes))])
    (write 'pipe)
    (write '=>)
    (write (pipe-y new_pipe))
    (write ':)
    new_pipe))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;             TODO 1                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define initial-state (list (bird bird-initial-y initial-momentum bird-color) (list (create-pipe)) 0))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;             TODO 16                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-initial-state)
  initial-state)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                TODO 2             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-bird state) (car state))
(define (get-bird-y bird) (bird-x bird))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                TODO 3             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (next-state-bird bird-obj gravity)
  (match-let* ([(bird y speed color) bird-obj])
    (bird (+ y speed) (+ speed gravity) bird-color)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                TODO 4             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-bird-v-y bird-obj)
  (bird-speed bird-obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                TODO 6             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (next-state-bird-onspace bird-obj momentum)
  (match-let* ([(bird y speed color) bird-obj])
    (bird y (* momentum -1) "yellow")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                TODO 7             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (change current-state pressed-key)
  (match-let* ([old_b (get-bird current-state)]
               [new_b (next-state-bird-onspace old_b initial-momentum)]
               [old_p (get-pipes current-state)]
               [old_score (get-score current-state)])
    (cond [(key=? pressed-key " ") (list new_b old_p old_score)]
          [else (list old_b old_p old_score)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                TODO 9             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-pipes state)
 (car (cdr state)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                TODO 10            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define(get-pipe-x pipe-obj)
  (pipe-x pipe-obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                TODO 11            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (move-pipes pipes scroll-speed)
  (if (null? pipes) pipes
      (match-let* ([(pipe x y color) (car pipes)]
                   [translated_pipe (pipe (- x scroll-speed) y color)])
      (cons translated_pipe (move-pipes (cdr pipes) scroll-speed)
  ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                TODO 12            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (is_in_scene pipe-obj)
  (if (> 0 (+ 104(pipe-x pipe-obj))) #f #t))
(define (clean-pipes pipes)
  (filter is_in_scene pipes))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                TODO 13            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (pipe-x (car (add-more-pipes (list (pipe 100 1234  "yellow") (pipe 45 1234  "yellow") (pipe 200 1234  "yellow") (pipe 12 1234  "yellow") (pipe 23 123 "ssad")))))
(define (get-furthest-pipe pipes result)
  (if (null? pipes) (car result)
      (if (or (null? result) (< (get-pipe-x (car result)) (get-pipe-x (car pipes)))) (get-furthest-pipe (cdr pipes) (list (car pipes)))
          (get-furthest-pipe (cdr pipes) result))))
(define (add-more-pipes pipes)
  (if (< (list-size pipes) no-pipes)
      (match-let* ([last_pipe (get-furthest-pipe pipes '())]
                   [(pipe x y color) last_pipe]
                   [(pipe new_x new_y new_color) (create-pipe)]
                   [new_pipe (pipe (+ pipe-width pipe-gap x) new_y new_color)])
        (cons new_pipe pipes))
      pipes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                TODO 14            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(pipe-x (car  (next-state-pipes (list (pipe 10 2000 "dsad") (pipe 20 2000 "dsad") (pipe 30 2000 "dsad") (pipe 40 2000 "dsad")) -124)))
(define (next-state-pipes pipes scroll-speed)
  (match-let*([moved_pipes (move-pipes pipes scroll-speed)]
              [cleaned_pipes (clean-pipes moved_pipes)]
              [added_pipes (add-more-pipes cleaned_pipes)])
    added_pipes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                TODO 17            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-score state)
  (car (cdr ( cdr state))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                TODO 19            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; prag 903
(define (check-ground-collision bird-obj)
 (match-let*([(bird y speed color) bird-obj]
             [lower_point (+ y bird-height)])
   (if (>= lower_point ground-y) #t #f)))


(define (check-bird-pipe-collision bird-obj pipe-obj)
  (match-let* ([(bird bUp speed bcolor) bird-obj]
               [(pipe gapLeft gapUp gapColor) pipe-obj]
               
               [bDown (+ bUp bird-height)]
               [bLeft bird-x]
               [bRight (+ bLeft bird-width)]
               [gapRight (+ gapLeft pipe-width)]
               [gapDown (+ gapUp pipe-self-gap)])  ;; TODO 
    (if (and (< bLeft gapRight) (> bRight gapLeft))
        (if (or (< bUp gapUp) (> bDown gapDown))

            
            (match-let* ()
                  (write bUp)
                  (write '-)
                  (write gapLeft)
                  (write '-)
                  (write gapUp)
                  (write '-)
                  #t
            ) 

            #f) #f)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                TODO 20            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                TODO 22            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (invalid-state? state)
  (match-let*([bird-obj (get-bird state)]
              [pipes (get-pipes state)])
    (or (check-ground-collision bird-obj) (check-pipe-collisions bird-obj pipes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                TODO 21            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (check-pipe-collisions bird-obj pipes)
  (check-pipe-collisions-helper bird-obj pipes #f))
(define (check-pipe-collisions-helper bird-obj pipes acc)
  (if (or (null? pipes) acc) acc
      (check-pipe-collisions-helper bird-obj (cdr pipes) (check-bird-pipe-collision bird-obj (car pipes)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                TODO 5             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (next-state-helper state)
        (match-let* ([old_b (get-bird state)]
                     [(bird y speed  color) old_b]
                     [new_b (next-state-bird old_b initial-gravity)]
                     
                     [old_pipes (get-pipes state)]
                     [new_pipes (next-state-pipes old_pipes initial-scroll-speed)]

                     [old_score (get-score state)]
                     [new_score (+ 0.1 old_score)])
                     (list new_b new_pipes new_score)))
(define (next-state state) (next-state-helper state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                TODO 15            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; added next-state-pipes in next-state

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                TODO 18            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; incrementeaza scorul cu 0.1 la fiecare cadru.


; draw-frame
; draw-frame va fi apelat de big-bang dupa fiecare apel la next-state, pentru a afisa cadrul curent.
;TODO 23
; Fiecare cadru va fi desenat in urmatorul mod:
; bird peste ground, peste scor, peste pipes, peste empty-scene.
;
; Hint: score-to-image primeste un numar real si intoarce scor-ul sub forma de imagine;
; Scor-ul îl puteți plasa direct la coordonatele date, fără a mai face translatiile menționate mai jos.
; Noi tinem minte coltul din stanga sus al imaginii, insa, la suprapunerea unei imagini A peste o alta imagine,
; coordonatele unde plasam imaginea A reprezinta centrul acesteia. Trebuie facute translatiile de la coltul din stanga
; sus la centrul imaginilor.
; Variabile folosite in aceasta functie:
; bird -> bird-width si bird-height
; ground -> ground-y si ground-height, acesta va acoperi intreaga latime a ecranului
; scor -> text-x si text-y
; pipes -> pipe-width si pipe-height


(define (get_drawn_pipes_helper pipes)
  (if (null? pipes) initial-scene
       (match-let* ([next_pipe (car pipes)]
                    [(pipe x y color) next_pipe]
                    [remaining_pipes (cdr pipes)])
         (place-image
           (rectangle pipe-width pipe-self-gap "solid" "white") (+ (quotient pipe-width 2) x) (+ (quotient pipe-self-gap 2) y)
           (place-image
               (rectangle pipe-width scene-height "solid" "green") (+ (quotient pipe-width 2) x) (+ (quotient scene-height 2) 0) 
               (get_drawn_pipes_helper remaining_pipes))))))


(define (get_drown_score_pipes score pipes)
  (match-let* ([score_pic (score-to-image score)])
    (place-image
         score_pic  text-x text-y
         (get_drawn_pipes_helper pipes))))
      

(define (get_drown_ground_score_pipes score pipes)
  (place-image
     ground-image (+ (quotient scene-width 2) 0) (+ (quotient ground-height 2) ground-y)
     (get_drown_score_pipes score pipes)))


(define (get_drown_bird_ground_score_pipes bird-obj score pipes)
  (match-let* ([(bird y speed color) bird-obj])
    (place-image
     bird-image (+ (quotient bird-width 2) bird-x) (+ (quotient bird-height 2) y)
     (get_drown_ground_score_pipes score pipes))))

(define (draw-frame state)
  (match-let* ([bird-obj (get-bird state)]
               [pipes (get-pipes state)]
               [score (get-score state)])
    
    (get_drown_bird_ground_score_pipes bird-obj score pipes)))




(define bird-image (rectangle bird-width bird-height  "solid" "yellow"))
(define ground-image (rectangle scene-width ground-height "solid" "brown"))
(define initial-scene (empty-scene scene-width scene-height))

(define text-family (list "Gill Sans" 'swiss 'normal 'bold #f))
(define (score-to-image x)
	(apply text/font (~v (round x)) 24 "indigo" text-family))


; Folosind `place-image/place-images` va poziționa pipe-urile pe scenă.
(define (place-pipes pipes scene)
	'your-code-here)

; Bonus
; Completați abilities.rkt mai întâi, aceste funcții căt, apoi legați
; această funcționalitate la jocul inițial.


; Abilitatea care va accelera timpul va dura 10 de secunde, va avea imaginea (hourglass "tomato")
; va avea inițial poziția null si va modifica scrolls-speed dupa formulă
; scroll-speed = scroll-speed + 1
(define fast-ability 'your-code-here)

; Abilitatea care va încetini timpul va dura 30 de secunde, va avea imaginea (hourglass "mediumseagreen")
; va avea inițial poziția null si va modifica scrolls-speed dupa formulă
; scroll-speed = max(5, scroll-speed - 1)
(define slow-ability 'your-code-here)

; lista cu toate abilităţile posibile în joc
(define ABILITIES (list slow-ability fast-ability))


(define get-variables 'your-code-here)
(define get-variables-gravity 'your-code-here)
(define get-variables-momentum 'your-code-here)
(define get-variables-scroll-speed 'your-code-here)

; Întoarce abilităţile din stare, cu o reprezentare
; intermediară care trebuie să conțină două liste:
;  - lista abilităţilor vizibile (încarcate în scenă dar nu neaparat vizibile pe ecran).
;  - lista abilităţilor activate (cu care pasărea a avut o coloziune).
(define (get-abilities x) null)

; Întoarce abilităţile vizibile din reprezentarea intermediară.
(define (get-abilities-visible x) null)

; Întoarce abilităţile active din reprezentarea intermediară.
(define (get-abilities-active x) null)

; Șterge din reprezentarea abilităţilor vizibile pe cele care nu mai sunt vizibile.
; echivalent cu clean-pipes.
(define (clean-abilities abilities)
	'your-code-here)


; Muta abilităţile vizibile spre stanga.
; echivalent cu move-pipes.
(define (move-abilities abilities scroll-speed)
	'your-code-here)


; Scurge timpul pentru abilităţile activate și le sterge pe cele care au expirat.
; Puteți să va folosiți de variabila globală fps.
(define (time-counter abilities)
	'your-code-here)

; Generează următoarele abilitați vizibile.
; *Atentie* La orice moment pe scena trebuie să fie exact DISPLAYED_ABILITIES
; abilităţi vizibile
; Folosiți funcția fill-abilities din abilities.rkt cât si cele scrise mai sus:
; move-abilities, clean-abilities, time-counter, etc..
(define (next-abilities-visible visible scroll-speed)
	'your-code-here)

; Generează structura intermediară cu abilități.
; Observați ca nu există next-abilities-active aceastea sunt acele abilităti
; întoarse next-abilities-visible care au o coliziune cu pasărea.
; Puteti folosi `filer`/`filter-not` ca sa verificați ce abilităti au și abilitați
; nu au coliziuni cu pasărea sau puteti folosi `partition`
(define (next-abilities abilities bird scroll-speed)
	'your-code-here)

; Dând-use variabilele actuale și abilitațile calculați care vor
; variabile finale folosite în joc
; Folositi compose-abilities
; Atenție când apelați `next-variables` în next-state dați ca paremetru
; initial-variables și nu variabilele aflate deja în stare
; In felul acesta atunci când
(define (next-variables variables abilities)
  'your-code-here)


; Folosind `place-image/place-images` va poziționa abilităţile vizibile la ability pos.
(define (place-visible-abilities abilities scene)
	'your-code-here)

; Folosind `place-image/place-images` va poziționa abilităţile active
; în partea de sus a ecranului lângă scor.
; Imaginiile vor scalate cu un factor de 0.75 și așezate plecând
; de la ability-posn (constantă globală) cu spații de 50 de px.
; Imaginea cu indexul i va fi așezată la (ability-posn.x - 50*i, ability-posn.y)
(define (place-active-abilities abilities scene)
	'your-code-here)


(module+ main
	(big-bang (get-initial-state)
	 [on-tick next-state (/ 1.0 fps)]
	 [to-draw draw-frame]
	 [on-key change]
	 [stop-when invalid-state?]
	 [close-on-stop #t]
	 [record? #f]))