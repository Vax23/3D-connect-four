
;-----VELICINA KOCKE-----

(defun velicinaKocke()
   (setq 
         n (progn
                 (format t "~%Unesite velicinu kocke >")
                 (read)))
     (cond
      ((AND (>= n 4) (= (mod n 2) 0)) (format t "Uneta je odgovarajuca velicina kocke"))
     (t (format t "Proverite sta ste uneli kao dimenziju priznaje se samo vrednost koja je veca od 4 i pritom je paran broj") (velicinaKocke))
     ))

(velicinaKocke)

(print n)



;-----IGRA PRVI-----

(setq oznakaCovek 'nil)
(setq oznakaRacunar 'nil)

(defun igraPrvi ()
  (let* (
         (prvi (progn
                 (format t "~%Unesite ko igra prvi (Racunar-0 Covek-1) >")
                 (read))))
    (cond 
      ( (equal prvi '1) (setq oznakaCovek 'x) (format t "~%Prvi igra Covek sa operandom ~a~%" oznakaCovek) (setq oznakaRacunar 'o) (format t "~%Drugi igra Racunar sa operandom ~a~%" oznakaRacunar))
    ( (equal prvi '0) (setq oznakaRacunar 'x) (format t "~%Prvi igra Racunar sa operandom ~a~%" oznakaRacunar) (setq oznakaCovek 'o) (format t "~%Drugi igra Covek sa operandom ~a~%" oznakaCovek)))))

(igraPrvi)

(print oznakaRacunar)
(print oznakaCovek)



;-----POSTAVLJANJE POCETNOG STANJA-----

(defun postaviPocetnoStanje(pomStanje pomPodLista clanoviUPodListi podLista)
  (cond
   ((= podLista (expt n 2)) (progn (setq stanje pomStanje)))
   ((= clanoviUPodListi n) (postaviPocetnoStanje (cons pomPodLista pomStanje) '() 0 (1+ podLista)))
   (t (postaviPocetnoStanje pomStanje (append '(-) pomPodLista) (1+ clanoviUPodListi) podLista))))

(defun odstampajJedno(stanje stapic naStapicu)
  (cond ((AND (not (< naStapicu 0)) (> n naStapicu)) (format t "~a " (nth naStapicu (nth stapic stanje))))
        (t (format t "  "))))

(defun odstampajMatricu(stanje indStapica indKuglice red dodaoN)
  (cond
   ((AND (> red (1- (* n 2))) (> indStapica (1- (expt n 2)))) (format t ""))
   ((AND (not(= 0 indStapica)) (= 0 (mod indStapica n)) (= 0 dodaoN)) (progn (odstampajMatricu stanje indStapica (+ indKuglice n) red 1)))
   ((= indStapica (expt n 2)) (cond 
                                        ((equal (1+ red) n) (progn (format t "~%") (odstampajMatricu stanje 0 (1- n) (1+ red) 0)))
                                        ((> (1+ red) n) (progn (format t "~%") (odstampajMatricu stanje 0 (- (1- (* n 2)) (1+ red)) (1+ red) 0)))
                                        ((< (1+ red) n) (progn (format t "~%") (odstampajMatricu stanje 0 (- (1- (* n 2)) (1+ red)) (1+ red) 0)))))
   (t (progn (odstampajJedno stanje indStapica indKuglice) (odstampajMatricu stanje (1+ indStapica) (1- indKuglice) red 0)))))

(defun odstampajOkvir()
  (format t "~%")
  (loop for ind from 0 to (1- (expt n 2)) do
        (cond
         ((= ind 10) (format t "A "))
         ((= ind 11) (format t "B "))
         ((= ind 12) (format t "C "))
         ((= ind 13) (format t "D "))
         ((= ind 14) (format t "E "))
         ((= ind 15) (format t "F "))
         ((= ind 16) (format t "G "))
         ((= ind 17) (format t "H "))
         ((= ind 18) (format t "I "))
         ((= ind 19) (format t "J "))
         ((= ind 20) (format t "K "))
         ((= ind 21) (format t "L "))
         ((= ind 22) (format t "M "))
         ((= ind 23) (format t "N "))
         ((= ind 24) (format t "O "))
         ((= ind 25) (format t "P "))
         ((= ind 26) (format t "Q "))
         ((= ind 27) (format t "R "))
         ((= ind 28) (format t "S "))
         ((= ind 29) (format t "T "))
         ((= ind 30) (format t "U "))
         ((= ind 31) (format t "V "))
         ((= ind 32) (format t "W "))
         ((= ind 33) (format t "X "))
         ((= ind 34) (format t "Y "))
         ((= ind 35) (format t "Z "))
         (t (format t "~a " ind)))))

;PRIKAZ DOBRO RADI AKO SE KORISTI FIXEDSYS FONT!!! (ZA OSTALE NE MOZEMO DA GARANTUJEMO)

;OVO STAMPA 4X4X4
(setq n '4)
(progn (odstampajOkvir)(format t "~%") (odstampajMatricu (postaviPocetnoStanje '() '() 0 0) 0 (- (* n 2) 2) 1 0))

;OVO STAMPA 6X6X6
(setq n '6)
(progn (odstampajOkvir)(format t "~%") (odstampajMatricu (postaviPocetnoStanje '() '() 0 0) 0 (- (* n 2) 2) 1 0))



;-----KONVERTUJ POTEZ-----

(defun konvertujPotez(ind)
  (cond
   ((equalp ind 'A) '10)
   ((equalp ind 'B) '11)
   ((equalp ind 'C) '12)
   ((equalp ind 'D) '13)
   ((equalp ind 'E) '14)
   ((equalp ind 'F) '15)
   ((equalp ind 'G) '16)
   ((equalp ind 'H) '17)
   ((equalp ind 'I) '18)
   ((equalp ind 'J) '19)
   ((equalp ind 'K) '20)
   ((equalp ind 'L) '21)
   ((equalp ind 'M) '22)
   ((equalp ind 'N) '23)
   ((equalp ind 'O) '24)
   ((equalp ind 'P) '25)
   ((equalp ind 'Q) '26)
   ((equalp ind 'R) '27)
   ((equalp ind 'S) '28)
   ((equalp ind 'T) '29)
   ((equalp ind 'U) '30)
   ((equalp ind 'V) '31)
   ((equalp ind 'W) '32)
   ((equalp ind 'X) '33)
   ((equalp ind 'Y) '34)
   ((equalp ind 'Z) '35)
   (t ind)))

(konvertujPotez '5)
(konvertujPotez 'a)
(konvertujPotez 't)



;-----POTEZ IGRACA-----

(defun potezIgrac(stapic stanje igrac)
  (cond ((potezValjan stapic stanje) (promeniStanje (nth stapic stanje) igrac '()))))

;za testiranje funkcije
(setq stanje '((- - - -)(x x o o)(o x - -)(x x - -)))
(setq n '4)

(potezIgrac 0 stanje 'o)
(potezIgrac 2 stanje 'x)
(potezIgrac 3 stanje 'o)



;-----POTEZ VALJAN-----

(defun potezValjan(stapic stanje)
  (cond (( > (expt n 2) stapic) (not (punJe stapic stanje)))))

;za testiranje funkcije
(setq stanje '((- - - -)(x x o o)(o x - -)(- - - -)))
(setq n 4)

(potezValjan 5 stanje)
(potezValjan 2 stanje)
(potezValjan 1 stanje)



;-----PUN JE-----

(defun punJe(stapic stanje)
  (nemaCrtice (nth stapic stanje)))

;provera da nema nijedne crtice na stapicu tj. tada je stapic pun
(defun nemaCrtice(podLista)
  (cond ((null podLista) T)
        ((equalp '- (car podLista)) '() )
        (t (nemaCrtice (cdr podLista)))))



;-----PROMENI STANJE-----

(defun promeniStanje(stanjeJednogStapica igrac predjeniDeo)
  (cond ((equalp '- (car stanjeJednogStapica)) (append predjeniDeo (cons igrac (cdr stanjeJednogStapica))))
        (t (promeniStanje (cdr stanjeJednogStapica) igrac (append predjeniDeo (list (car stanjeJednogStapica)))))))

;za testiranje funkcije

(promeniStanje '(- - - -) 'o '())
(promeniStanje '(x o x -) 'x '())
(promeniStanje '(o o - -) 'x '())



;-----KRAJ IGRE-----

(defun krajIgre(stanje) 
  (cond
   ((null stanje) (format t "~%Kraj igre~%"))
   ((nemaCrtice (car stanje)) (krajIgre (cdr stanje))) 
   (t (format t "~%Nije kraj igre~%"))))

;za testiranje funkcije

;-----OVDE TREBA DA JAVI DA NIJE KRAJ-----
(setq stanje '((x x x x)(x x o o)(o x x o)(o o o o)(x x x x)(x x o o)(o x x o)(o o o o)(x x x x)(x x - -)(o x x o)(o o o o)(x x x x)(x x o o)(o x x o)(o o o o)))

(krajIgre stanje)

;-----OVDE JAVLJA DA JE KRAJ----
(setq stanje '((x x x x)(x x o o)(o x x o)(o o o o)(x x x x)(x x o o)(o x x o)(o o o o)(x x x x)(x x o o)(o x x o)(o o o o)(x x x x)(x x o o)(o x x o)(o o o o)))

(krajIgre stanje)



;-----PRIKAZ PROIZVOLJNOG STANJA-----

;poziva se za stanje kada je tabla 4x4x4
(setq n '4)

(defun prikazProizvoljnogStanja4()
  (progn (odstampajOkvir)(format t "~%") (odstampajMatricu stanje16 0 (- (* n 2) 2) 1 0) (odstampajOkvir)))

(setq stanje16 '((x x - -)(x x o o)(o x x o)(o o o o)(x x x x)(x x o o)(o x x o)(o o - -)(x x x x)(x x o o)(o x - -)(o o o o)(x x x x)(x x o o)(o x x o)(o o - -)))

(prikazProizvoljnogStanja4)


;poziva se za stanje kada je tabla 6x6x6
(setq n '6)

(defun prikazProizvoljnogStanja6()
  (progn (odstampajOkvir)(format t "~%") (odstampajMatricu stanje36 0 (- (* n 2) 2) 1 0) (odstampajOkvir)))

(setq stanje36 '((x x - - - -)(x x o o - -)(o x x o - -)(o o o o - -)(x x x x - -)(x x o o - -)(o x x o - -)(o o - - - -)(x x x x - -)(x x o o - -)(o x - - - - )(o o o o - -)(x x x x - -)(x x o o - -)(o x x o - -)(o o - - - -)
                 (x x - - - -)(x x o o - -)(o x x o - -)(o o o o - -)(x x x x - -)(x x o o - -)(o x x o - -)(o o - - - -)(x x x x - -)(x x o o - -)(o x - - - -)(o o o o - -)(x x x x - -)(x x o o - -)(o x x o - -)(o o - - - -)
                 (o x x o - -)(o o - - - -)(x x x x - -)(x x o o - -)))

(prikazProizvoljnogStanja6)



















