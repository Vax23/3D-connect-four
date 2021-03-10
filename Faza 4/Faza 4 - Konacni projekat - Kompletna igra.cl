;;------------------------------------------------------------------------------------------------
;;
;; Konacni projekat
;;
;; 1. Funkcije za prikaz
;; 2. Funkcije za postavljanje parametara igre
;; 3. Funkcije za postavljanje i menjanje stanja 
;; 4. Minimax
;; 5. Heuristika kao funkcija
;; 6. Funkcije za odredjivanje pobednika
;; 7. Funkcije za konacno pokretanje
;;
;;------------------------------------------------------------------------------------------------

;;------------------------------------------------------------------------------------------------
;; 1. Funkcije za prikaz 
;;------------------------------------------------------------------------------------------------


(defun igra2(st)
  (prikazProizvoljnogStanja st)
  (start '1))

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

(defun prikaziMatricu()
  (progn (odstampajOkvir)(format t "~%") (odstampajMatricu (postaviPocetnoStanje '() '() 0 0) 0 (- (* n 2) 2) 1 0)))

(defun prikazProizvoljnogStanja(st)
  (progn (odstampajOkvir)(format t "~%") (odstampajMatricu st 0 (- (* n 2) 2) 1 0) (odstampajOkvir)))


;;------------------------------------------------------------------------------------------------
;; 2. Funkcije za postavljanje parametara igre
;;------------------------------------------------------------------------------------------------


(defun velicinaKocke()
   (setq 
         n (progn
                 (format t "~%Unesite velicinu kocke >")
                 (read)))
     (cond
      ((AND (>= n 4) (= (mod n 2) 0)) (format t "Uneta je odgovarajuca velicina kocke"))
     (t (format t "Proverite sta ste uneli kao dimenziju priznaje se samo vrednost koja je veca od 4 i pritom je paran broj") (velicinaKocke))
      ))

(defun igraPrvi ()
  (let* (
         (prvi (progn
                 (format t "~%Unesite ko igra prvi (Racunar-0 Covek-1) >")
                 (read))))
    (cond 
      ( (equal prvi '1) (setq covek 'x) (format t "~%Prvi igra Covek sa operandom ~a~%" covek) (setq comp 'o) (format t "~%Drugi igra Racunar sa operandom ~a~%" comp))
    ( (equal prvi '0) (setq comp 'x) (format t "~%Prvi igra Racunar sa operandom ~a~%" comp) (setq covek 'o) (format t "~%Drugi igra Covek sa operandom ~a~%" covek)))))


;;------------------------------------------------------------------------------------------------
;; 3. Funkcije za postavljanje i menjanje stanja 
;;------------------------------------------------------------------------------------------------


(defun formirajListuMogucihPoteza(stapic igrac trenutnoStanje daLiJeVracenoStanje moguceStanje lista)
  (cond
   ((= stapic (expt n 2)) lista)
   ((= daLiJeVracenoStanje 1)
    (cond 
     ((equalp moguceStanje '()) (formirajListuMogucihPoteza (1+ stapic) igrac trenutnoStanje 0 '() lista))
     (t (formirajListuMogucihPoteza (1+ stapic) igrac trenutnoStanje 0 '() (cons moguceStanje lista)))))
   (t (formirajListuMogucihPoteza stapic igrac trenutnoStanje 1 (nadjiStanje trenutnoStanje stapic igrac) lista))))

(defun konvertujBrojUPotez(ind)
  (cond
   ((= ind 10) 'A)
   ((= ind 11) 'B)
   ((= ind 12) 'C)
   ((= ind 13) 'D)
   ((= ind 14) 'E)
   ((= ind 15) 'F)
   ((= ind 16) 'G)
   ((= ind 17) 'H)
   ((= ind 18) 'I)
   ((= ind 19) 'J)
   ((= ind 20) 'K)
   ((= ind 21) 'L)
   ((= ind 22) 'M)
   ((= ind 23) 'N)
   ((= ind 24) 'O)
   ((= ind 25) 'P)
   ((= ind 26) 'Q)
   ((= ind 27) 'R)
   ((= ind 28) 'S)
   ((= ind 29) 'T)
   ((= ind 30) 'U)
   ((= ind 31) 'V)
   ((= ind 32) 'W)
   ((= ind 33) 'X)
   ((= ind 34) 'Y)         
   ((= ind 35) 'Z)
   (t ind)))

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

(defun nadjiStanje(stanje stapic potez)
  (setq virtuelnoStanje (progn (umetni (progn (promeniStanjeJednogStapica (nth (konvertujPotez stapic) stanje) potez '())) (konvertujPotez stapic) (obrisi '0  (konvertujPotez stapic) stanje))))
  (if (not (equalp virtuelnoStanje stanje)) virtuelnoStanje))

(defun nemaCrtice(podLista)
  (cond ((null podLista) T)
        ((equalp '- (car podLista)) '() )
        (t (nemaCrtice (cdr podLista)))))

(defun obrisi (pom ind lista)
  (cond
   ((null lista) '())
   ((= pom ind) (obrisi (1+ pom) ind (cdr lista)))
   (t (cons (car lista) (obrisi (1+ pom) ind (cdr lista))))))

(defun postaviPocetnoStanje(pomStanje pomPodLista clanoviUPodListi podLista)
  (cond
   ((= podLista (expt n 2)) (progn (setq stanje pomStanje)))
   ((= clanoviUPodListi n) (postaviPocetnoStanje (cons pomPodLista pomStanje) '() 0 (1+ podLista)))
   (t (postaviPocetnoStanje pomStanje (append '(-) pomPodLista) (1+ clanoviUPodListi) podLista))))

(defun potezIgrac(stapic stanje igrac)
  (cond ((potezValjan stapic stanje) (promeniStanje (nth stapic stanje) igrac '()))))

(defun potezValjan(stapic stanje)
  (cond (( > (expt n 2) stapic) (not (punJe stapic stanje)))))

(defun promenaStanja(st stapic potez)
  (setq stanje (progn (umetni (progn (promeniStanjeJednogStapica (nth (konvertujPotez stapic) st) potez '())) (konvertujPotez stapic) (obrisi '0  (konvertujPotez stapic) st))))
  (if (not (equalp stanje st)) (progn (format t "~% Potez: ~a" (konvertujBrojUPotez stapic))
                                         (odstampajOkvir)(format t "~%") (odstampajMatricu stanje 0 (- (* n 2) 2) 1 0) (odstampajOkvir))))

(defun promenaVirtuelnogStanja(st stapic potez)
  (setq virtuelnoStanje (progn (umetni (progn (promeniStanjeJednogStapica (nth (konvertujPotez stapic) st) potez '())) (konvertujPotez stapic) (obrisi '0  (konvertujPotez stapic) st))))
  (if (not (equalp virtuelnoStanje st)) (progn (format t "~% Potez: ~a" (konvertujBrojUPotez stapic))
                                         (odstampajOkvir)(format t "~%") (odstampajMatricu virtuelnoStanje 0 (- (* n 2) 2) 1 0) (odstampajOkvir))))

(defun promeniStanje(stanjeJednogStapica igrac predjeniDeo)
  (cond ((equalp '- (car stanjeJednogStapica)) (append predjeniDeo (cons igrac (cdr stanjeJednogStapica))))
        (t (promeniStanje (cdr stanjeJednogStapica) igrac (append predjeniDeo (list (car stanjeJednogStapica)))))))

(defun promeniStanjeJednogStapica(stanjeJednogStapica igrac predjeniDeo)
  (cond 
   ((= (length predjeniDeo) n) predjeniDeo)
   ((equalp '- (car stanjeJednogStapica)) (append predjeniDeo (cons igrac (cdr stanjeJednogStapica))))
   (t (promeniStanjeJednogStapica (cdr stanjeJednogStapica) igrac (append predjeniDeo (list (car stanjeJednogStapica)))))))

(defun punJe(stapic stanje)
  (nemaCrtice (nth stapic stanje)))

(defun sviMoguciPotezi(igrac)
  (progn (format t "~% Moguci potezi iz stanja: ")
    (odstampajOkvir) (format t "~%") (odstampajMatricu stanje 0 (- (* n 2) 2) 1 0) (odstampajOkvir)
    (format t "~% su: ")
    (loop for stapic from 0 to (1- (expt n 2)) do
        (promenaVirtuelnogStanja stanje stapic igrac))))

(defun umetni (el n lista)
  (obrisi 0 3 lista)
  (cond
   ((= n 0) (cons el lista))
   (t (cons (car lista) (umetni el (1- n) (cdr lista))))))

(defun unosPoteza(st igrac)
  (let* (
         (stapic (progn
                   (format t "~%Unesite zeljeni stapic >")
                   (read))))
    (cond 
     ((potezValjan (konvertujPotez stapic) st) (promenaStanja st (konvertujPotez stapic) igrac))
     (t (format t "~%Ovaj potez nije moguc!Pokusajte neki drugi!") (unosPoteza st igrac)))))


;;------------------------------------------------------------------------------------------------
;; 4. Minimax
;;------------------------------------------------------------------------------------------------


(defun a-b-Minimax(sstanje potez dubina start alfa beta maxIgrac)
  (cond 
   ((zerop dubina) (list sstanje (or (da-li-je-kraj sstanje) (proceni-stanje sstanje maxIgrac))))
   (t (let ((lista-poteza (formiraj-listu-mogucih-poteza 0 potez sstanje 0 '() '()))
         (m-m (if (equalp potez comp) 'max-stanje 'min-stanje)))
         (cond 
          ((null lista-poteza) (list sstanje (or (da-li-je-kraj sstanje) (proceni-stanje sstanje maxIgrac))))
          (start (apply m-m (list (mapcar (lambda (x)
                                             (a-b-Minimax x (if (equal potez 'x) 'o 'x) (1- dubina) '() 0 0 (if (equal maxIgrac 'x) 'o 'x))) lista-poteza))))
          (t (cons sstanje (cdr (apply m-m (list (mapcar (lambda (x) 
                                                           (a-b-Minimax x (if (equal potez 'x) 'o 'x) (1- dubina) '() 0 0 (if (equal maxIgrac 'x) 'o 'x) )) lista-poteza)))))))))))

(defun da-li-je-kraj(st) 
  (cond
   ((null st) '())
   ((nemaCrtice (car st)) (da-li-je-kraj (cdr st)))
   (t '())))

(defun formiraj-listu-mogucih-poteza(stapic igrac trenutno-stanje da-li-je-vraceno-stanje moguce-stanje lista)
  (cond
   ((= stapic (expt n 2)) lista)
   ((= da-li-je-vraceno-stanje 1)
    (cond 
     ((equalp moguce-stanje '()) 
      (formiraj-listu-mogucih-poteza (1+ stapic) igrac trenutno-stanje 0 '() lista))
     (t 
      (formiraj-listu-mogucih-poteza (1+ stapic) igrac trenutno-stanje 0 '() (cons moguce-stanje lista)))))
   (t (formiraj-listu-mogucih-poteza stapic igrac trenutno-stanje 1 (nadjiStanje trenutno-stanje stapic igrac) lista))))

(defun  max-stanje (lsv)
  (max-stanje-i (cdr lsv) (car lsv)))

(defun max-stanje-i (lsv stanje-vrednost)
  (cond ((null lsv) stanje-vrednost)
        ((> (cadar lsv) (cadr stanje-vrednost))
         (max-stanje-i (cdr lsv) (car lsv)))
        (t (max-stanje-i (cdr lsv) stanje-vrednost))))

(defun min-stanje (lsv)
  (min-stanje-i (cdr lsv) (car lsv)))

(defun min-stanje-i (lsv stanje-vrednost)
  (cond ((null lsv) stanje-vrednost)
        ((< (cadar lsv) (cadr stanje-vrednost))
         (min-stanje-i (cdr lsv) (car lsv)))
        (t (min-stanje-i (cdr lsv) stanje-vrednost)))) 

(defun proceni-stanje (st igrac)
  (oceniStanje st igrac))


;;------------------------------------------------------------------------------------------------
;; 5. Heuristika kao funkcija
;;------------------------------------------------------------------------------------------------


;;----- Sve zajedno -----

(defun oceniStanje(st igrac)
  (+ (* 16 (car (pobednikCetiri st igrac))) 
     (* 9 (car (pobednikTri st igrac))) 
     (* 4 (car (pobednikDve st igrac))) 
     (car (pobednikJedna st igrac))))

;;----- Jedna kuglica -----

(defun pobednikStapiciJednaZaredom (st stapic predjeneKuglice kugliceZaredom igrac poeni)
  (cond 
   ((= stapic (expt n 2)) (list poeni))
   ((= predjeneKuglice (1+ n)) (pobednikStapiciJednaZaredom st (1+ stapic) 0 0 igrac poeni))
   ((cond 
                    ((equalp (nth predjeneKuglice (nth stapic st)) igrac) (pobednikStapiciJednaZaredom st stapic (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                    ((and (= kugliceZaredom 1) (or (= predjeneKuglice (- (/ n 2) 1)) (if (= n 6) (= predjeneKuglice (/ n 2)))) (equalp (nth predjeneKuglice (nth stapic st)) '-)) (pobednikStapiciJednaZaredom st stapic (1+ predjeneKuglice) 0 igrac (1+ poeni)))
                    (t (pobednikStapiciJednaZaredom st stapic (1+ predjeneKuglice) 0 igrac poeni))))
   ((= kugliceZaredom 1) (pobednikStapiciJednaZaredom st stapic predjeneKuglice kugliceZaredom igrac poeni))
   (t (cond
       ((equalp (nth predjeneKuglice (nth stapic st)) igrac) (pobednikStapiciJednaZaredom st stapic (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
       (t (pobednikStapiciJednaZaredom st stapic (1+ predjeneKuglice) 0 igrac poeni))))))

(defun pobednikPoNivoimaJednaZaredom (st nivo stapic vrsta kolona dijagonala predjeneKuglice kugliceZaredom igrac poeni)
  (cond
   ((= nivo (1+ n)) (list poeni))
   ((> predjeneKuglice n) (cond 
                           ((< vrsta n) (pobednikPoNivoimaJednaZaredom st nivo (1+ vrsta) (1+ vrsta) -1 0 0 0 igrac poeni))
                           ((< kolona n) (pobednikPoNivoimaJednaZaredom st nivo (* n (1+ kolona)) vrsta (1+ kolona) 0 0 0 igrac poeni))
                           ((= dijagonala 0) (pobednikPoNivoimaJednaZaredom st nivo 0 vrsta kolona (1+ dijagonala) 0 0 igrac poeni))
                           ((= dijagonala 1) (pobednikPoNivoimaJednaZaredom st nivo (1- n) vrsta kolona (1+ dijagonala) 0 0 igrac poeni))
                           (t (pobednikPoNivoimaJednaZaredom st (1+ nivo) 0 0 0 0 0 0 igrac poeni))))
   ((cond
                    ((< vrsta n) (cond
                                  ((equalp (nth nivo (nth stapic st)) igrac) 
                                   (pobednikPoNivoimaJednaZaredom st nivo (+ stapic n) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom)  igrac poeni))
                                  ((and (= kugliceZaredom 1) (or (= predjeneKuglice (- (/ n 2) 1)) (if (= n 6) (= predjeneKuglice (/ n 2)))) (equalp (nth predjeneKuglice (nth stapic st)) '-))
                                   (pobednikPoNivoimaJednaZaredom st nivo (+ stapic n) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac (1+ poeni)))
                                  (t 
                                   (pobednikPoNivoimaJednaZaredom st nivo (+ stapic n) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
                    ((< kolona n) (cond
                                   ((equalp (nth nivo (nth stapic st)) igrac) 
                                    (pobednikPoNivoimaJednaZaredom st nivo (1+ stapic) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                                   ((and (= kugliceZaredom 1) (or (= predjeneKuglice (- (/ n 2) 1)) (if (= n 6) (= predjeneKuglice (/ n 2)))) (equalp (nth predjeneKuglice (nth stapic st)) '-))
                                   (pobednikPoNivoimaJednaZaredom st nivo (1+ stapic) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac (1+ poeni)))
                                   (t 
                                    (pobednikPoNivoimaJednaZaredom st nivo (1+ stapic) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
                    ((= dijagonala 1) (cond 
                                       ((equalp (nth nivo (nth stapic st)) igrac) 
                                        (pobednikPoNivoimaJednaZaredom st nivo (+ stapic (1+ n)) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                                       ((and (= kugliceZaredom 1) (or (= predjeneKuglice (- (/ n 2) 1)) (if (= n 6) (= predjeneKuglice (/ n 2)))) (equalp (nth predjeneKuglice (nth stapic st)) '-))
                                        (pobednikPoNivoimaJednaZaredom st nivo (+ stapic (1+ n)) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac (1+ poeni)))
                                       (t 
                                        (pobednikPoNivoimaJednaZaredom st nivo (+ stapic (1+ n)) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
                    (t (cond
                        ((equalp (nth nivo (nth stapic st)) igrac) 
                         (pobednikPoNivoimaJednaZaredom st nivo (+ stapic (1- n)) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                        ((and (= kugliceZaredom 1) (or (= predjeneKuglice (- (/ n 2) 1)) (if (= n 6) (= predjeneKuglice (/ n 2)))) (equalp (nth predjeneKuglice (nth stapic st)) '-))
                         (pobednikPoNivoimaJednaZaredom st nivo (+ stapic (1- n)) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac (1+ poeni)))
                        (t 
                         (pobednikPoNivoimaJednaZaredom st nivo (+ stapic (1- n)) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))))
   ((= kugliceZaredom 1) (pobednikPoNivoimaJednaZaredom st nivo stapic vrsta kolona dijagonala predjeneKuglice kugliceZaredom igrac poeni))
   (t (cond
       ((< vrsta n) (cond
                     ((equalp (nth nivo (nth stapic st)) igrac) 
                      (pobednikPoNivoimaJednaZaredom st nivo (+ stapic n) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                     (t 
                      (pobednikPoNivoimaJednaZaredom st nivo (+ stapic n) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
       ((< kolona n) (cond
                      ((equalp (nth nivo (nth stapic st)) igrac) 
                       (pobednikPoNivoimaJednaZaredom st nivo (1+ stapic) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                      (t 
                       (pobednikPoNivoimaJednaZaredom st nivo (1+ stapic) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
       ((= dijagonala 1) (cond
                          ((equalp (nth nivo (nth stapic st)) igrac) 
                           (pobednikPoNivoimaJednaZaredom st nivo (+ stapic (1+ n)) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                          (t 
                           (pobednikPoNivoimaJednaZaredom st nivo (+ stapic (1+ n)) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
       (t (cond
           ((equalp (nth nivo (nth stapic st)) igrac) 
            (pobednikPoNivoimaJednaZaredom st nivo (+ stapic (1- n)) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
           (t 
            (pobednikPoNivoimaJednaZaredom st nivo (+ stapic (1- n)) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))))))

(defun pobednikPreseciVrsteIKoloneDijagonalnoJednaZaredom (st stapic nivo vrsta kolona dijagonala predjeneKuglice kugliceZaredom igrac poeni)
  (cond 
   ((AND (= kugliceZaredom 0) (> vrsta (1- n)) (> kolona (1- n))) (list poeni))
   ((< nivo 0) (pobednikPreseciVrsteIKoloneDijagonalnoJednaZaredom st stapic 0 vrsta kolona dijagonala predjeneKuglice kugliceZaredom igrac poeni))
   ((< stapic 0) (pobednikPreseciVrsteIKoloneDijagonalnoJednaZaredom st 0 nivo vrsta kolona dijagonala predjeneKuglice 0 igrac poeni))
   ((OR (> predjeneKuglice n) (AND (= kolona -1) (= vrsta n))) (cond
                                                                ((= dijagonala 0) (cond 
                                                                                   ((< vrsta n) (pobednikPreseciVrsteIKoloneDijagonalnoJednaZaredom st vrsta 0 vrsta 0 1 0 0 igrac poeni))
                                                                                   (t (pobednikPreseciVrsteIKoloneDijagonalnoJednaZaredom st (* kolona n) 0 vrsta  kolona 1 0 0 igrac poeni))))
                                                                (t (cond
                                                                    ((= vrsta (1- n)) (pobednikPreseciVrsteIKoloneDijagonalnoJednaZaredom st (* kolona n) (1- n) (1+ vrsta) kolona 0 0 0 igrac poeni))
                                                                    ((< vrsta n) (pobednikPreseciVrsteIKoloneDijagonalnoJednaZaredom st (1+ vrsta) (1- n) (1+ vrsta) kolona 0 0 0 igrac poeni))
                                                                    ((< kolona n) (pobednikPreseciVrsteIKoloneDijagonalnoJednaZaredom st (* (1+ kolona) n) (1- n) vrsta (1+ kolona) 0 0 0 igrac poeni))
                                                                    (t (pobednikPreseciVrsteIKoloneDijagonalnoJednaZaredom st stapic nivo vrsta kolona dijagonala predjeneKuglice kugliceZaredom igrac poeni))))))
   ((cond 
                    ((= dijagonala 0) (cond
                                       ((< vrsta n) (cond 
                                                     ((AND (< predjeneKuglice n) (equalp (nth nivo (nth stapic st)) igrac)) (pobednikPreseciVrsteIKoloneDijagonalnoJednaZaredom st (+ stapic n) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                                                     ((and (= kugliceZaredom 1) (or (= predjeneKuglice (- (/ n 2) 1)) (if (= n 6) (= predjeneKuglice (/ n 2)))) (equalp (nth predjeneKuglice (nth stapic st)) '-))
                                                      (pobednikPreseciVrsteIKoloneDijagonalnoJednaZaredom st (+ stapic n) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac (1+ poeni)))
                                                     (t (pobednikPreseciVrsteIKoloneDijagonalnoJednaZaredom st (+ stapic n) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
                                       (t (cond
                                           ((AND (< predjeneKuglice n) (equalp (nth nivo (nth stapic st)) igrac)) (pobednikPreseciVrsteIKoloneDijagonalnoJednaZaredom st (1+ stapic) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                                                              (t (pobednikPreseciVrsteIKoloneDijagonalnoJednaZaredom st (1+ stapic) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))))
                    ((= dijagonala 1) (cond
                                       ((< vrsta n) (cond 
                                                     ((AND (< predjeneKuglice n) (equalp (nth nivo (nth stapic st)) igrac)) (pobednikPreseciVrsteIKoloneDijagonalnoJednaZaredom st (+ stapic n) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                                                     ((and (= kugliceZaredom 1) (or (= predjeneKuglice (- (/ n 2) 1)) (if (= n 6) (= predjeneKuglice (/ n 2)))) (equalp (nth predjeneKuglice (nth stapic st)) '-))
                                                      (pobednikPreseciVrsteIKoloneDijagonalnoJednaZaredom st (+ stapic n) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac (1+ poeni)))
                                                     (t (pobednikPreseciVrsteIKoloneDijagonalnoJednaZaredom st (+ stapic n) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
                                       (t (cond
                                           ((AND (< predjeneKuglice n) (equalp (nth nivo (nth stapic st)) igrac)) (pobednikPreseciVrsteIKoloneDijagonalnoJednaZaredom st (1+ stapic) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))
                                                              (t (pobednikPreseciVrsteIKoloneDijagonalnoJednaZaredom st (1+ stapic) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))))))
   ((= kugliceZaredom 1) (pobednikPreseciVrsteIKoloneDijagonalnoJednaZaredom st stapic nivo vrsta kolona dijagonala predjeneKuglice kugliceZaredom igrac poeni))
   (t (cond
       ((= dijagonala 0) (cond
                          ((< vrsta n) (cond 
                                                 ((equalp (nth nivo (nth stapic st)) igrac) (pobednikPreseciVrsteIKoloneDijagonalnoJednaZaredom st (+ stapic n) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                                                 (t (pobednikPreseciVrsteIKoloneDijagonalnoJednaZaredom st (+ stapic n) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
                          (t (cond
                              ((equalp (nth nivo (nth stapic st)) igrac) (pobednikPreseciVrsteIKoloneDijagonalnoJednaZaredom st (1+ stapic) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                              (t (pobednikPreseciVrsteIKoloneDijagonalnoJednaZaredom st (1+ stapic) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))))
       ((= dijagonala 1) (cond
                          ((< vrsta n) (cond 
                                                 ((equalp (nth nivo (nth stapic st)) igrac) (pobednikPreseciVrsteIKoloneDijagonalnoJednaZaredom st (+ stapic n) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                                                 (t (pobednikPreseciVrsteIKoloneDijagonalnoJednaZaredom st (+ stapic n) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
                          (t (cond
                              ((equalp (nth nivo (nth stapic st)) igrac) (pobednikPreseciVrsteIKoloneDijagonalnoJednaZaredom st (1+ stapic) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                              (t (pobednikPreseciVrsteIKoloneDijagonalnoJednaZaredom st (1+ stapic) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))))))))


(defun pobednikUnutrasnjostDijagonalnoJednaZaredom (st stapic nivo presek dijagonala predjeneKuglice kugliceZaredom igrac poeni)
  (cond 
   ((AND (= presek 2) (= dijagonala 2)) (list poeni))
   ((< nivo 0) (pobednikUnutrasnjostDijagonalnoJednaZaredom st (expt n 2) 0 presek dijagonala predjeneKuglice kugliceZaredom igrac poeni))
   ((AND (> predjeneKuglice (1- n)) (= kugliceZaredom 0)) (cond
                          ((= presek 0) (cond
                                         ((= dijagonala 0) (pobednikUnutrasnjostDijagonalnoJednaZaredom st 0 (1- n) presek 1 0 0 igrac poeni))
                                         (t (pobednikUnutrasnjostDijagonalnoJednaZaredom st (1- n) 0 1 0 0 0 igrac poeni))))
                          (t (cond
                              ((= dijagonala 0) (pobednikUnutrasnjostDijagonalnoJednaZaredom st (1- n) (1- n) presek 1 0 0 igrac poeni))
                              (t (pobednikUnutrasnjostDijagonalnoJednaZaredom st stapic nivo 2 2 0 0 igrac poeni))))))
   ((cond 
                    ((= presek 0) (cond
                                   ((= dijagonala 0) (cond
                                                      ((equalp (nth nivo (nth stapic st)) igrac) 
                                                       (pobednikUnutrasnjostDijagonalnoJednaZaredom st (+ stapic (1+ n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                                                      ((and (= kugliceZaredom 1) (or (= predjeneKuglice (- (/ n 2) 1)) (if (= n 6) (= predjeneKuglice (/ n 2)))) (equalp (nth predjeneKuglice (nth stapic st)) '-))
                                                       (pobednikUnutrasnjostDijagonalnoJednaZaredom st (+ stapic (1+ n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) 0 igrac (1+ poeni)))
                                                      (t 
                                                       (pobednikUnutrasnjostDijagonalnoJednaZaredom st (+ stapic (1+ n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
                                   (t (cond
                                       ((equalp (nth nivo (nth stapic st)) igrac)
                                        (pobednikUnutrasnjostDijagonalnoJednaZaredom st (+ stapic (1+ n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                                       ((and (= kugliceZaredom 1) (or (= predjeneKuglice (- (/ n 2) 1)) (if (= n 6) (= predjeneKuglice (/ n 2)))) (equalp (nth predjeneKuglice (nth stapic st)) '-))
                                        (pobednikUnutrasnjostDijagonalnoJednaZaredom st (+ stapic (1+ n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) 0 igrac (1+ poeni)))
                                       (t 
                                        (pobednikUnutrasnjostDijagonalnoJednaZaredom st (+ stapic (1+ n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) 0 igrac poeni))))))
                    (t (cond
                        ((= dijagonala 0) (cond
                                           ((equalp (nth nivo (nth stapic st)) igrac) 
                                            (pobednikUnutrasnjostDijagonalnoJednaZaredom st (+ stapic (1- n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                                           ((and (= kugliceZaredom 1) (or (= predjeneKuglice (- (/ n 2) 1)) (if (= n 6) (= predjeneKuglice (/ n 2)))) (equalp (nth predjeneKuglice (nth stapic st)) '-))
                                            (pobednikUnutrasnjostDijagonalnoJednaZaredom st (+ stapic (1- n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) 0 igrac (1+ poeni)))
                                           (t 
                                            (pobednikUnutrasnjostDijagonalnoJednaZaredom st (+ stapic (1- n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
                        (t (cond
                            ((equalp (nth nivo (nth stapic st)) igrac) 
                             (pobednikUnutrasnjostDijagonalnoJednaZaredom st (+ stapic (1- n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                            ((and (= kugliceZaredom 1) (or (= predjeneKuglice (- (/ n 2) 1)) (if (= n 6) (= predjeneKuglice (/ n 2)))) (equalp (nth predjeneKuglice (nth stapic st)) '-))
                             (pobednikUnutrasnjostDijagonalnoJednaZaredom st (+ stapic (1- n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) 0 igrac (1+ poeni)))
                            (t 
                             (pobednikUnutrasnjostDijagonalnoJednaZaredom st (+ stapic (1- n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) 0 igrac poeni))))))))
   ((= kugliceZaredom 1) (pobednikUnutrasnjostDijagonalnoJednaZaredom st stapic nivo presek dijagonala predjeneKuglice kugliceZaredom igrac poeni))
   (t (cond
       ((= presek 0) (cond
                      ((= dijagonala 0) (cond
                                         ((equalp (nth nivo (nth stapic st)) igrac) 
                                          (pobednikUnutrasnjostDijagonalnoJednaZaredom st (+ stapic (1+ n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                                         (t 
                                          (pobednikUnutrasnjostDijagonalnoJednaZaredom st (+ stapic (1+ n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
                      (t (cond
                          ((equalp (nth nivo (nth stapic st)) igrac) 
                           (pobednikUnutrasnjostDijagonalnoJednaZaredom st (+ stapic (1+ n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                          (t 
                           (pobednikUnutrasnjostDijagonalnoJednaZaredom st (+ stapic (1+ n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) 0 igrac poeni))))))
       (t (cond
           ((= dijagonala 0) (cond
                              ((equalp (nth nivo (nth stapic st)) igrac) 
                               (pobednikUnutrasnjostDijagonalnoJednaZaredom st (+ stapic (1- n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                              (t 
                               (pobednikUnutrasnjostDijagonalnoJednaZaredom st (+ stapic (1- n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
           (t (cond
               ((equalp (nth nivo (nth stapic st)) igrac) 
                (pobednikUnutrasnjostDijagonalnoJednaZaredom st (+ stapic (1- n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
               (t 
                (pobednikUnutrasnjostDijagonalnoJednaZaredom st (+ stapic (1- n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) 0 igrac poeni))))))))))

(defun pobednikJedna(st igrac)
  (mapcar '+ (pobednikStapiciJednaZaredom st 0 0 0 igrac 0)
       (pobednikPoNivoimaJednaZaredom st 0 0 0 0 0 0 0 igrac 0)
       (pobednikPreseciVrsteIKoloneDijagonalnoJednaZaredom st 0 3 0 0 0 0 0 igrac 0)                                               
       (pobednikUnutrasnjostDijagonalnoJednaZaredom st 0 0 0 0 0 0 igrac 0)))

;;----- Dve kuglice -----


(defun pobednikStapiciDvaZaredom (st stapic predjeneKuglice kugliceZaredom igrac poeni)
  (cond 
   ((= stapic (expt n 2)) (list poeni))
   ((= predjeneKuglice (1+ n)) (pobednikStapiciDvaZaredom st (1+ stapic) 0 0 igrac poeni))
   ((cond 
                    ((equalp (nth predjeneKuglice (nth stapic st)) igrac) (pobednikStapiciDvaZaredom st stapic (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                    ((and (= kugliceZaredom 2) (equalp (nth predjeneKuglice (nth stapic st)) '-)) (pobednikStapiciDvaZaredom st stapic (1+ predjeneKuglice) 0 igrac (1+ poeni)))
                    (t (pobednikStapiciDvaZaredom st stapic (1+ predjeneKuglice) 0 igrac poeni))))
   ((= kugliceZaredom 2) (pobednikStapiciDvaZaredom st stapic predjeneKuglice kugliceZaredom igrac poeni))
   (t (cond
       ((equalp (nth predjeneKuglice (nth stapic st)) igrac) (pobednikStapiciDvaZaredom st stapic (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
       (t (pobednikStapiciDvaZaredom st stapic (1+ predjeneKuglice) 0 igrac poeni))))))

(defun pobednikPoNivoimaDvaZaredom (st nivo stapic vrsta kolona dijagonala predjeneKuglice kugliceZaredom igrac poeni)
  (cond
   ((= nivo (1+ n)) (list poeni))
   ((> predjeneKuglice n) (cond 
                           ((< vrsta n) (pobednikPoNivoimaDvaZaredom st nivo (1+ vrsta) (1+ vrsta) -1 0 0 0 igrac poeni))
                           ((< kolona n) (pobednikPoNivoimaDvaZaredom st nivo (* n (1+ kolona)) vrsta (1+ kolona) 0 0 0 igrac poeni))
                           ((= dijagonala 0) (pobednikPoNivoimaDvaZaredom st nivo 0 vrsta kolona (1+ dijagonala) 0 0 igrac poeni))
                           ((= dijagonala 1) (pobednikPoNivoimaDvaZaredom st nivo (1- n) vrsta kolona (1+ dijagonala) 0 0 igrac poeni))
                           (t (pobednikPoNivoimaDvaZaredom st (1+ nivo) 0 0 0 0 0 0 igrac poeni))))
   ((cond
                    ((< vrsta n) (cond
                                  ((equalp (nth nivo (nth stapic st)) igrac) 
                                   (pobednikPoNivoimaDvaZaredom st nivo (+ stapic n) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom)  igrac poeni))
                                  ((and (= kugliceZaredom 2) (equalp (nth nivo (nth stapic st)) '-)) 
                                   (pobednikPoNivoimaDvaZaredom st nivo (+ stapic n) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac (1+ poeni)))
                                  (t 
                                   (pobednikPoNivoimaDvaZaredom st nivo (+ stapic n) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
                    ((< kolona n) (cond
                                   ((equalp (nth nivo (nth stapic st)) igrac) 
                                    (pobednikPoNivoimaDvaZaredom st nivo (1+ stapic) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                                   ((and (= kugliceZaredom 2) (equalp (nth nivo (nth stapic st)) '-)) 
                                   (pobednikPoNivoimaDvaZaredom st nivo (1+ stapic) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac (1+ poeni)))
                                   (t 
                                    (pobednikPoNivoimaDvaZaredom st nivo (1+ stapic) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
                    ((= dijagonala 1) (cond 
                                       ((equalp (nth nivo (nth stapic st)) igrac) 
                                        (pobednikPoNivoimaDvaZaredom st nivo (+ stapic (1+ n)) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                                       ((and (= kugliceZaredom 2) (equalp (nth nivo (nth stapic st)) '-)) 
                                        (pobednikPoNivoimaDvaZaredom st nivo (+ stapic (1+ n)) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac (1+ poeni)))
                                       (t 
                                        (pobednikPoNivoimaDvaZaredom st nivo (+ stapic (1+ n)) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
                    (t (cond
                        ((equalp (nth nivo (nth stapic st)) igrac) 
                         (pobednikPoNivoimaDvaZaredom st nivo (+ stapic (1- n)) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                        ((and (= kugliceZaredom 2) (equalp (nth nivo (nth stapic st)) '-)) 
                         (pobednikPoNivoimaDvaZaredom st nivo (+ stapic (1- n)) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac (1+ poeni)))
                        (t 
                         (pobednikPoNivoimaDvaZaredom st nivo (+ stapic (1- n)) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))))
   
   ((= kugliceZaredom 2) (pobednikPoNivoimaDvaZaredom st nivo stapic vrsta kolona dijagonala predjeneKuglice kugliceZaredom igrac poeni))
   (t (cond
       ((< vrsta n) (cond
                     ((equalp (nth nivo (nth stapic st)) igrac) 
                      (pobednikPoNivoimaDvaZaredom st nivo (+ stapic n) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                     (t 
                      (pobednikPoNivoimaDvaZaredom st nivo (+ stapic n) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
       ((< kolona n) (cond
                      ((equalp (nth nivo (nth stapic st)) igrac) 
                       (pobednikPoNivoimaDvaZaredom st nivo (1+ stapic) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                      (t 
                       (pobednikPoNivoimaDvaZaredom st nivo (1+ stapic) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
       ((= dijagonala 1) (cond
                          ((equalp (nth nivo (nth stapic st)) igrac) 
                           (pobednikPoNivoimaDvaZaredom st nivo (+ stapic (1+ n)) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                          (t 
                           (pobednikPoNivoimaDvaZaredom st nivo (+ stapic (1+ n)) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
       (t (cond
           ((equalp (nth nivo (nth stapic st)) igrac) 
            (pobednikPoNivoimaDvaZaredom st nivo (+ stapic (1- n)) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
           (t 
            (pobednikPoNivoimaDvaZaredom st nivo (+ stapic (1- n)) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))))))

(defun pobednikPreseciVrsteIKoloneDijagonalnoDveZaredom (st stapic nivo vrsta kolona dijagonala predjeneKuglice kugliceZaredom igrac poeni)
  (cond 
   ((AND (= kugliceZaredom 0) (> vrsta (1- n)) (> kolona (1- n))) (list poeni))
   ((< nivo 0) (pobednikPreseciVrsteIKoloneDijagonalnoDveZaredom st stapic 0 vrsta kolona dijagonala predjeneKuglice kugliceZaredom igrac poeni))
   ((< stapic 0) (pobednikPreseciVrsteIKoloneDijagonalnoDveZaredom st 0 nivo vrsta kolona dijagonala predjeneKuglice 0 igrac poeni))
   ((OR (> predjeneKuglice n) (AND (= kolona -1) (= vrsta n))) (cond
                                                                ((= dijagonala 0) (cond 
                                                                                   ((< vrsta n) (pobednikPreseciVrsteIKoloneDijagonalnoDveZaredom st vrsta 0 vrsta 0 1 0 0 igrac poeni))
                                                                                   (t (pobednikPreseciVrsteIKoloneDijagonalnoDveZaredom st (* kolona n) 0 vrsta  kolona 1 0 0 igrac poeni))))
                                                                (t (cond
                                                                    ((= vrsta (1- n)) (pobednikPreseciVrsteIKoloneDijagonalnoDveZaredom st (* kolona n) (1- n) (1+ vrsta) kolona 0 0 0 igrac poeni))
                                                                    ((< vrsta n) (pobednikPreseciVrsteIKoloneDijagonalnoDveZaredom st (1+ vrsta) (1- n) (1+ vrsta) kolona 0 0 0 igrac poeni))
                                                                    ((< kolona n) (pobednikPreseciVrsteIKoloneDijagonalnoDveZaredom st (* (1+ kolona) n) (1- n) vrsta (1+ kolona) 0 0 0 igrac poeni))
                                                                    (t (pobednikPreseciVrsteIKoloneDijagonalnoDveZaredom st stapic nivo vrsta kolona dijagonala predjeneKuglice kugliceZaredom igrac poeni))))))
   ((cond 
                    ((= dijagonala 0) (cond
                                       ((< vrsta n) (cond 
                                                     ((AND (< predjeneKuglice n) (equalp (nth nivo (nth stapic st)) igrac)) (pobednikPreseciVrsteIKoloneDijagonalnoDveZaredom st (+ stapic n) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                                                     ((and (= kugliceZaredom 2) (equalp (nth nivo (nth stapic st)) '-)) 
                                                      (pobednikPreseciVrsteIKoloneDijagonalnoDveZaredom st (+ stapic n) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac (1+ poeni)))
                                                     (t (pobednikPreseciVrsteIKoloneDijagonalnoDveZaredom st (+ stapic n) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
                                       (t (cond
                                           ((AND (< predjeneKuglice n) (equalp (nth nivo (nth stapic st)) igrac)) (pobednikPreseciVrsteIKoloneDijagonalnoDveZaredom st (1+ stapic) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                                                              (t (pobednikPreseciVrsteIKoloneDijagonalnoDveZaredom st (1+ stapic) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))))
                    ((= dijagonala 1) (cond
                                       ((< vrsta n) (cond 
                                                     ((AND (< predjeneKuglice n) (equalp (nth nivo (nth stapic st)) igrac)) (pobednikPreseciVrsteIKoloneDijagonalnoDveZaredom st (+ stapic n) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                                                     ((and (= kugliceZaredom 2) (equalp (nth nivo (nth stapic st)) '-)) 
                                                      (pobednikPreseciVrsteIKoloneDijagonalnoDveZaredom st (+ stapic n) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac (1+ poeni)))
                                                     (t (pobednikPreseciVrsteIKoloneDijagonalnoDveZaredom st (+ stapic n) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
                                       (t (cond
                                           ((AND (< predjeneKuglice n) (equalp (nth nivo (nth stapic st)) igrac)) (pobednikPreseciVrsteIKoloneDijagonalnoDveZaredom st (1+ stapic) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))
                                                              (t (pobednikPreseciVrsteIKoloneDijagonalnoDveZaredom st (1+ stapic) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))))))
   ((= kugliceZaredom 2) (pobednikPreseciVrsteIKoloneDijagonalnoDveZaredom st stapic nivo vrsta kolona dijagonala predjeneKuglice kugliceZaredom igrac poeni))
   (t (cond
       ((= dijagonala 0) (cond
                          ((< vrsta n) (cond 
                                                 ((equalp (nth nivo (nth stapic st)) igrac) (pobednikPreseciVrsteIKoloneDijagonalnoDveZaredom st (+ stapic n) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                                                 (t (pobednikPreseciVrsteIKoloneDijagonalnoDveZaredom st (+ stapic n) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
                          (t (cond
                              ((equalp (nth nivo (nth stapic st)) igrac) (pobednikPreseciVrsteIKoloneDijagonalnoDveZaredom st (1+ stapic) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                              (t (pobednikPreseciVrsteIKoloneDijagonalnoDveZaredom st (1+ stapic) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))))
       ((= dijagonala 1) (cond
                          ((< vrsta n) (cond 
                                                 ((equalp (nth nivo (nth stapic st)) igrac) (pobednikPreseciVrsteIKoloneDijagonalnoDveZaredom st (+ stapic n) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                                                 (t (pobednikPreseciVrsteIKoloneDijagonalnoDveZaredom st (+ stapic n) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
                          (t (cond
                              ((equalp (nth nivo (nth stapic st)) igrac) (pobednikPreseciVrsteIKoloneDijagonalnoDveZaredom st (1+ stapic) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                              (t (pobednikPreseciVrsteIKoloneDijagonalnoDveZaredom st (1+ stapic) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))))))))


(defun pobednikUnutrasnjostDijagonalnoDvaZaredom (st stapic nivo presek dijagonala predjeneKuglice kugliceZaredom igrac poeni)
  (cond 
   ((AND (= presek 2) (= dijagonala 2)) (list poeni))
   ((< nivo 0) (pobednikUnutrasnjostDijagonalnoDvaZaredom st (expt n 2) 0 presek dijagonala predjeneKuglice kugliceZaredom igrac poeni))
   ((AND (> predjeneKuglice (1- n)) (= kugliceZaredom 0)) (cond
                          ((= presek 0) (cond
                                         ((= dijagonala 0) (pobednikUnutrasnjostDijagonalnoDvaZaredom st 0 (1- n) presek 1 0 0 igrac poeni))
                                         (t (pobednikUnutrasnjostDijagonalnoDvaZaredom st (1- n) 0 1 0 0 0 igrac poeni))))
                          (t (cond
                              ((= dijagonala 0) (pobednikUnutrasnjostDijagonalnoDvaZaredom st (1- n) (1- n) presek 1 0 0 igrac poeni))
                              (t (pobednikUnutrasnjostDijagonalnoDvaZaredom st stapic nivo 2 2 0 0 igrac poeni))))))
   ((cond 
                    ((= presek 0) (cond
                                   ((= dijagonala 0) (cond
                                                      ((equalp (nth nivo (nth stapic st)) igrac) 
                                                       (pobednikUnutrasnjostDijagonalnoDvaZaredom st (+ stapic (1+ n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                                                      ((and (= kugliceZaredom 2) (equalp (nth nivo (nth stapic st)) '-)) 
                                                       (pobednikUnutrasnjostDijagonalnoDvaZaredom st (+ stapic (1+ n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) 0 igrac (1+ poeni)))
                                                      (t 
                                                       (pobednikUnutrasnjostDijagonalnoDvaZaredom st (+ stapic (1+ n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
                                   (t (cond
                                       ((equalp (nth nivo (nth stapic st)) igrac)
                                        (pobednikUnutrasnjostDijagonalnoDvaZaredom st (+ stapic (1+ n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                                       ((and (= kugliceZaredom 2) (equalp (nth nivo (nth stapic st)) '-)) 
                                        (pobednikUnutrasnjostDijagonalnoDvaZaredom st (+ stapic (1+ n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) 0 igrac (1+ poeni)))
                                       (t 
                                        (pobednikUnutrasnjostDijagonalnoDvaZaredom st (+ stapic (1+ n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) 0 igrac poeni))))))
                    (t (cond
                        ((= dijagonala 0) (cond
                                           ((equalp (nth nivo (nth stapic st)) igrac) 
                                            (pobednikUnutrasnjostDijagonalnoDvaZaredom st (+ stapic (1- n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                                           ((and (= kugliceZaredom 2) (equalp (nth nivo (nth stapic st)) '-)) 
                                            (pobednikUnutrasnjostDijagonalnoDvaZaredom st (+ stapic (1- n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) 0 igrac (1+ poeni)))
                                           (t 
                                            (pobednikUnutrasnjostDijagonalnoDvaZaredom st (+ stapic (1- n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
                        (t (cond
                            ((equalp (nth nivo (nth stapic st)) igrac) 
                             (pobednikUnutrasnjostDijagonalnoDvaZaredom st (+ stapic (1- n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                            ((and (= kugliceZaredom 2) (equalp (nth nivo (nth stapic st)) '-)) 
                             (pobednikUnutrasnjostDijagonalnoDvaZaredom st (+ stapic (1- n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) 0 igrac (1+ poeni)))
                            (t 
                             (pobednikUnutrasnjostDijagonalnoDvaZaredom st (+ stapic (1- n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) 0 igrac poeni))))))))
   ((= kugliceZaredom 2) (pobednikUnutrasnjostDijagonalnoDvaZaredom st stapic nivo presek dijagonala predjeneKuglice kugliceZaredom igrac poeni))
   (t (cond
       ((= presek 0) (cond
                      ((= dijagonala 0) (cond
                                         ((equalp (nth nivo (nth stapic st)) igrac) 
                                          (pobednikUnutrasnjostDijagonalnoDvaZaredom st (+ stapic (1+ n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                                         (t 
                                          (pobednikUnutrasnjostDijagonalnoDvaZaredom st (+ stapic (1+ n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
                      (t (cond
                          ((equalp (nth nivo (nth stapic st)) igrac) 
                           (pobednikUnutrasnjostDijagonalnoDvaZaredom st (+ stapic (1+ n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                          (t 
                           (pobednikUnutrasnjostDijagonalnoDvaZaredom st (+ stapic (1+ n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) 0 igrac poeni))))))
       (t (cond
           ((= dijagonala 0) (cond
                              ((equalp (nth nivo (nth stapic st)) igrac) 
                               (pobednikUnutrasnjostDijagonalnoDvaZaredom st (+ stapic (1- n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                              (t 
                               (pobednikUnutrasnjostDijagonalnoDvaZaredom st (+ stapic (1- n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
           (t (cond
               ((equalp (nth nivo (nth stapic st)) igrac) 
                (pobednikUnutrasnjostDijagonalnoDvaZaredom st (+ stapic (1- n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
               (t 
                (pobednikUnutrasnjostDijagonalnoDvaZaredom st (+ stapic (1- n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) 0 igrac poeni))))))))))

(defun pobednikDve(st igrac)
  (mapcar '+ (pobednikStapiciDvaZaredom st 0 0 0 igrac 0)
       (pobednikPoNivoimaDvaZaredom st 0 0 0 0 0 0 0 igrac 0)
       (pobednikPreseciVrsteIKoloneDijagonalnoDveZaredom st 0 3 0 0 0 0 0 igrac 0)                                               
    (pobednikUnutrasnjostDijagonalnoDvaZaredom st 0 0 0 0 0 0 igrac 0)))

;;----- Tri kuglice -----

(defun pobednikUnutrasnjostDijagonalnoTriZaredom (st stapic nivo presek dijagonala predjeneKuglice kugliceZaredom igrac poeni)
  (cond 
   ((AND (= presek 2) (= dijagonala 2)) (list poeni))
   ((< nivo 0) (pobednikUnutrasnjostDijagonalnoTriZaredom st (expt n 2) 0 presek dijagonala predjeneKuglice kugliceZaredom igrac poeni))
   ((AND (> predjeneKuglice (1- n)) (= kugliceZaredom 0)) (cond
                          ((= presek 0) (cond
                                         ((= dijagonala 0) (pobednikUnutrasnjostDijagonalnoTriZaredom st 0 (1- n) presek 1 0 0 igrac poeni))
                                         (t (pobednikUnutrasnjostDijagonalnoTriZaredom st (1- n) 0 1 0 0 0 igrac poeni))))
                          (t (cond
                              ((= dijagonala 0) (pobednikUnutrasnjostDijagonalnoTriZaredom st (1- n) (1- n) presek 1 0 0 igrac poeni))
                              (t (pobednikUnutrasnjostDijagonalnoTriZaredom st stapic nivo 2 2 0 0 igrac poeni))))))
   ((cond 
                    ((= presek 0) (cond
                                   ((= dijagonala 0) (cond
                                                      ((equalp (nth nivo (nth stapic st)) igrac) 
                                                       (pobednikUnutrasnjostDijagonalnoTriZaredom st (+ stapic (1+ n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                                                      ((and (= kugliceZaredom 3) (equalp (nth nivo (nth stapic st)) '-)) 
                                                       (pobednikUnutrasnjostDijagonalnoTriZaredom st (+ stapic (1+ n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) 0 igrac (1+ poeni)))
                                                      (t 
                                                       (pobednikUnutrasnjostDijagonalnoTriZaredom st (+ stapic (1+ n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
                                   (t (cond
                                       ((equalp (nth nivo (nth stapic st)) igrac)
                                        (pobednikUnutrasnjostDijagonalnoTriZaredom st (+ stapic (1+ n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                                       ((and (= kugliceZaredom 3) (equalp (nth nivo (nth stapic st)) '-)) 
                                        (pobednikUnutrasnjostDijagonalnoTriZaredom st (+ stapic (1+ n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) 0 igrac (1+ poeni)))
                                       (t 
                                        (pobednikUnutrasnjostDijagonalnoTriZaredom st (+ stapic (1+ n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) 0 igrac poeni))))))
                    (t (cond
                        ((= dijagonala 0) (cond
                                           ((equalp (nth nivo (nth stapic st)) igrac) 
                                            (pobednikUnutrasnjostDijagonalnoTriZaredom st (+ stapic (1- n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                                           ((and (= kugliceZaredom 3) (equalp (nth nivo (nth stapic st)) '-)) 
                                            (pobednikUnutrasnjostDijagonalnoTriZaredom st (+ stapic (1- n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) 0 igrac (1+ poeni)))
                                           (t 
                                            (pobednikUnutrasnjostDijagonalnoTriZaredom st (+ stapic (1- n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
                        (t (cond
                            ((equalp (nth nivo (nth stapic st)) igrac) 
                             (pobednikUnutrasnjostDijagonalnoTriZaredom st (+ stapic (1- n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                            ((and (= kugliceZaredom 3) (equalp (nth nivo (nth stapic st)) '-)) 
                             (pobednikUnutrasnjostDijagonalnoTriZaredom st (+ stapic (1- n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) 0 igrac (1+ poeni)))
                            (t 
                             (pobednikUnutrasnjostDijagonalnoTriZaredom st (+ stapic (1- n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) 0 igrac poeni))))))))
   ((= kugliceZaredom 3) (pobednikUnutrasnjostDijagonalnoTriZaredom st stapic nivo presek dijagonala predjeneKuglice kugliceZaredom igrac poeni))
   (t (cond
       ((= presek 0) (cond
                      ((= dijagonala 0) (cond
                                         ((equalp (nth nivo (nth stapic st)) igrac) 
                                          (pobednikUnutrasnjostDijagonalnoTriZaredom st (+ stapic (1+ n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                                         (t 
                                          (pobednikUnutrasnjostDijagonalnoTriZaredom st (+ stapic (1+ n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
                      (t (cond
                          ((equalp (nth nivo (nth stapic st)) igrac) 
                           (pobednikUnutrasnjostDijagonalnoTriZaredom st (+ stapic (1+ n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                          (t 
                           (pobednikUnutrasnjostDijagonalnoTriZaredom st (+ stapic (1+ n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) 0 igrac poeni))))))
       (t (cond
           ((= dijagonala 0) (cond
                              ((equalp (nth nivo (nth stapic st)) igrac) 
                               (pobednikUnutrasnjostDijagonalnoTriZaredom st (+ stapic (1- n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                              (t 
                               (pobednikUnutrasnjostDijagonalnoTriZaredom st (+ stapic (1- n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
           (t (cond
               ((equalp (nth nivo (nth stapic st)) igrac) 
                (pobednikUnutrasnjostDijagonalnoTriZaredom st (+ stapic (1- n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
               (t 
                (pobednikUnutrasnjostDijagonalnoTriZaredom st (+ stapic (1- n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) 0 igrac poeni))))))))))

(defun pobednikPreseciVrsteIKoloneDijagonalnoTriZaredom (st stapic nivo vrsta kolona dijagonala predjeneKuglice kugliceZaredom igrac poeni)
  (cond 
   ((AND (= kugliceZaredom 0) (> vrsta (1- n)) (> kolona (1- n))) (list poeni))
   ((< nivo 0) (pobednikPreseciVrsteIKoloneDijagonalnoTriZaredom st stapic 0 vrsta kolona dijagonala predjeneKuglice kugliceZaredom igrac poeni))
   ((< stapic 0) (pobednikPreseciVrsteIKoloneDijagonalnoTriZaredom st 0 nivo vrsta kolona dijagonala predjeneKuglice 0 igrac poeni))
   ((OR (> predjeneKuglice n) (AND (= kolona -1) (= vrsta n))) (cond
                                                                ((= dijagonala 0) (cond 
                                                                                   ((< vrsta n) (pobednikPreseciVrsteIKoloneDijagonalnoTriZaredom st vrsta 0 vrsta 0 1 0 0 igrac poeni))
                                                                                   (t (pobednikPreseciVrsteIKoloneDijagonalnoTriZaredom st (* kolona n) 0 vrsta  kolona 1 0 0 igrac poeni))))
                                                                (t (cond
                                                                    ((= vrsta (1- n)) (pobednikPreseciVrsteIKoloneDijagonalnoTriZaredom st (* kolona n) (1- n) (1+ vrsta) kolona 0 0 0 igrac poeni))
                                                                    ((< vrsta n) (pobednikPreseciVrsteIKoloneDijagonalnoTriZaredom st (1+ vrsta) (1- n) (1+ vrsta) kolona 0 0 0 igrac poeni))
                                                                    ((< kolona n) (pobednikPreseciVrsteIKoloneDijagonalnoTriZaredom st (* (1+ kolona) n) (1- n) vrsta (1+ kolona) 0 0 0 igrac poeni))
                                                                    (t (pobednikPreseciVrsteIKoloneDijagonalnoTriZaredom st stapic nivo vrsta kolona dijagonala predjeneKuglice kugliceZaredom igrac poeni))))))
   ((cond 
     ((= dijagonala 0) (cond
                        ((< vrsta n) (cond 
                                      ((AND (< predjeneKuglice n) (equalp (nth nivo (nth stapic st)) igrac)) (pobednikPreseciVrsteIKoloneDijagonalnoTriZaredom st (+ stapic n) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                                      ((and (= kugliceZaredom 3) (equalp (nth nivo (nth stapic st)) '-)) 
                                       (pobednikPreseciVrsteIKoloneDijagonalnoTriZaredom st (+ stapic n) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac (1+ poeni)))
                                      (t (pobednikPreseciVrsteIKoloneDijagonalnoTriZaredom st (+ stapic n) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
                        (t (cond
                            ((AND (< predjeneKuglice n) (equalp (nth nivo (nth stapic st)) igrac)) (pobednikPreseciVrsteIKoloneDijagonalnoTriZaredom st (1+ stapic) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                            (t (pobednikPreseciVrsteIKoloneDijagonalnoTriZaredom st (1+ stapic) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))))
     ((= dijagonala 1) (cond
                        ((< vrsta n) (cond 
                                      ((AND (< predjeneKuglice n) (equalp (nth nivo (nth stapic st)) igrac)) (pobednikPreseciVrsteIKoloneDijagonalnoTriZaredom st (+ stapic n) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                                      ((and (= kugliceZaredom 3) (equalp (nth nivo (nth stapic st)) '-)) 
                                       (pobednikPreseciVrsteIKoloneDijagonalnoTriZaredom st (+ stapic n) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac (1+ poeni)))
                                      (t (pobednikPreseciVrsteIKoloneDijagonalnoTriZaredom st (+ stapic n) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
                        (t (cond
                            ((AND (< predjeneKuglice n) (equalp (nth nivo (nth stapic st)) igrac)) (pobednikPreseciVrsteIKoloneDijagonalnoTriZaredom st (1+ stapic) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))
                            (t (pobednikPreseciVrsteIKoloneDijagonalnoTriZaredom st (1+ stapic) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))))))
   ((= kugliceZaredom 3) (pobednikPreseciVrsteIKoloneDijagonalnoTriZaredom st stapic nivo vrsta kolona dijagonala predjeneKuglice kugliceZaredom igrac poeni))
   (t (cond
       ((= dijagonala 0) (cond
                          ((< vrsta n) (cond 
                                        ((equalp (nth nivo (nth stapic st)) igrac) (pobednikPreseciVrsteIKoloneDijagonalnoTriZaredom st (+ stapic n) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                                        (t (pobednikPreseciVrsteIKoloneDijagonalnoTriZaredom st (+ stapic n) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
                          (t (cond
                              ((equalp (nth nivo (nth stapic st)) igrac) (pobednikPreseciVrsteIKoloneDijagonalnoTriZaredom st (1+ stapic) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                              (t (pobednikPreseciVrsteIKoloneDijagonalnoTriZaredom st (1+ stapic) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))))
       ((= dijagonala 1) (cond
                          ((< vrsta n) (cond 
                                        ((equalp (nth nivo (nth stapic st)) igrac) (pobednikPreseciVrsteIKoloneDijagonalnoTriZaredom st (+ stapic n) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                                        (t (pobednikPreseciVrsteIKoloneDijagonalnoTriZaredom st (+ stapic n) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
                          (t (cond
                              ((equalp (nth nivo (nth stapic st)) igrac) (pobednikPreseciVrsteIKoloneDijagonalnoTriZaredom st (1+ stapic) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                              (t (pobednikPreseciVrsteIKoloneDijagonalnoTriZaredom st (1+ stapic) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))))))))

(defun pobednikPoNivoimaTriZaredom (st nivo stapic vrsta kolona dijagonala predjeneKuglice kugliceZaredom igrac poeni)
  (cond
   ((= nivo (1+ n)) (list poeni))
   ((> predjeneKuglice n) (cond 
                           ((< vrsta n) (pobednikPoNivoimaTriZaredom st nivo (1+ vrsta) (1+ vrsta) -1 0 0 0 igrac poeni))
                           ((< kolona n) (pobednikPoNivoimaTriZaredom st nivo (* n (1+ kolona)) vrsta (1+ kolona) 0 0 0 igrac poeni))
                           ((= dijagonala 0) (pobednikPoNivoimaTriZaredom st nivo 0 vrsta kolona (1+ dijagonala) 0 0 igrac poeni))
                           ((= dijagonala 1) (pobednikPoNivoimaTriZaredom st nivo (1- n) vrsta kolona (1+ dijagonala) 0 0 igrac poeni))
                           (t (pobednikPoNivoimaTriZaredom st (1+ nivo) 0 0 0 0 0 0 igrac poeni))))
   ((cond
     ((< vrsta n) (cond
                   ((equalp (nth nivo (nth stapic st)) igrac) 
                    (pobednikPoNivoimaTriZaredom st nivo (+ stapic n) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom)  igrac poeni))
                   ((and (= kugliceZaredom 3) (equalp (nth nivo (nth stapic st)) '-)) 
                    (pobednikPoNivoimaTriZaredom st nivo (+ stapic n) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac (1+ poeni)))
                   (t 
                    (pobednikPoNivoimaTriZaredom st nivo (+ stapic n) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
     ((< kolona n) (cond
                    ((equalp (nth nivo (nth stapic st)) igrac) 
                     (pobednikPoNivoimaTriZaredom st nivo (1+ stapic) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                    ((and (= kugliceZaredom 3) (equalp (nth nivo (nth stapic st)) '-)) 
                     (pobednikPoNivoimaTriZaredom st nivo (1+ stapic) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac (1+ poeni)))
                    (t 
                     (pobednikPoNivoimaTriZaredom st nivo (1+ stapic) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
     ((= dijagonala 1) (cond 
                        ((equalp (nth nivo (nth stapic st)) igrac) 
                         (pobednikPoNivoimaTriZaredom st nivo (+ stapic (1+ n)) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                        ((and (= kugliceZaredom 3) (equalp (nth nivo (nth stapic st)) '-)) 
                         (pobednikPoNivoimaTriZaredom st nivo (+ stapic (1+ n)) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac (1+ poeni)))
                        (t 
                         (pobednikPoNivoimaTriZaredom st nivo (+ stapic (1+ n)) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
     (t (cond
         ((equalp (nth nivo (nth stapic st)) igrac) 
          (pobednikPoNivoimaTriZaredom st nivo (+ stapic (1- n)) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
         ((and (= kugliceZaredom 3) (equalp (nth nivo (nth stapic st)) '-)) 
          (pobednikPoNivoimaTriZaredom st nivo (+ stapic (1- n)) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac (1+ poeni)))
         (t 
          (pobednikPoNivoimaTriZaredom st nivo (+ stapic (1- n)) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))))
   
   ((= kugliceZaredom 3) (pobednikPoNivoimaTriZaredom st nivo stapic vrsta kolona dijagonala predjeneKuglice kugliceZaredom igrac poeni))
   (t (cond
       ((< vrsta n) (cond
                     ((equalp (nth nivo (nth stapic st)) igrac) 
                      (pobednikPoNivoimaTriZaredom st nivo (+ stapic n) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                     (t 
                      (pobednikPoNivoimaTriZaredom st nivo (+ stapic n) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
       ((< kolona n) (cond
                      ((equalp (nth nivo (nth stapic st)) igrac) 
                       (pobednikPoNivoimaTriZaredom st nivo (1+ stapic) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                      (t 
                       (pobednikPoNivoimaTriZaredom st nivo (1+ stapic) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
       ((= dijagonala 1) (cond
                          ((equalp (nth nivo (nth stapic st)) igrac) 
                           (pobednikPoNivoimaTriZaredom st nivo (+ stapic (1+ n)) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
                          (t 
                           (pobednikPoNivoimaTriZaredom st nivo (+ stapic (1+ n)) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))
       (t (cond
           ((equalp (nth nivo (nth stapic st)) igrac) 
            (pobednikPoNivoimaTriZaredom st nivo (+ stapic (1- n)) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
           (t 
            (pobednikPoNivoimaTriZaredom st nivo (+ stapic (1- n)) vrsta kolona dijagonala (1+ predjeneKuglice) 0 igrac poeni))))))))

(defun pobednikStapiciTriZaredom (st stapic predjeneKuglice kugliceZaredom igrac poeni)
  (cond 
   ((= stapic (expt n 2)) (list poeni))
   ((= predjeneKuglice (1+ n)) (pobednikStapiciTriZaredom st (1+ stapic) 0 0 igrac poeni))
   ((cond 
     ((equalp (nth predjeneKuglice (nth stapic st)) igrac) (pobednikStapiciTriZaredom st stapic (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
     ((and (= kugliceZaredom 3) (equalp (nth predjeneKuglice (nth stapic st)) '-)) (pobednikStapiciTriZaredom st stapic (1+ predjeneKuglice) 0 igrac (1+ poeni)))
     (t (pobednikStapiciTriZaredom st stapic (1+ predjeneKuglice) 0 igrac poeni))))
   ((= kugliceZaredom 3) (pobednikStapiciTriZaredom st stapic predjeneKuglice kugliceZaredom igrac poeni))
   (t (cond
       ((equalp (nth predjeneKuglice (nth stapic st)) igrac) (pobednikStapiciTriZaredom st stapic (1+ predjeneKuglice) (1+ kugliceZaredom) igrac poeni))
       (t (pobednikStapiciTriZaredom st stapic (1+ predjeneKuglice) 0 igrac poeni))))))

(defun pobednikTri(st igrac)
  (mapcar '+ (pobednikStapiciTriZaredom st 0 0 0 igrac 0)
    (pobednikPoNivoimaTriZaredom st 0 0 0 0 0 0 0 igrac 0)
    (pobednikPreseciVrsteIKoloneDijagonalnoTriZaredom st 0 3 0 0 0 0 0 igrac 0)                                             
    (pobednikUnutrasnjostDijagonalnoTriZaredom st 0 0 0 0 0 0 igrac 0)))

;;----- Cetiri kuglice -----

(defun pobednikStapiciCetiriZaredom (st stapic predjeneKuglice kugliceZaredom dosloDo4 igrac poeni)
  (cond 
   ((= stapic (expt n 2)) (list poeni))
   ((= predjeneKuglice (1+ n)) (pobednikStapiciCetiriZaredom st (1+ stapic) 0 0 0 igrac poeni))
   ((> dosloDo4 0) (cond 
                    ((equalp (nth predjeneKuglice (nth stapic st)) igrac) (pobednikStapiciCetiriZaredom st stapic (1+ predjeneKuglice) (1+ kugliceZaredom) (1+ dosloDo4) igrac poeni ))
                    (t (pobednikStapiciCetiriZaredom st stapic (1+ predjeneKuglice) 0 0 igrac (+ poeni dosloDo4)))))
   ((= kugliceZaredom 4) (pobednikStapiciCetiriZaredom st stapic predjeneKuglice kugliceZaredom 1 igrac poeni))
   (t (cond
       ((equalp (nth predjeneKuglice (nth stapic st)) igrac) (pobednikStapiciCetiriZaredom st stapic (1+ predjeneKuglice) (1+ kugliceZaredom) 0 igrac poeni))
       (t (pobednikStapiciCetiriZaredom st stapic (1+ predjeneKuglice) 0 0 igrac poeni))))))


(defun pobednikPoNivoimaCetiriZaredom (st nivo stapic vrsta kolona dijagonala predjeneKuglice kugliceZaredom dosloDo4 igrac poeni)
  (cond
   ((= nivo (1+ n)) (list poeni))
   ((> predjeneKuglice n) (cond 
                           ((< vrsta n) (pobednikPoNivoimaCetiriZaredom st nivo (1+ vrsta) (1+ vrsta) -1 0 0 0 0 igrac poeni))
                           ((< kolona n) (pobednikPoNivoimaCetiriZaredom st nivo (* n (1+ kolona)) vrsta (1+ kolona) 0 0 0 0 igrac poeni))
                           ((= dijagonala 0) (pobednikPoNivoimaCetiriZaredom st nivo 0 vrsta kolona (1+ dijagonala) 0 0 0 igrac poeni))
                           ((= dijagonala 1) (pobednikPoNivoimaCetiriZaredom st nivo (1- n) vrsta kolona (1+ dijagonala) 0 0 0 igrac poeni))
                           (t (pobednikPoNivoimaCetiriZaredom st (1+ nivo) 0 0 0 0 0 0 0 igrac poeni))))
   ((> dosloDo4 0) (cond
                    ((< vrsta n) (cond
                                  ((equalp (nth nivo (nth stapic st)) igrac) 
                                   (pobednikPoNivoimaCetiriZaredom st nivo (+ stapic n) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) (1+ dosloDo4) igrac poeni))
                                  (t 
                                   (pobednikPoNivoimaCetiriZaredom st nivo (+ stapic n) vrsta kolona dijagonala (1+ predjeneKuglice) 0 0 igrac (+ poeni dosloDo4)))))
                    ((< kolona n) (cond
                                   ((equalp (nth nivo (nth stapic st)) igrac) 
                                    (pobednikPoNivoimaCetiriZaredom st nivo (1+ stapic) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) (1+ dosloDo4) igrac poeni))
                                   (t 
                                    (pobednikPoNivoimaCetiriZaredom st nivo (1+ stapic) vrsta kolona dijagonala (1+ predjeneKuglice) 0 0 igrac (+ poeni dosloDo4)))))
                    ((= dijagonala 1) (cond 
                                       ((equalp (nth nivo (nth stapic st)) igrac) 
                                        (pobednikPoNivoimaCetiriZaredom st nivo (+ stapic (1+ n)) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) (1+ dosloDo4) igrac poeni))
                                       (t 
                                        (pobednikPoNivoimaCetiriZaredom st nivo (+ stapic (1+ n)) vrsta kolona dijagonala (1+ predjeneKuglice) 0 0 igrac (+ poeni dosloDo4)))))
                    (t (cond
                        ((equalp (nth nivo (nth stapic st)) igrac) 
                         (pobednikPoNivoimaCetiriZaredom st nivo (+ stapic (1- n)) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) (1+ dosloDo4) igrac poeni))
                        (t 
                         (pobednikPoNivoimaCetiriZaredom st nivo (+ stapic (1- n)) vrsta kolona dijagonala (1+ predjeneKuglice) 0 0 igrac (+ poeni dosloDo4)))))))
   
   ((= kugliceZaredom 4) (pobednikPoNivoimaCetiriZaredom st nivo stapic vrsta kolona dijagonala predjeneKuglice kugliceZaredom 1 igrac poeni))
   (t (cond
       ((< vrsta n) (cond
                     ((equalp (nth nivo (nth stapic st)) igrac) 
                      (pobednikPoNivoimaCetiriZaredom st nivo (+ stapic n) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) 0 igrac poeni))
                     (t 
                      (pobednikPoNivoimaCetiriZaredom st nivo (+ stapic n) vrsta kolona dijagonala (1+ predjeneKuglice) 0 0 igrac poeni))))
       ((< kolona n) (cond
                      ((equalp (nth nivo (nth stapic st)) igrac) 
                       (pobednikPoNivoimaCetiriZaredom st nivo (1+ stapic) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) 0 igrac poeni))
                      (t 
                       (pobednikPoNivoimaCetiriZaredom st nivo (1+ stapic) vrsta kolona dijagonala (1+ predjeneKuglice) 0 0 igrac poeni))))
       ((= dijagonala 1) (cond
                          ((equalp (nth nivo (nth stapic st)) igrac) 
                           (pobednikPoNivoimaCetiriZaredom st nivo (+ stapic (1+ n)) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) 0 igrac poeni))
                          (t 
                           (pobednikPoNivoimaCetiriZaredom st nivo (+ stapic (1+ n)) vrsta kolona dijagonala (1+ predjeneKuglice) 0 0 igrac poeni))))
       (t (cond
           ((equalp (nth nivo (nth stapic st)) igrac) 
            (pobednikPoNivoimaCetiriZaredom st nivo (+ stapic (1- n)) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) 0 igrac poeni))
           (t 
            (pobednikPoNivoimaCetiriZaredom st nivo (+ stapic (1- n)) vrsta kolona dijagonala (1+ predjeneKuglice) 0 0 igrac poeni))))))))


(defun pobednikPreseciVrsteIKoloneDijagonalnoCetiriZaredom (st stapic nivo vrsta kolona dijagonala predjeneKuglice kugliceZaredom dosloDo4 igrac poeni)
  (cond 
   ((AND (= kugliceZaredom 0) (> vrsta (1- n)) (> kolona (1- n))) (list poeni))
   ((< nivo 0) (pobednikPreseciVrsteIKoloneDijagonalnoCetiriZaredom st stapic 0 vrsta kolona dijagonala predjeneKuglice kugliceZaredom dosloDo4 igrac poeni))
   ((< stapic 0) (pobednikPreseciVrsteIKoloneDijagonalnoCetiriZaredom st 0 nivo vrsta kolona dijagonala predjeneKuglice 0 0 igrac poeni))
   ((OR (> predjeneKuglice n) (AND (= kolona -1) (= vrsta n))) (cond
                                                                ((= dijagonala 0) (cond 
                                                                                   ((< vrsta n) (pobednikPreseciVrsteIKoloneDijagonalnoCetiriZaredom st vrsta 0 vrsta 0 1 0 0 0 igrac poeni))
                                                                                   (t (pobednikPreseciVrsteIKoloneDijagonalnoCetiriZaredom st (* kolona n) 0 vrsta  kolona 1 0 0 0 igrac poeni))))
                                                                (t (cond
                                                                    ((= vrsta (1- n)) (pobednikPreseciVrsteIKoloneDijagonalnoCetiriZaredom st (* kolona n) (1- n) (1+ vrsta) kolona 0 0 0 0 igrac poeni))
                                                                    ((< vrsta n) (pobednikPreseciVrsteIKoloneDijagonalnoCetiriZaredom st (1+ vrsta) (1- n) (1+ vrsta) kolona 0 0 0 0 igrac poeni))
                                                                    ((< kolona n) (pobednikPreseciVrsteIKoloneDijagonalnoCetiriZaredom st (* (1+ kolona) n) (1- n) vrsta (1+ kolona) 0 0 0 0 igrac poeni))
                                                                    (t (pobednikPreseciVrsteIKoloneDijagonalnoCetiriZaredom st stapic nivo vrsta kolona dijagonala predjeneKuglice kugliceZaredom dosloDo4 igrac poeni))))))
   ((> dosloDo4 0) (cond 
                    ((= dijagonala 0) (cond
                                       ((< vrsta n) (cond 
                                                     ((AND (< predjeneKuglice n) (equalp (nth nivo (nth stapic st)) igrac)) (pobednikPreseciVrsteIKoloneDijagonalnoCetiriZaredom st (+ stapic n) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) (1+ dosloDo4) igrac poeni))
                                                              (t (pobednikPreseciVrsteIKoloneDijagonalnoCetiriZaredom st (+ stapic n) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 0 igrac (+ poeni dosloDo4)))))
                                       (t (cond
                                           ((AND (< predjeneKuglice n) (equalp (nth nivo (nth stapic st)) igrac)) (pobednikPreseciVrsteIKoloneDijagonalnoCetiriZaredom st (1+ stapic) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) (1+ dosloDo4) igrac poeni))
                                                              (t (pobednikPreseciVrsteIKoloneDijagonalnoCetiriZaredom st (1+ stapic) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 0 igrac (+ poeni dosloDo4)))))))
                    ((= dijagonala 1) (cond
                                       ((< vrsta n) (cond 
                                                              ((AND (< predjeneKuglice n) (equalp (nth nivo (nth stapic st)) igrac)) (pobednikPreseciVrsteIKoloneDijagonalnoCetiriZaredom st (+ stapic n) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) (1+ dosloDo4) igrac poeni))
                                                              (t (pobednikPreseciVrsteIKoloneDijagonalnoCetiriZaredom st (+ stapic n) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 0 igrac (+ poeni dosloDo4)))))
                                       (t (cond
                                           ((AND (< predjeneKuglice n) (equalp (nth nivo (nth stapic st)) igrac)) (pobednikPreseciVrsteIKoloneDijagonalnoCetiriZaredom st (1+ stapic) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 0 igrac poeni))
                                                              (t (pobednikPreseciVrsteIKoloneDijagonalnoCetiriZaredom st (1+ stapic) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 0 igrac (+ poeni dosloDo4)))))))))
   ((= kugliceZaredom 4) (pobednikPreseciVrsteIKoloneDijagonalnoCetiriZaredom st stapic nivo vrsta kolona dijagonala predjeneKuglice kugliceZaredom 1 igrac poeni))
   (t (cond
       ((= dijagonala 0) (cond
                          ((< vrsta n) (cond 
                                                 ((equalp (nth nivo (nth stapic st)) igrac) (pobednikPreseciVrsteIKoloneDijagonalnoCetiriZaredom st (+ stapic n) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) 0 igrac poeni))
                                                 (t (pobednikPreseciVrsteIKoloneDijagonalnoCetiriZaredom st (+ stapic n) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 0 igrac poeni))))
                          (t (cond
                              ((equalp (nth nivo (nth stapic st)) igrac) (pobednikPreseciVrsteIKoloneDijagonalnoCetiriZaredom st (1+ stapic) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) 0 igrac poeni))
                              (t (pobednikPreseciVrsteIKoloneDijagonalnoCetiriZaredom st (1+ stapic) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 0 igrac poeni))))))
       ((= dijagonala 1) (cond
                          ((< vrsta n) (cond 
                                                 ((equalp (nth nivo (nth stapic st)) igrac) (pobednikPreseciVrsteIKoloneDijagonalnoCetiriZaredom st (+ stapic n) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) 0 igrac poeni))
                                                 (t (pobednikPreseciVrsteIKoloneDijagonalnoCetiriZaredom st (+ stapic n) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 0 igrac poeni))))
                          (t (cond
                              ((equalp (nth nivo (nth stapic st)) igrac) (pobednikPreseciVrsteIKoloneDijagonalnoCetiriZaredom st (1+ stapic) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) 0 igrac poeni))
                              (t (pobednikPreseciVrsteIKoloneDijagonalnoCetiriZaredom st (1+ stapic) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 0 igrac poeni))))))))))


(defun pobednikUnutrasnjostDijagonalnoCetiriZaredom (st stapic nivo presek dijagonala predjeneKuglice kugliceZaredom dosloDo4 igrac poeni)
  (cond 
   ((AND (= presek 2) (= dijagonala 2)) (list poeni))
   ((< nivo 0) (pobednikUnutrasnjostDijagonalnoCetiriZaredom st (expt n 2) 0 presek dijagonala predjeneKuglice kugliceZaredom dosloDo4 igrac poeni))
   ((AND (> predjeneKuglice (1- n)) (= kugliceZaredom 0)) (cond
                          ((= presek 0) (cond
                                         ((= dijagonala 0) (pobednikUnutrasnjostDijagonalnoCetiriZaredom st 0 (1- n) presek 1 0 0 0 igrac poeni))
                                         (t (pobednikUnutrasnjostDijagonalnoCetiriZaredom st(1- n) 0 1 0 0 0 0 igrac poeni))))
                          (t (cond
                              ((= dijagonala 0) (pobednikUnutrasnjostDijagonalnoCetiriZaredom st (1- n) (1- n) presek 1 0 0 0 igrac poeni))
                              (t (pobednikUnutrasnjostDijagonalnoCetiriZaredom st stapic nivo 2 2 0 0 0 igrac poeni))))))
   ((> dosloDo4 0) (cond 
                    ((= presek 0) (cond
                                   ((= dijagonala 0) (cond
                                                      ((equalp (nth nivo (nth stapic st)) igrac) 
                                                       (pobednikUnutrasnjostDijagonalnoCetiriZaredom st (+ stapic (1+ n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) (1+ dosloDo4) igrac poeni))
                                                      (t 
                                                       (pobednikUnutrasnjostDijagonalnoCetiriZaredom st (+ stapic (1+ n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) 0 0 igrac (+ poeni dosloDo4)))))
                                   (t (cond
                                       ((equalp (nth nivo (nth stapic st)) igrac)
                                        (pobednikUnutrasnjostDijagonalnoCetiriZaredom st (+ stapic (1+ n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) (1+ dosloDo4) igrac poeni))
                                       (t 
                                        (pobednikUnutrasnjostDijagonalnoCetiriZaredom st (+ stapic (1+ n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) 0 0 igrac (+ poeni dosloDo4)))))))
                    (t (cond
                        ((= dijagonala 0) (cond
                                           ((equalp (nth nivo (nth stapic st)) igrac) 
                                            (pobednikUnutrasnjostDijagonalnoCetiriZaredom st (+ stapic (1- n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) (1+ dosloDo4) igrac poeni))
                                           (t 
                                            (pobednikUnutrasnjostDijagonalnoCetiriZaredom st (+ stapic (1- n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) 0 0 igrac (+ poeni dosloDo4)))))
                        (t (cond
                            ((equalp (nth nivo (nth stapic st)) igrac) 
                             (pobednikUnutrasnjostDijagonalnoCetiriZaredom st (+ stapic (1- n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) (1+ dosloDo4) igrac poeni))
                            (t 
                             (pobednikUnutrasnjostDijagonalnoCetiriZaredom st (+ stapic (1- n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) 0 0 igrac (+ poeni dosloDo4)))))))))
   ((= kugliceZaredom 4) (pobednikUnutrasnjostDijagonalnoCetiriZaredom st stapic nivo presek dijagonala predjeneKuglice kugliceZaredom 1 igrac poeni))
   (t (cond
       ((= presek 0) (cond
                      ((= dijagonala 0) (cond
                                         ((equalp (nth nivo (nth stapic st)) igrac) 
                                          (pobednikUnutrasnjostDijagonalnoCetiriZaredom st (+ stapic (1+ n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) 0 igrac poeni))
                                         (t 
                                          (pobednikUnutrasnjostDijagonalnoCetiriZaredom st (+ stapic (1+ n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) 0 0 igrac poeni))))
                      (t (cond
                          ((equalp (nth nivo (nth stapic st)) igrac) 
                           (pobednikUnutrasnjostDijagonalnoCetiriZaredom st (+ stapic (1+ n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) 0 igrac poeni))
                          (t 
                           (pobednikUnutrasnjostDijagonalnoCetiriZaredom st (+ stapic (1+ n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) 0 0 igrac poeni))))))
       (t (cond
           ((= dijagonala 0) (cond
                              ((equalp (nth nivo (nth stapic st)) igrac) 
                               (pobednikUnutrasnjostDijagonalnoCetiriZaredom st (+ stapic (1- n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) 0 igrac poeni))
                              (t 
                               (pobednikUnutrasnjostDijagonalnoCetiriZaredom st (+ stapic (1- n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) 0 0 igrac poeni))))
           (t (cond
               ((equalp (nth nivo (nth stapic st)) igrac) 
                (pobednikUnutrasnjostDijagonalnoCetiriZaredom st (+ stapic (1- n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) 0 igrac poeni))
               (t 
                (pobednikUnutrasnjostDijagonalnoCetiriZaredom st (+ stapic (1- n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) 0 0 igrac poeni))))))))))

(defun pobednikCetiri(st igrac)
  (mapcar '+ (pobednikStapiciCetiriZaredom st 0 0 0 0 igrac 0) 
       (pobednikPoNivoimaCetiriZaredom st 0 0 0 0 0 0 0 0 igrac 0)
       (pobednikPreseciVrsteIKoloneDijagonalnoCetiriZaredom st 0 (1- n) 0 0 0 0 0 0 igrac 0)                                                  
       (pobednikUnutrasnjostDijagonalnoCetiriZaredom st 0 0 0 0 0 0 0 igrac 0)))



;;------------------------------------------------------------------------------------------------
;; 6. Funkcije za odredjivanje pobednika
;;------------------------------------------------------------------------------------------------


(defun krajIgre(st) 
  (cond
   ((null st) (progn (format t "~%Kraj igre~%") (pobednik stanje) '1))
   ((nemaCrtice (car st)) (krajIgre (cdr st)))
   (t '())))

(defun pobednik(pomStanje)
  (format t "~%Rezultat igre je: (X) ~a : ~a (O) poena" 
    (+ (pobednikStapici pomStanje 0 0 0 0 'x 0) 
       (pobednikPoNivoima pomStanje 0 0 0 0 0 0 0 0 'x 0)
       (pobednikPreseciVrsteIKoloneDijagonalno pomStanje 0 (1- n) 0 0 0 0 0 0 'x 0)                                                  
       (pobednikUnutrasnjostDijagonalno pomStanje 0 0 0 0 0 0 0 'x 0))
    (+ (pobednikStapici pomStanje 0 0 0 0 'o 0) 
       (pobednikPoNivoima pomStanje 0 0 0 0 0 0 0 0 'o 0)
       (pobednikPreseciVrsteIKoloneDijagonalno pomStanje 0 (1- n) 0 0 0 0 0 0 'o 0)                                                  
       (pobednikUnutrasnjostDijagonalno pomStanje 0 0 0 0 0 0 0 'o 0))))

(defun pobednikStapici (pomStanje stapic predjeneKuglice kugliceZaredom dosloDo4 igrac poeni)
  (cond 
   ((= stapic (expt n 2)) (progn (format t "~%Igrac: ~a, Broj poena po stapicima: ~a" igrac poeni) poeni))
   ((= predjeneKuglice (1+ n)) (pobednikStapici pomStanje (1+ stapic) 0 0 0 igrac poeni))
   ((> dosloDo4 0) (cond 
                    ((equalp (nth predjeneKuglice (nth stapic pomStanje)) igrac) (pobednikStapici pomStanje stapic (1+ predjeneKuglice) (1+ kugliceZaredom) (1+ dosloDo4) igrac poeni ))
                    (t (pobednikStapici pomStanje stapic (1+ predjeneKuglice) 0 0 igrac (+ poeni dosloDo4)))))
   ((= kugliceZaredom 4) (pobednikStapici pomStanje stapic predjeneKuglice kugliceZaredom 1 igrac poeni))
   (t (cond
       ((equalp (nth predjeneKuglice (nth stapic pomStanje)) igrac) (pobednikStapici pomStanje stapic (1+ predjeneKuglice) (1+ kugliceZaredom) 0 igrac poeni))
       (t (pobednikStapici pomStanje stapic (1+ predjeneKuglice) 0 0 igrac poeni))))))

(defun pobednikPoNivoima (pomStanje nivo stapic vrsta kolona dijagonala predjeneKuglice kugliceZaredom dosloDo4 igrac poeni)
  (cond
   ((= nivo (1+ n)) (progn (format t "~%Igrac: ~a, Broj poena po nivoima: ~a" igrac poeni) poeni))
   ((> predjeneKuglice n) (cond 
                           ((< vrsta n) (pobednikPoNivoima pomStanje nivo (1+ vrsta) (1+ vrsta) -1 0 0 0 0 igrac poeni))
                           ((< kolona n) (pobednikPoNivoima pomStanje nivo (* n (1+ kolona)) vrsta (1+ kolona) 0 0 0 0 igrac poeni))
                           ((= dijagonala 0) (pobednikPoNivoima pomStanje nivo 0 vrsta kolona (1+ dijagonala) 0 0 0 igrac poeni))
                           ((= dijagonala 1) (pobednikPoNivoima pomStanje  nivo (1- n) vrsta kolona (1+ dijagonala) 0 0 0 igrac poeni))
                           (t (pobednikPoNivoima pomStanje (1+ nivo) 0 0 0 0 0 0 0 igrac poeni))))
   ((> dosloDo4 0) (cond
                    ((< vrsta n) (cond
                                  ((equalp (nth nivo (nth stapic pomStanje)) igrac) 
                                   (pobednikPoNivoima pomStanje nivo (+ stapic n) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) (1+ dosloDo4) igrac poeni))
                                  (t 
                                   (pobednikPoNivoima pomStanje nivo (+ stapic n) vrsta kolona dijagonala (1+ predjeneKuglice) 0 0 igrac (+ poeni dosloDo4)))))
                    ((< kolona n) (cond
                                   ((equalp (nth nivo (nth stapic pomStanje)) igrac) 
                                    (pobednikPoNivoima pomStanje nivo (1+ stapic) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) (1+ dosloDo4) igrac poeni))
                                   (t 
                                    (pobednikPoNivoima pomStanje nivo (1+ stapic) vrsta kolona dijagonala (1+ predjeneKuglice) 0 0 igrac (+ poeni dosloDo4)))))
                    ((= dijagonala 1) (cond 
                                       ((equalp (nth nivo (nth stapic pomStanje)) igrac) 
                                        (pobednikPoNivoima pomStanje nivo (+ stapic (1+ n)) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) (1+ dosloDo4) igrac poeni))
                                       (t 
                                        (pobednikPoNivoima pomStanje nivo (+ stapic (1+ n)) vrsta kolona dijagonala (1+ predjeneKuglice) 0 0 igrac (+ poeni dosloDo4)))))
                    (t (cond
                        ((equalp (nth nivo (nth stapic pomStanje)) igrac) 
                         (pobednikPoNivoima pomStanje nivo (+ stapic (1- n)) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) (1+ dosloDo4) igrac poeni))
                        (t 
                         (pobednikPoNivoima pomStanje nivo (+ stapic (1- n)) vrsta kolona dijagonala (1+ predjeneKuglice) 0 0 igrac (+ poeni dosloDo4)))))))
   
   ((= kugliceZaredom 4) (pobednikPoNivoima pomStanje nivo stapic vrsta kolona dijagonala predjeneKuglice kugliceZaredom 1 igrac poeni))
   (t (cond
       ((< vrsta n) (cond
                     ((equalp (nth nivo (nth stapic pomStanje)) igrac) 
                      (pobednikPoNivoima pomStanje nivo (+ stapic n) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) 0 igrac poeni))
                     (t 
                      (pobednikPoNivoima pomStanje nivo (+ stapic n) vrsta kolona dijagonala (1+ predjeneKuglice) 0 0 igrac poeni))))
       ((< kolona n) (cond
                      ((equalp (nth nivo (nth stapic pomStanje)) igrac) 
                       (pobednikPoNivoima pomStanje nivo (1+ stapic) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) 0 igrac poeni))
                      (t 
                       (pobednikPoNivoima pomStanje nivo (1+ stapic) vrsta kolona dijagonala (1+ predjeneKuglice) 0 0 igrac poeni))))
       ((= dijagonala 1) (cond
                          ((equalp (nth nivo (nth stapic pomStanje)) igrac) 
                           (pobednikPoNivoima pomStanje nivo (+ stapic (1+ n)) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) 0 igrac poeni))
                          (t 
                           (pobednikPoNivoima pomStanje nivo (+ stapic (1+ n)) vrsta kolona dijagonala (1+ predjeneKuglice) 0 0 igrac poeni))))
       (t (cond
           ((equalp (nth nivo (nth stapic pomStanje)) igrac) 
            (pobednikPoNivoima pomStanje nivo (+ stapic (1- n)) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) 0 igrac poeni))
           (t 
            (pobednikPoNivoima pomStanje nivo (+ stapic (1- n)) vrsta kolona dijagonala (1+ predjeneKuglice) 0 0 igrac poeni))))))))


(defun pobednikPreseciVrsteIKoloneDijagonalno (pomStanje stapic nivo vrsta kolona dijagonala predjeneKuglice kugliceZaredom dosloDo4 igrac poeni)
  (cond 
   ((AND (= kugliceZaredom 0) (> vrsta (1- n)) (> kolona (1- n))) (progn (format t "~%Igrac: ~a, Broj poena po presecima: ~a" igrac poeni) poeni))
   ((< nivo 0) (pobednikPreseciVrsteIKoloneDijagonalno pomStanje stapic 0 vrsta kolona dijagonala predjeneKuglice kugliceZaredom dosloDo4 igrac poeni))
   ((< stapic 0) (pobednikPreseciVrsteIKoloneDijagonalno pomStanje 0 nivo vrsta kolona dijagonala predjeneKuglice 0 0 igrac poeni))
   ((OR (> predjeneKuglice n) (AND (= kolona -1) (= vrsta n))) (cond
                                                                ((= dijagonala 0) (cond 
                                                                                   ((< vrsta n) (pobednikPreseciVrsteIKoloneDijagonalno pomStanje vrsta 0 vrsta 0 1 0 0 0 igrac poeni))
                                                                                   (t (pobednikPreseciVrsteIKoloneDijagonalno pomStanje (* kolona n) 0 vrsta  kolona 1 0 0 0 igrac poeni))))
                                                                (t (cond
                                                                    ((= vrsta (1- n)) (pobednikPreseciVrsteIKoloneDijagonalno pomStanje (* kolona n) (1- n) (1+ vrsta) kolona 0 0 0 0 igrac poeni))
                                                                    ((< vrsta n) (pobednikPreseciVrsteIKoloneDijagonalno pomStanje (1+ vrsta) (1- n) (1+ vrsta) kolona 0 0 0 0 igrac poeni))
                                                                    ((< kolona n) (pobednikPreseciVrsteIKoloneDijagonalno pomStanje (* (1+ kolona) n) (1- n) vrsta (1+ kolona) 0 0 0 0 igrac poeni))
                                                                    (t (pobednikPreseciVrsteIKoloneDijagonalno pomStanje stapic nivo vrsta kolona dijagonala predjeneKuglice kugliceZaredom dosloDo4 igrac poeni))))))
   ((> dosloDo4 0) (cond 
                    ((= dijagonala 0) (cond
                                       ((< vrsta n) (cond 
                                                              ((AND (< predjeneKuglice n) (equalp (nth nivo (nth stapic pomStanje)) igrac)) (pobednikPreseciVrsteIKoloneDijagonalno pomStanje (+ stapic n) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) (1+ dosloDo4) igrac poeni))
                                                              (t (pobednikPreseciVrsteIKoloneDijagonalno pomStanje (+ stapic n) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 0 igrac (+ poeni dosloDo4)))))
                                       (t (cond
                                           ((AND (< predjeneKuglice n) (equalp (nth nivo (nth stapic pomStanje)) igrac)) (pobednikPreseciVrsteIKoloneDijagonalno pomStanje (1+ stapic) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) (1+ dosloDo4) igrac poeni))
                                                              (t (pobednikPreseciVrsteIKoloneDijagonalno pomStanje (1+ stapic) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 0 igrac (+ poeni dosloDo4)))))))
                    ((= dijagonala 1) (cond
                                       ((< vrsta n) (cond 
                                                              ((AND (< predjeneKuglice n) (equalp (nth nivo (nth stapic pomStanje)) igrac)) (pobednikPreseciVrsteIKoloneDijagonalno pomStanje (+ stapic n) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) (1+ dosloDo4) igrac poeni))
                                                              (t (pobednikPreseciVrsteIKoloneDijagonalno pomStanje (+ stapic n) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 0 igrac (+ poeni dosloDo4)))))
                                       (t (cond
                                           ((AND (< predjeneKuglice n) (equalp (nth nivo (nth stapic pomStanje)) igrac)) (pobednikPreseciVrsteIKoloneDijagonalno pomStanje (1+ stapic) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 0 igrac poeni))
                                                              (t (pobednikPreseciVrsteIKoloneDijagonalno pomStanje (1+ stapic) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 0 igrac (+ poeni dosloDo4)))))))))
   ((= kugliceZaredom 4) (pobednikPreseciVrsteIKoloneDijagonalno pomStanje stapic nivo vrsta kolona dijagonala predjeneKuglice kugliceZaredom 1 igrac poeni))
   (t (cond
       ((= dijagonala 0) (cond
                          ((< vrsta n) (cond 
                                                 ((equalp (nth nivo (nth stapic pomStanje)) igrac) (pobednikPreseciVrsteIKoloneDijagonalno pomStanje (+ stapic n) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) 0 igrac poeni))
                                                 (t (pobednikPreseciVrsteIKoloneDijagonalno pomStanje (+ stapic n) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 0 igrac poeni))))
                          (t (cond
                              ((equalp (nth nivo (nth stapic pomStanje)) igrac) (pobednikPreseciVrsteIKoloneDijagonalno pomStanje (1+ stapic) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) 0 igrac poeni))
                              (t (pobednikPreseciVrsteIKoloneDijagonalno pomStanje (1+ stapic) (1- nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 0 igrac poeni))))))
       ((= dijagonala 1) (cond
                          ((< vrsta n) (cond 
                                                 ((equalp (nth nivo (nth stapic pomStanje)) igrac) (pobednikPreseciVrsteIKoloneDijagonalno pomStanje (+ stapic n) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) 0 igrac poeni))
                                                 (t (pobednikPreseciVrsteIKoloneDijagonalno pomStanje (+ stapic n) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 0 igrac poeni))))
                          (t (cond
                              ((equalp (nth nivo (nth stapic pomStanje)) igrac) (pobednikPreseciVrsteIKoloneDijagonalno pomStanje (1+ stapic) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) 0 igrac poeni))
                              (t (pobednikPreseciVrsteIKoloneDijagonalno pomStanje (1+ stapic) (1+ nivo) vrsta kolona dijagonala (1+ predjeneKuglice) 0 0 igrac poeni))))))))))

(defun pobednikUnutrasnjostDijagonalno (pomStanje stapic nivo presek dijagonala predjeneKuglice kugliceZaredom dosloDo4 igrac poeni)
  (cond 
   ((AND (= presek 2) (= dijagonala 2)) (progn (format t "~%Igrac: ~a, Broj poena dijagonalno u unutrasnjosti: ~a" igrac poeni) poeni))
   ((< nivo 0) (pobednikUnutrasnjostDijagonalno pomStanje (expt n 2) 0 presek dijagonala predjeneKuglice kugliceZaredom dosloDo4 igrac poeni))
   ((AND (> predjeneKuglice (1- n)) (= kugliceZaredom 0)) (cond
                          ((= presek 0) (cond
                                         ((= dijagonala 0) (pobednikUnutrasnjostDijagonalno pomStanje 0 (1- n) presek 1 0 0 0 igrac poeni))
                                         (t (pobednikUnutrasnjostDijagonalno pomStanje (1- n) 0 1 0 0 0 0 igrac poeni))))
                          (t (cond
                              ((= dijagonala 0) (pobednikUnutrasnjostDijagonalno pomStanje (1- n) (1- n) presek 1 0 0 0 igrac poeni))
                              (t (pobednikUnutrasnjostDijagonalno pomStanje stapic nivo 2 2 0 0 0 igrac poeni))))))
   ((> dosloDo4 0) (cond 
                    ((= presek 0) (cond
                                   ((= dijagonala 0) (cond
                                                      ((equalp (nth nivo (nth stapic pomStanje)) igrac) 
                                                       (pobednikUnutrasnjostDijagonalno pomStanje (+ stapic (1+ n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) (1+ dosloDo4) igrac poeni))
                                                      (t 
                                                       (pobednikUnutrasnjostDijagonalno pomStanje (+ stapic (1+ n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) 0 0 igrac (+ poeni dosloDo4)))))
                                   (t (cond
                                       ((equalp (nth nivo (nth stapic pomStanje)) igrac)
                                        (pobednikUnutrasnjostDijagonalno pomStanje (+ stapic (1+ n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) (1+ dosloDo4) igrac poeni))
                                       (t 
                                        (pobednikUnutrasnjostDijagonalno pomStanje (+ stapic (1+ n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) 0 0 igrac (+ poeni dosloDo4)))))))
                    (t (cond
                        ((= dijagonala 0) (cond
                                           ((equalp (nth nivo (nth stapic pomStanje)) igrac) 
                                            (pobednikUnutrasnjostDijagonalno pomStanje (+ stapic (1- n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) (1+ dosloDo4) igrac poeni))
                                           (t 
                                            (pobednikUnutrasnjostDijagonalno pomStanje (+ stapic (1- n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) 0 0 igrac (+ poeni dosloDo4)))))
                        (t (cond
                            ((equalp (nth nivo (nth stapic pomStanje)) igrac) 
                             (pobednikUnutrasnjostDijagonalno pomStanje (+ stapic (1- n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) (1+ dosloDo4) igrac poeni))
                            (t 
                             (pobednikUnutrasnjostDijagonalno pomStanje (+ stapic (1- n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) 0 0 igrac (+ poeni dosloDo4)))))))))
   ((= kugliceZaredom 4) (pobednikUnutrasnjostDijagonalno pomStanje stapic nivo presek dijagonala predjeneKuglice kugliceZaredom 1 igrac poeni))
   (t (cond
       ((= presek 0) (cond
                      ((= dijagonala 0) (cond
                                         ((equalp (nth nivo (nth stapic pomStanje)) igrac) 
                                          (pobednikUnutrasnjostDijagonalno pomStanje (+ stapic (1+ n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) 0 igrac poeni))
                                         (t 
                                          (pobednikUnutrasnjostDijagonalno pomStanje (+ stapic (1+ n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) 0 0 igrac poeni))))
                      (t (cond
                          ((equalp (nth nivo (nth stapic pomStanje)) igrac) 
                           (pobednikUnutrasnjostDijagonalno pomStanje (+ stapic (1+ n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) 0 igrac poeni))
                          (t 
                           (pobednikUnutrasnjostDijagonalno pomStanje (+ stapic (1+ n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) 0 0 igrac poeni))))))
       (t (cond
           ((= dijagonala 0) (cond
                              ((equalp (nth nivo (nth stapic pomStanje)) igrac) 
                               (pobednikUnutrasnjostDijagonalno pomStanje (+ stapic (1- n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) 0 igrac poeni))
                              (t 
                               (pobednikUnutrasnjostDijagonalno pomStanje (+ stapic (1- n)) (1+ nivo) presek dijagonala (1+ predjeneKuglice) 0 0 igrac poeni))))
           (t (cond
               ((equalp (nth nivo (nth stapic pomStanje)) igrac) 
                (pobednikUnutrasnjostDijagonalno pomStanje (+ stapic (1- n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) (1+ kugliceZaredom) 0 igrac poeni))
               (t 
                (pobednikUnutrasnjostDijagonalno pomStanje (+ stapic (1- n)) (1- nivo) presek dijagonala (1+ predjeneKuglice) 0 0 igrac poeni))))))))))



;;------------------------------------------------------------------------------------------------
;; 7. Funkcije za konacno pokretanje 
;;------------------------------------------------------------------------------------------------

(defun igra()
  (prikaziMatricu)
  (start '1))

;;-----IGRA KAD SE POCNE OD NEKOG PROIZVOLJNOG STANJA-----

;(setq stanje '((x o - -)(x x o o)(o x x o)(o x o o)(x x x -)(o x o o)(o x x o)(o o x -)(x x - -)(x x o o)(o x x -)(o o o o)(x x x -)(x x o o)(o x x o)(o o x -)))

;(progn (velicinaKocke) (igraPrvi) (igra2 stanje))


;-----IGRA OD POCETKA-----

;(setq stanje '())



;;------------------------------------------------------------------------------------------------
;; PRIMERI ZA ODBRANU
;;------------------------------------------------------------------------------------------------

;;IGRA IZMEDJU DVA IGRACA (COVEK I COVEK)

(defun start(igrac)
  (cond
   ((equal (krajIgre stanje) '()) (cond
                                   ((= igrac 1) 
                                    (progn  (format t "~%Na potezu je prvi igrac (x):") (unosPoteza stanje 'x) (if (equalp (krajIgre stanje) '()) (start '0))))
                                   (t (progn  (format t "~%Na potezu je drugi igrac (o):") (unosPoteza stanje 'o) (if (equalp (krajIgre stanje) '()) (start '1))))))
   (t )))

(progn (velicinaKocke) (igra))

;; POBEDNIK 6X6X6

(setq stanje '((x x x x x x)(x x o o o o)(o x x o x x)(o o o o o o)(x x x x o o)(x x o o o x)(o x x o x x)(o o o o x x)(x x x x x x)(x x o o o o)(o x x x x x )(o o o o o o)(x x x x x x)(x x o o x x)(o x x o o o)(o o x x x x)
                 (x x x x x x)(x x o o x x)(o x x o x x)(o o o o x x)(x x x x x x)(x x o o o o)(o x x o o o)(o o x x x x)(x x x x o o)(x x o o x x)(o x x x x o)(o o o o o o)(x x x x o o)(x x o o o o)(o x x o o o)(o o o o o o)
               (o x x o x x)(o o x o o o)(x x x x x x)(x x o o o o)))

(setq n 6)

(prikazProizvoljnogStanja stanje)

(pobednik stanje)

;;POBEDNIK 4X4X4

(setq stanje '((x x x x)(x x o o)(o x x o)(o o o o)(x x x x)(x x o o)(o x x o)(o o x x)(x x x x)(x x o o)(o x o x)(o o o o)(x x x x)(x x o o)(o x x o)(o o o o)))

(setq n 4)

(prikazProizvoljnogStanja stanje)

(pobednik stanje)

;;-----IGRA KAD SE POCNE OD NEKOG PROIZVOLJNOG STANJA  (COVEK I RACUNAR)-----

(defun start(igrac)
  (cond
   ((equal (krajIgre stanje) '()) 
    (cond
     ((= igrac 1) 
      (cond
       ((equalp comp 'x) (progn (format t "~%Na potezu je prvi igrac (x):") (setq stanje (car (a-b-Minimax stanje 'x 5 1 0 0 'x))) (prikazProizvoljnogStanja stanje) (if (equalp (krajIgre stanje) '()) (start '0))))
       (t (progn  (format t "~%Na potezu je prvi igrac (x):") (unosPoteza stanje 'x) (if (equalp (krajIgre stanje) '()) (start '0))))))
     (t (cond 
      ((equal comp 'o) (progn (format t "~%Na potezu je drugi igrac (o):") (setq stanje (car (a-b-Minimax stanje 'o 5 1 0 0 'o))) (prikazProizvoljnogStanja stanje) (if (equalp (krajIgre stanje) '()) (start '1))))
        (t (progn  (format t "~%Na potezu je drugi igrac (o):") (unosPoteza stanje 'o) (if (equalp (krajIgre stanje) '()) (start '1)))))) (t )))))

(setq stanje '((x o - -)(x x o o)(o x x o)(o x o o)(x x x -)(o x o o)(o x x o)(o o x -)(x x - -)(x x o o)(o x x -)(o o o o)(x x x -)(x x o o)(o x x o)(o o x -)))

(progn (velicinaKocke) (igraPrvi) (igra2 stanje))





