
;-----FUNKCIJE IZ PRVE FAZE-----

(setq n '4)

(defun prikazProizvoljnogStanja()
  (progn (odstampajOkvir)(format t "~%") (odstampajMatricu stanje 0 (- (* n 2) 2) 1 0) (odstampajOkvir)))

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

(defun odstampajMatricu(stanje indStapica indKuglice red dodaoN)
  (cond
   ((AND (> red (1- (* n 2))) (> indStapica (1- (expt n 2)))) (format t ""))
   ((AND (not(= 0 indStapica)) (= 0 (mod indStapica n)) (= 0 dodaoN)) (progn (odstampajMatricu stanje indStapica (+ indKuglice n) red 1)))
   ((= indStapica (expt n 2)) (cond 
                                        ((equal (1+ red) n) (progn (format t "~%") (odstampajMatricu stanje 0 (1- n) (1+ red) 0)))
                                        ((> (1+ red) n) (progn (format t "~%") (odstampajMatricu stanje 0 (- (1- (* n 2)) (1+ red)) (1+ red) 0)))
                                        ((< (1+ red) n) (progn (format t "~%") (odstampajMatricu stanje 0 (- (1- (* n 2)) (1+ red)) (1+ red) 0)))))
   (t (progn (odstampajJedno stanje indStapica indKuglice) (odstampajMatricu stanje (1+ indStapica) (1- indKuglice) red 0)))))

(defun odstampajJedno(stanje stapic naStapicu)
  (cond ((AND (not (< naStapicu 0)) (> n naStapicu)) (format t "~a " (nth naStapicu (nth stapic stanje))))
        (t (format t "  "))))

(defun potezValjan(stapic stanje igrac)
   (progn ( > (expt n 2) stapic) (not (punJe stapic stanje))))

(defun punJe(stapic stanje)
  (nemaCrtice (nth stapic stanje)))

(defun nemaCrtice(podLista)
  (cond ((null podLista) T)
        ((equalp '- (car podLista)) '() )
        (t (nemaCrtice (cdr podLista)))))

(defun promeniStanjeJednogStapica(stanjeJednogStapica igrac predjeniDeo)
  (cond 
   ((= (length predjeniDeo) n) predjeniDeo)
   ((equalp '- (car stanjeJednogStapica)) (append predjeniDeo (cons igrac (cdr stanjeJednogStapica))))
   (t (promeniStanjeJednogStapica (cdr stanjeJednogStapica) igrac (append predjeniDeo (list (car stanjeJednogStapica)))))))

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

(defun krajIgre(st) 
  (cond
   ((null st) (progn (format t "~%Kraj igre~%") (pobednik stanje) '1))
   ((nemaCrtice (car st)) (krajIgre (cdr st)))
   (t '())))





;-----FUNKCIJE IZ DRUGE FAZE-----

;-----FUNKCIJE ZA OPERATORE PROMENE STANJA PROBLEMA U OPSTEM SLUCAJU-----

;-----FUNKCIJA KOJA NA OSNOVU TRENUTNE SITUACIJE U KOCKI I ZADATOG POTEZA-----
;-----FORMIRA NOVU SITUACIJU NA KOCKI-----

(defun obrisi (pom ind lista)
  (cond
   ((null lista) '())
   ((= pom ind) (obrisi (1+ pom) ind (cdr lista)))
   (t (cons (car lista) (obrisi (1+ pom) ind (cdr lista))))))

(defun umetni (el n lista)
  (obrisi 0 3 lista)
  (cond
   ((= n 0) (cons el lista))
   (t (cons (car lista) (umetni el (1- n) (cdr lista))))))

(defun promenaStanja(st stapic potez)
  (setq stanje (progn (umetni (progn (promeniStanjeJednogStapica (nth (konvertujPotez stapic) st) potez '())) (konvertujPotez stapic) (obrisi '0  (konvertujPotez stapic) st))))
  (if (not (equalp stanje st)) (progn (format t "~% Potez: ~a" (konvertujBrojUPotez stapic))
                                         (odstampajOkvir)(format t "~%") (odstampajMatricu stanje 0 (- (* n 2) 2) 1 0) (odstampajOkvir))))

(defun unosPoteza(st igrac)
  (let* (
         (stapic (progn
                   (format t "~%Unesite zeljeni stapic >")
                   (read))))
    (cond 
     ((potezValjan (konvertujPotez stapic) st igrac) (promenaStanja st (konvertujPotez stapic) igrac))
     (t (format t "~%Ovaj potez nije moguc!Pokusajte neki drugi!") (unosPoteza st igrac)))))


(setq stanje '((x x - -)(x x o o)(o x x o)(o o o o)(x x x x)(x x o o)(o x x o)(o o - -)(x x x x)(x x o o)(o x - -)(o o o o)(x x x x)(x x o o)(o x x o)(o o - -)))

(prikazProizvoljnogStanja)

(unosPoteza stanje 'x)





;-----FUNKCIJA KOJA NA OSNOVU TRENUTNE SITUACIJE U KOCKI I IGRACA KOJI JE NA POTEZU-----
;-----FORMIRA LISTU SVIH MOGUCIH SITUACIJA U KOCKI-----

(defun promenaVirtuelnogStanja(st stapic potez)
  (setq virtuelnoStanje (progn (umetni (progn (promeniStanjeJednogStapica (nth (konvertujPotez stapic) st) potez '())) (konvertujPotez stapic) (obrisi '0  (konvertujPotez stapic) st))))
  (if (not (equalp virtuelnoStanje st)) (progn (format t "~% Potez: ~a" (konvertujBrojUPotez stapic))
                                         (odstampajOkvir)(format t "~%") (odstampajMatricu virtuelnoStanje 0 (- (* n 2) 2) 1 0) (odstampajOkvir))))

(defun nadjiStanje(stanje stapic potez)
  (setq virtuelnoStanje (progn (umetni (progn (promeniStanjeJednogStapica (nth (konvertujPotez stapic) stanje) potez '())) (konvertujPotez stapic) (obrisi '0  (konvertujPotez stapic) stanje))))
  (if (not (equalp virtuelnoStanje stanje)) virtuelnoStanje))

(defun formirajListuMogucihPoteza(stapic igrac trenutnoStanje daLiJeVracenoStanje moguceStanje lista)
  (cond
   ((= stapic (expt n 2)) lista)
   ((= daLiJeVracenoStanje 1)
    (cond 
     ((equalp moguceStanje '()) (formirajListuMogucihPoteza (1+ stapic) igrac trenutnoStanje 0 '() lista))
     (t (formirajListuMogucihPoteza (1+ stapic) igrac trenutnoStanje 0 '() (cons moguceStanje lista)))))
   (t (formirajListuMogucihPoteza stapic igrac trenutnoStanje 1 (nadjiStanje trenutnoStanje stapic igrac) lista))))

;-----POZIVOM OVE FUNKCIJE U KONZOLI SE PRIKAZUJE LISTA SVIH MOGUCIH NAREDNIH SITUACIJA U KOCKI-----
(print (formirajListuMogucihPoteza 0 'x stanje 0 '() '()))


;-----POZIVOM OVE FUNKCIJE SE U KONZOLI GRAFICKI PRIKAZUJU ("3D") SVE MOGUCE NAREDNE SITUACIJE U KOCKI
(defun sviMoguciPotezi(igrac)
  (progn (format t "~% Moguci potezi iz stanja: ")
    (odstampajOkvir) (format t "~%") (odstampajMatricu stanje 0 (- (* n 2) 2) 1 0) (odstampajOkvir)
    (format t "~% su: ")
    (loop for stapic from 0 to (1- (expt n 2)) do
        (promenaVirtuelnogStanja stanje stapic igrac))))

(sviMoguciPotezi 'x)





;-----FUNKCIJE KOJE OBEZBEDJUJU ODIGRAVANJE PARTIJE IZMEDJU DVA IGRACA-----

(defun start(igrac)
  (cond
   ((equal (krajIgre stanje) '()) (cond
                                   ((= igrac 1) 
                                    (progn  (format t "~%Na potezu je prvi igrac (x):") (unosPoteza stanje 'x) (if (equalp (krajIgre stanje) '()) (start '0))))
                                   (t (progn  (format t "~%Na potezu je drugi igrac (o):") (unosPoteza stanje 'o) (if (equalp (krajIgre stanje) '()) (start '1))))))
   (t )))

(defun prikaziMatricu()
  (progn (odstampajOkvir)(format t "~%") (odstampajMatricu (postaviPocetnoStanje '() '() 0 0) 0 (- (* n 2) 2) 1 0)))

(defun postaviPocetnoStanje(pomStanje pomPodLista clanoviUPodListi podLista)
  (cond
   ((= podLista (expt n 2)) (progn (setq stanje pomStanje)))
   ((= clanoviUPodListi n) (postaviPocetnoStanje (cons pomPodLista pomStanje) '() 0 (1+ podLista)))
   (t (postaviPocetnoStanje pomStanje (append '(-) pomPodLista) (1+ clanoviUPodListi) podLista))))

;-----IGRA OD POCETKA-----

(defun igra()
  (prikaziMatricu)
  (start '1))

(igra)

;-----IGRA KAD SE POCNE OD NEKOG PROIZVOLJNOG STANJA-----
(setq stanje '((x x - -)(x x o o)(o x x o)(o x o o)(x x x x)(o x o o)(o x x o)(o o - -)(x x x x)(x x o o)(o x x -)(o o o o)(x x x x)(x x o o)(o x x o)(o o x x)))

(defun igra2()
  (prikazProizvoljnogStanja)
  (start '1))

(igra2)


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


;1. pobednik stapici - za svaki od n^2 stapica, uvis
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

;2. pobednik po nivoima - za svaki od polozenih kvadrata velicine nxn (tj. kvadrata koji su paralelni sa podom) se proverava:
;;       po vrstama, po kolonama i dijagonalno
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


;;  3. pobednik strane omotac dijagonalno - za svaku od strana koja preseca kocku proveravaju se obe dijagonale - max 2*2*N poena
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

;;  4. pobednik unutrasnjost dijagonalno - za dve ravni koje dijagonalno presecaju kocku na pola, pa njihove dijagonale - max 4 poena
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












