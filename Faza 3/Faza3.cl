;;-----STARE FUNKCIJE-----
(defun promenaVirtuelnogStanja(st stapic potez)
  (setq virtuelnoStanje (progn (umetni (progn (promeniStanjeJednogStapica (nth (konvertujPotez stapic) st) potez '())) (konvertujPotez stapic) (obrisi '0  (konvertujPotez stapic) st))))
  (if (not (equalp virtuelnoStanje st)) (progn (format t "~% Potez: ~a" (konvertujBrojUPotez stapic))
                                         (odstampajOkvir)(format t "~%") (odstampajMatricu virtuelnoStanje 0 (- (* n 2) 2) 1 0) (odstampajOkvir))))

(defun nadjiStanje(stanje stapic potez)
  (setq virtuelnoStanje (progn (umetni (progn (promeniStanjeJednogStapica (nth (konvertujPotez stapic) stanje) potez '())) (konvertujPotez stapic) (obrisi '0  (konvertujPotez stapic) stanje))))
  (if (not (equalp virtuelnoStanje stanje)) virtuelnoStanje))

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

(setq stanje '((x x - -)(x x o o)(o x x o)(o o o o)
               (x x x x)(x x o o)(o x x o)(o o - -)
               (x x x x)(x x o o)(o x - -)(o o o o)
               (x x x x)(x x o o)(o x x o)(o o - -)))

(setq n 4)

;;-----IZMENJENA FUNKCIJA-----
(defun formirajListuMogucihPoteza(stapic igrac trenutnoStanje daLiJeVracenoStanje moguceStanje lista)
  (cond
   ((= stapic (expt n 2)) lista)
   ((= daLiJeVracenoStanje 1)
    (cond 
     ((equalp moguceStanje '()) 
      (formirajListuMogucihPoteza (1+ stapic) igrac trenutnoStanje 0 '() lista))
     (t 
      (formirajListuMogucihPoteza (1+ stapic) igrac trenutnoStanje 0 '() (cons moguceStanje lista)))))
   (t (formirajListuMogucihPoteza stapic igrac trenutnoStanje 1 (nadjiStanje trenutnoStanje stapic igrac) lista))))

(formirajListuMogucihPoteza 0 'x stanje 0 '() '())

;;-----STARE FUNKCIJE-----
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

;;-----NOVA FUNKCIJA-----
(defun proceniStanje (st)
  (cond
   ((equal st stanje0) 3)
   ((equal st stanje1) 4)
   ((equal st stanje2) 2)
   ((equal st stanje3) 4)
   ((equal st stanje4) 1) 
   ((equal st stanje5) 0)
   ((equal st stanje6) 3)
   ((equal st stanje7) 1)
   (t 0)))

(setq stanje0 
      '((x x o x)(x x o o)(o x x o)(o o o o)
        (x x x x)(x x o o)(o x x o)(o o o o)
        (x x x x)(x x o o)(o x o x)(o o o o)
        (x x x x)(x x o o)(o x x o)(o o x x)))

(pobednik stanje0)

(setq stanje1
      '((x x o x)(x x o o)(o x x o)(o o o o)
        (x x x x)(x x o o)(o x x o)(o o o o)
        (x x x x)(x x o o)(o x x o)(o o o o)
        (x x x x)(x x o o)(o x x o)(o o x x)))

(pobednik stanje1)

(setq stanje2 
      '((x x o x)(x x o o)(o x x o)(o o o o)
        (x x x x)(x x o o)(o x x o)(o o x x)
        (x x x x)(x x o o)(o x o x)(o o o o)
        (x x x x)(x x o o)(o x x o)(o o o o)))

(pobednik stanje2)

(setq stanje3 
      '((x x o x)(x x o o)(o x x o)(o o o o)
        (x x x x)(x x o o)(o x x o)(o o x o)
        (x x x x)(x x o o)(o x o x)(o o o o)
        (x x x x)(x x o o)(o x x o)(o o o x)))

(pobednik stanje3)

(setq stanje4
      '((x x x o)(x x o o)(o x x o)(o o o o)
        (x x x x)(x x o o)(o x x o)(o o o o)
        (x x x x)(x x o o)(o x o x)(o o o o)
        (x x x x)(x x o o)(o x x o)(o o x x)))

(pobednik stanje4)

(setq stanje5 
      '((x x x o)(x x o o)(o x x o)(o o o o)
        (x x x x)(x x o o)(o x x o)(o o x x)
        (x x x x)(x x o o)(o x x o)(o o o o)
        (x x x x)(x x o o)(o x x o)(o o o o)))

(pobednik stanje5)

(setq stanje6 
      '((x x x o)(x x o o)(o x x o)(o o o o)
        (x x x x)(x x o o)(o x x o)(o o x x)
        (x x x x)(x x o o)(o x o x)(o o o o)
        (x x x x)(x x o o)(o x x o)(o o o o)))

(pobednik stanje6)

(setq stanje7 
      '((x x x o)(x x o o)(o x x o)(o o o o)
        (x x x x)(x x o o)(o x x o)(o o x o)
        (x x x x)(x x o o)(o x o x)(o o o o)
        (x x x x)(x x o o)(o x x o)(o o o x)))

(pobednik stanje7)

;;-----STARE FUNKCIJE------
(defun krajIgre(st) 
  (cond
   ((null st) (progn (format t "~%Kraj igre~%") (pobednik stanje) '1))
   ((nemaCrtice (car st)) (krajIgre (cdr st)))
   (t '())))

(defun nemaCrtice(podLista)
  (cond ((null podLista) T)
        ((equalp '- (car podLista)) '() )
        (t (nemaCrtice (cdr podLista)))))

;;-----DODATNE FUNKCIJE SA SLAJDA-----
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

(setq comp 'x)
(setq covek 'o)

;;-----GLAVNA FUNKCIJA-----

(defun minimax (st dubina potez start)
  (cond 
   ((zerop dubina) (list st (or (krajIgre st) (proceniStanje st))))
   (t (let ((listaPoteza (formirajListuMogucihPoteza 0 potez st 0 '() '()))
        (mm (if (equal potez comp) 'max-stanje 'min-stanje)))
        (cond
         ((null listaPoteza)
          (list st (or (krajIgre st) (proceniStanje st))))
         (start (apply mm (list 
                           (mapcar (lambda (x)
                                     (minimax2 x (1- dubina) (if (equal potez 'x) 'o 'x) '())) listaPoteza))))
         (t (cons st (cdr (apply mm (list (mapcar (lambda (x)
                                                    (minimax2 x (1- dubina)  (if (equal potez 'x) 'o 'x) '()))
                                            listaPoteza)))))))))))

(print (minimax stanje 6 'x t))










