
(setq comp 'x)

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

(defun da-li-je-kraj(st) 
  (cond
   ((null st) '())
   ((nemaCrtice (car st)) (da-li-je-kraj (cdr st)))
   (t '())))

(defun proceni-stanje (st)
  (cond
   ((equal st stanje0) 3)
   ((equal st stanje1) 2)
   ((equal st stanje2) 2)
   ((equal st stanje3) 4)
   ((equal st stanje4) 1) 
   ((equal st stanje5) 0)
   ((equal st stanje6) 3)
   ((equal st stanje7) 1)
   (t 0)))

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
               (x x x x)(x x o o)(o x x o)(o o o -)))



(setq n 4)


;;-----NOVA FUNKCIJA-----

(setq stanje0 
      '((x x o x)(x x o o)(o x x o)(o o o o)
        (x x x x)(x x o o)(o x x o)(o o o o)
        (x x x x)(x x o o)(o x o x)(o o o o)
        (x x x x)(x x o o)(o x x o)(o o x x)))

;(pobednik stanje0)

(setq stanje1
      '((x x o x)(x x o o)(o x x o)(o o o o)
        (x x x x)(x x o o)(o x x o)(o o o o)
        (x x x x)(x x o o)(o x x o)(o o o o)
        (x x x x)(x x o o)(o x x o)(o o x x)))

;(pobednik stanje1)

(setq stanje2 
      '((x x o x)(x x o o)(o x x o)(o o o o)
        (x x x x)(x x o o)(o x x o)(o o x x)
        (x x x x)(x x o o)(o x o x)(o o o o)
        (x x x x)(x x o o)(o x x o)(o o o o)))

;(pobednik stanje2)

(setq stanje3 
      '((x x o x)(x x o o)(o x x o)(o o o o)
        (x x x x)(x x o o)(o x x o)(o o x o)
        (x x x x)(x x o o)(o x o x)(o o o o)
        (x x x x)(x x o o)(o x x o)(o o o x)))

;(pobednik stanje3)

(setq stanje4
      '((x x x o)(x x o o)(o x x o)(o o o o)
        (x x x x)(x x o o)(o x x o)(o o o o)
        (x x x x)(x x o o)(o x o x)(o o o o)
        (x x x x)(x x o o)(o x x o)(o o x x)))

;(pobednik stanje4)

(setq stanje5 
      '((x x x o)(x x o o)(o x x o)(o o o o)
        (x x x x)(x x o o)(o x x o)(o o x x)
        (x x x x)(x x o o)(o x x o)(o o o o)
        (x x x x)(x x o o)(o x x o)(o o o o)))

;(pobednik stanje5)

(setq stanje6 
      '((x x x o)(x x o o)(o x x o)(o o o o)
        (x x x x)(x x o o)(o x x o)(o o x x)
        (x x x x)(x x o o)(o x o x)(o o o o)
        (x x x x)(x x o o)(o x x o)(o o o o)))

;(pobednik stanje6)

(setq stanje7 
      '((x x x o)(x x o o)(o x x o)(o o o o)
        (x x x x)(x x o o)(o x x o)(o o x o)
        (x x x x)(x x o o)(o x o x)(o o o o)
        (x x x x)(x x o o)(o x x o)(o o o x)))

;(pobednik stanje7)

;;-----STARE FUNKCIJE------


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

(print (a-b-Minimax stanje comp 7 t 0 0 0))













