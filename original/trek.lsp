#|
	Trek
	Original BASIC version is 
	  http://www.dunnington.u-net.com/public/startrek/startrek.txt
        (some information in http://www.dunnington.u-net.com/public/startrek/)

	Rewritten in Common Lisp by Shozo TAKEOKA (take@axe-inc.co.jp)
			http://www.takeoka.org/~take/
	2007/FEB/12 CL Ver.1.2.2
	2007/JAN/30 CL Ver.1.2.1
	2006/DEC/23 CL Ver.1.2
	2006/DEC/21 CL Ver.1.1
	2006/OCT/09 CL Ver.1

 tested system:
  GCL2.2.6 under WindowsXP
  GCL2.2.5 undef FreeBSD5.4R
  ALLEGRO-V8.0 under WindowsXP
  Clisp under MacOS 10.3.9 (PPC)
|#


;(si:chdir "/take/src/trek")
;(load "trek.lsp")
;(trek)

;#+:GCL (si::use-fast-links nil)

;;;
(defstruct klingon
  (x 0)
  (y 0)
  (energy 0))

(defstruct quad
  (base 0)
  (star 0)
  (klingon 0)
  (visit nil))



;;; direction vector. 9th data is same as 1st data
(defconstant *cx*
      (make-array '9 :initial-contents
		  '(-1 -1 0 1 1 1 0 -1 -1)))
(defconstant *cy*
      (make-array '9 :initial-contents
		  '(0 1 1 1 0 -1 -1 -1 0)))

;;;Constant
(defconstant *full-energy* 3000)
(defconstant *full-torpedo* 10) ; max of torpedoes
(defconstant *klingon-max-energy* 200)


;;; Global var
(defvar *ggg* (make-array '(8 8) :initial-element 0))
(defvar *kkk* (make-array '(4) :initial-element 0))

(defvar *time* 0)
(defvar *time0* 0)
(defvar *t-period* 0)

(defvar *base-total* 0) ; Base total No.
(defvar *klingon-total* 0) ; Klingon total No.

(defvar *c-klingons* 0) ; current-Klingons
(defvar *c-bases* 0) ; current-Bases
(defvar *c-stars* 0) ; current-Stars

(defvar *bx* 0) ; Base-X pos in Quad
(defvar *by* 0) ; Base-Y pos in Quad

(defvar *ex* 0) ; Enterprise X pos in Quad
(defvar *ey* 0) ; Y pos in Quad
(defvar *qx* 0) ;Quadorant X
(defvar *qy* 0) ;Quadorant Y
(defvar *energy* 0)
(defvar *torpedo* 0)  ;No. of torpedoes
(defvar *shield* 0)  ;shield
(defvar *docked* nil) ; docked

;;; global flags
(defvar *klingon-attack* nil) ; turn of Klingon

(defvar *success* nil)  ;success flag
(defvar *mission-end* nil)  ;mission terminated




(defun title ()
  (format t "THE USS ENTERPRISE --- NCC-1701~%")
  (format t "                  ,------*------,~%" )
  (format t "  ,-------------   '---  ------'~%" )
  (format t "   '-------- --'      / /~%" )
  (format t "       ,---' '-------/ /--,~%" )
  (format t "        '----------------'~%" )
  )

(defmacro aset (a n v)
  `(setf (aref ,a ,n) ,v))

(defmacro aset2 (a x y  v)
  `(setf (aref ,a ,x ,y ) ,v))


(defun init()
  (setq *damage-repair-magic-number* (/(random 50) 100)) ;D4=.5*RND(1)
;  (setq *bbb* (make-array 2 :initial-element 0))
  (setq *success* nil)
  (setq *mission-end* nil)
  (setq *klingon-attack* nil)
  (setq *klingon-org* 0)
  (dotimes (i 4)
    (setf (aref *kkk* i) (make-klingon)))
  (setq *time* (* (+ (random 20) 20) 100)) ; current time
  (setq *time0* *time*) ; initial time
  (setq *t-period* (+ (random 10) 25)) ; end time
  (setq *docked* nil) ; docked
  (setq *energy* *full-energy*)
  (setq *torpedo* *full-torpedo*) ;No. of torpedoes
  (setq *shield* 0)  ;shield
)

(defun init2()
 (setq *qx* (random 8)) ;Quadorant X
 (setq *qy* (random 8)) ;Quadorant Y
 (setq *ex* (random 8)) ;Sector X
 (setq *ey* (random 8)) ;Sector Y
 (setq *ddd* (make-array 10 :initial-element 0)) ;no Damage
)


;470 DEF FND(D)=SQR((K(I,1)-S1)^2+(K(I,2)-S2)^2)
(defun klingon-distance(i)
  (let* (
	(k (aref *kkk* i))
	(xx1 (- (klingon-x k) *ex*))
	(xx2 (- (klingon-y k) *ey*)))
    (floor (+(sqrt (+(* xx1 xx1) (* xx2 xx2))) 0.5))))


;475 DEF FNR(R)=INT(RND(R)*7.98+1.01)
(defun rnd1-8()
  (1+ (random 8)))

(defun fnrand()
  (random 8))

(defun trek()
  (title)
  (loop
   (trek1)
   (if (not(more-mission)) (return)))
   (format t "~%*** END ***~%"))


(defun trek1()
  (init)
  (init2)
  (make-galaxy)
  (print-mission)
  (if (not(acceptp)) (return-from trek1))
  ;;;
  (enter-quad)
  (catch 'game-end 
    (mloop))
  (when (or *success* (<= *klingon-total* 0))
    (success)))



(defun make-galaxy1()
  (let (k3 b3)
    (setq *base-total* 0) ; Base total No.
    (setq *klingon-total* 0) ; Klingon total No.
    (dotimes (i 8)
      (dotimes (j 8)
	(incf *klingon-total*
	      (setq k3
		    (cond ((>(setq r (random 100)) 98) 3)
			  ((> r 95) 2)
			  ((> r  8) 1)
			  (t 0))))
	(incf *base-total* 
	      (setq b3
		    (cond ((>(random 100) 96) 1)
			  (t 0))))
	(setf (aref *ggg* i j)
	      (make-quad 
	       :klingon k3 :base b3 :star (rnd1-8)))))))


(defun make-galaxy()
  (make-galaxy1)
  (if(> *klingon-total* *t-period*) (setq *t-period* (1+ *klingon-total*)))
  (cond ((zerop *base-total*)
	  (cond ((< (quad-klingon (aref *ggg* *qx* *qy*)) 2)
		 (incf *klingon-total*)
		 (incf (quad-klingon (aref *ggg* *qx* *qy*)))))
	  (setq *base-total* 1)
	  (incf (quad-base (aref *ggg* *qx* *qy*)))
	  (setq *qx* (fnrand))
	  (setq *qy* (fnrand))))
  (setq *klingon-org* *klingon-total*))



(defun print-mission()
  (format t "YOUR ORDERS ARE AS FOLLOWS:~%")
  (format t "--------------------------~%")
  (format t "   DESTROY THE ~a KLINGON WARSHIPS WHICH HAVE INVADED~%"
	  *klingon-total*)
  (format t "   THE GALAXY BEFORE THEY CAN ATTACK FEDERATION HEADQUARTERS~%")
  (format t "   ON STARDATE ~5,2f. THIS GIVES YOU ~a DAYS.~%"
	(+ *time0* *t-period*) *t-period* )
  (format t
    " THERE ~a ~a STARBASE~a IN THE GALAXY FOR RESUPPLYING YOUR SHIP.~%~%"
	  (if (eql *base-total* 1) "IS" "ARE")
	  *base-total*
	  (if (eql *base-total* 1) "" "S")))


#|
|#
(defun acceptp()
  (format t "ARE YOU READY TO ACCEPT COMMAND? ('N' FOR End)")
  (not(eql (read) 'n)))
  

(defun enter-quad ()
  (let (k)
    (setf (quad-visit (aref *ggg* *qx* *qy*)) t) ; make known Quad
    (when(not(or (< *qx* 0) (> *qx* 7) (< *qy* 0) (> *qy* 7)))
      (disp-quad-name (quad-name *qy* *qx* 0))
      (enter-quad1)
;(format t "*c-bases*=~a *c-klingons*=~a *c-stars*=~a~%" *c-bases* *c-klingons* *c-stars*)
      (repo-entering-quad-stat))
    (init-sector)
    (short-range-sensor)))


(defun disp-quad-name(qq)
  (cond
   ((eql *time0* *time*)
    (format t "YOUR MISSION BEGINS WITH YOUR STARSHIP LOCATED~%")
    (format t "IN THE GALACTIC QUADRANT, '~a'.~%" qq))
   (t 
    (format t "~%NOW ENTERING ~a QUADRANT . . .~%" qq))))

(defun repo-entering-quad-stat ()
  (when (/= *c-klingons* 0)
    (format t "  COMBAT AREA      CONDITION RED  ~%"))
  (when (<= *shield* 200)
    (format t "      SHIELDS DANGEROUSLY LOW     ~%")))

(defun enter-quad1 ()
  (let ((g (aref *ggg* *qx* *qy*)))
    (setq *c-klingons* (quad-klingon g))
    (setq *c-bases* (quad-base g))
    (setq *c-stars* (quad-star g))))


(defun energy-check()
  (if (and (> (+ *shield* *energy*) 10)
	   (or (> *energy* 10) (zerop (aref *ddd* 7))))
      (return-from energy-check t))
  (format t "** FATAL ERROR **~%")
  (format t "YOU'VE JUST STRANDED YOUR SHIP IN SPACE.~%")
  (format t "YOU HAVE INSUFFICIENT MANEUVERING ENERGY,~%")
  (format t "AND SHIELD CONTROL IS PRESENTLY INCAPABLE OF~%")
  (format t "CROSS-CIRCUITING TO ENGINE ROOM!!~%")
  nil
;   PRINT:GOTO 6220
)


(defun help-com ()
  (format t "ENTER ONE OF THE FOLLOWING:~%")
  (format t "--------------------------~%")
  (format t "  W  (WARP)~%")
  (format t "  S  (FOR SHORT RANGE SENSOR SCAN)~%")
  (format t "  L  (FOR LONG RANGE SENSOR SCAN)~%")
  (format t "  P  (TO FIRE PHASERS)~%")
  (format t "  T  (TO FIRE PHOTON TORPEDOES)~%")
  (format t "  Z  (TO RAISE OR LOWER SHIELDS)~%")
  (format t "  R  (FOR DAMAGE CONTROL REPORTS)~%")
  (format t "  C  (TO CALL ON LIBRARY-COMPUTER)~%")
  (format t "  XXX  (TO RESIGN YOUR COMMAND)~%")
  (format t "  (zzz  break for debug)~%"))


(defun init-sector()
  (let (x y k)
    (setq *sec* (make-array '(8 8) :initial-element nil))
    (setf (aref *sec* *ex* *ey*) 'e)
    (dotimes (i 4) (setf (klingon-energy(aref *kkk* i)) 0))
    (dotimes (i *c-klingons*)
      (loop
       (setq x (fnrand))
       (setq y (fnrand))
       (when (not(aref *sec* x y))
	 (setq k (aref *kkk* i))
	 (setf (klingon-x k) x)
	 (setf (klingon-y k) y)
	 (setf (klingon-energy k) (* *klingon-max-energy*(+ 5(random 10)) 0.1))
	 (setf (aref *sec* x y) 'k)
	 (return))))
    (dotimes (i *c-bases*)
      (loop
       (setq x (fnrand))
       (setq y (fnrand))
       (when (not(aref *sec* x y))
	 (setq *bx* x)
	 (setq *by* y)
	 (setf (aref *sec* x y) 'b)
	 (return))))
    (dotimes (i *c-stars*)
      (loop
       (setq x (fnrand))
       (setq y (fnrand))
       (when (not(aref *sec* x y))
	 (setf (aref *sec* x y) 's)
	 (return))))))




(defun mloop()
  (let (klatt)
    (loop
;1990
     (when (or *success* (<= *klingon-total* 0))
       (return-from mloop (success)))
     (when (> *time* (+ *time0* *t-period*))
       (return-from mloop (fail-mission)))

     (when *mission-end*
       (return-from mloop))

     (when *klingon-attack*
       (klingon-attack)
       (setq *klingon-attack* nil))

     (when (not(energy-check))
       (fail-mission)
       (return-from mloop nil))

     (format t "~%COMMAND")
     (setq aaa (read))

     (case aaa
      (w 
       (if(not(nav)) (return-from mloop nil)))
      (s (short-range-sensor))
      (l (long-range-sensor))
      (p (phaser))
      ((t) (torpedo))
      (z (shield))
      (r (damage-report))
      (c (computer))
      (xxx (end-of-mission))
      (zzz (break))
      (t (help-com)))
     )
    ))

(defun nav()
  (let (c1 n w1)
    (when (not (setq c1 (input-course "LT. SULU"))) (return-from nav t))
;(format t "c1=~a~%" c1)
    (when (not (setq w1 (nav-factor))) (return-from nav t))
;(format t "w1=~a~%" w1)
    (when (not (setq n  (nav-energy w1))) (return-from nav t))
;(format t "n=~a~%" n)
    (klingon-attack-warp)
    (repair-by-warp w1)
    (damage-by-warp)
    (when (not (nav4 c1 n w1)) (return-from nav t))
    (warp-time w1)
))

(defun warp-time (w1)
  (let ((t8 1))
    (if (< w1 1) (setq t8 (/ (floor (* 10 w1)) 10)))
    (incf *time* t8)
    (cond
     ((> *time* (+ *time0* *t-period*))
	(fail-mission))
     (t t))))


(defun input-course (man)
  (format t "COURSE (0-8, -1)")
  (setq c1 (read))
  (cond
   ((not(numberp c1)) nil)
   ((= c1 -1) nil)
   (t
    (cond
     ((or (< c1 0) (> c1 8))
      (format t "   ~a: 'INCORRECT COURSE DATA, SIR!'" man)
      t)
     (t
      (if(= c1 8)
	  0
	c1))))))


(defun nav-factor ()
  (let* (
	 (wdamage (aref *ddd* 1))
	 (x (if (< wdamage 0) 0.2 8))
	 w1)
    (format t "WARP FACTOR (0-~a)" x)
    (setq w1 (read))
    (cond
     ((not(numberp w1)) nil)
     ((= w1 0) nil)
     ((and (< wdamage 0)(> w1 0.2))
      (format t "WARP ENGINES ARE DAMAGED.  MAXIUM SPEED = WARP 0.2")
      nil)
     ((or (< w1 0)(> w1 8))
      (format t "   CHIEF ENGINEER SCOTT: 'THE ENGINES WON'T TAKE WARP~a!'" w1)
      nil)
     (t w1))))



(defun nav-energy (w1)
  (let (
	(n (floor (+(* w1 8) 0.5))))
    (cond
     ((< *energy* n)
      (format t"ENGINEERING:  'INSUFFICIENT ENERGY AVAILABLE~%")
      (format t"               FOR MANEUVERING AT WARP~a!'~%" w1)
      (cond
       ((or (< *shield* (- n *energy*)) (< (aref *ddd* 7) 0))
	t)
       (t 
	(format t"DEFLECTOR CONTROL ROOM:  ~a UNITS OF ENERGY" *shield*)
	(format t"                          PRESENTLY DEPLOYED TO SHIELDS.")))
      t)
     (t n))))



(defun klingon-attack-warp()
  (dotimes (i 4)
    (cond
     ((/= 0 (klingon-energy (aref *kkk* i)))
;(format t "k-att move ~a~%" i)
      (klingon-rand-move i))))
  (klingon-attack))


(defun repair-by-warp(w1)
  (let ((flag nil) x)
    (dotimes (i 9)
      (setq ii (1+ i))
      (cond
       ((<(aref *ddd* ii) 0)
	(setq x (incf (aref *ddd* ii)))
	(cond
	 ((>= x 0)
	  (setf (aref *ddd* ii) 0)
	  (cond
	   ((not flag)
	    (format t "DAMAGE CONTROL REPORT:  ")
	    (setq flag t)))
	  (format t "~a REPAIR COMPLETED.~%" (device-name ii)))))))))

(defun damage-by-warp()
  (let (damdev)
    (cond
     ((<= (random 10) 2)
      (setq damdev (rnd1-8))
      (incf (aref *ddd* damdev)
	    (cond 
	     ((<(random 10) 6)
	      (format t "DAMAGE CONTROL REPORT:  ~a DAMAGED~%"
		      (device-name damdev))
	      (* -1 (1+ (/(random 500)100))))
	     (t 
	      (format t "DAMAGE CONTROL REPORT:  ~a STATE OF REPAIR IMPROVED~%"
		      (device-name damdev))
	      (1+ (/(random 300)100)))))))))

(defun cal-vec (va c1)
  (let (ci cr)
    (setq ci(floor c1))
    (setq cr (- c1 ci))
    (+ (aref va ci)
       (* (- (aref va (+ ci 1)) (aref va ci))
	  cr))))

		
(defun nav4 (c1 n w1)
  (let
      (
       (x *ex*) (y *ey*)
       (dx (cal-vec *cx* c1))
       (dy (cal-vec *cy* c1))
       (x0 *ex*) (y0 *ey*) (old-qx *qx*) (old-qy *qy*))
    (setf (aref *sec* *ex* *ey*) nil)
    (dotimes (i n)
      (incf x dx)(incf y dy)
      (setq *ex* (floor (+ x 0.5)))
      (setq *ey* (floor (+ y 0.5)))
      (cond 
       ((or (< *ex* 0)(> *ex* 7)
		(< *ey* 0)(> *ey* 7))
	(return-from nav4
		     (cond ((exit-quad n x0 y0 dx dy w1 old-qx old-qy)
			    (enter-quad) t)
			   (t nil))))
       (t (format t "(~a,~a)" *ex* *ey*)))

      (when(aref *sec* *ex* *ey*)
	(setq *ex* (floor(- x dx)))
	(setq *ey* (floor(- y dy)))
	(format t "~%WARP ENGINES SHUT DOWN AT ")
	(format t "SECTOR ~a , ~a DUE TO BAD NAVAGATION" *ex* *ey*)
	(return)))
    (setf (aref *sec* *ex* *ey*) 'E)
    (dec-energy n)
    (format t "~%")
    (short-range-sensor)
    t))


(defun exit-quad (n x y x1 y1 w1 old-qx old-qy)
  (let ((flag nil))
    (incf x (+ (* 8 *qx*) (* n x1)))
    (incf y (+ (* 8 *qy*) (* n y1)))
    (setq *qx* (floor (/ x 8)))
    (setq *qy* (floor (/ y 8)))
    (setq *ex* (floor (- x (* *qx* 8))))
    (setq *ey* (floor (- y (* *qy* 8))))
    (when (< *qx* 0) (setq flag t)(setq *qx* 0)(setq *ex* 0))
    (when (> *qx* 7) (setq flag t)(setq *qx* 7)(setq *ex* 7))
    (when (< *qy* 0) (setq flag t)(setq *qy* 0)(setq *ey* 0))
    (when (> *qy* 7) (setq flag t)(setq *qy* 7)(setq *ey* 7))
    (cond (flag
	   (format t "LT. UHURA: MESSAGE FROM STARFLEET COMMAND --~%")
	   (format t
		   "  'PERMISSION TO ATTEMPT CROSSING OF GALACTIC PERIMETER~%")
	   (format t "  IS HEREBY *DENIED*.  SHUT DOWN YOUR ENGINES.'~%")
	   (format t "CHIEF ENGINEER SCOTT:  'WARP ENGINES SHUT DOWN~%")
	   (format t "  AT SECTOR ~a , ~a OF QUADRANT ~a , ~a.'~%"
		   *ex* *ey* *qx* *qy*)
	   (setf (aref *sec* *ex* *ey*) 'E)
	   (when (> *time* (+ *time0* *t-period*))
	     (fail-mission)
	     nil))
	  (t
	   (cond
	    ((and (eql *qx* old-qx)(eql *qy* old-qy))
	     (warp-time w1))
	    (t
	     (incf *time*)
	     (dec-energy n)
	     t))))))
	   

;;; dec energy
(defun dec-energy (n)
  (when (< (decf *energy* (+ n 10)) 0)
    (format t "SHIELD CONTROL SUPPLIES ENERGY TO COMPLETE THE MANEUVER.~%")
    (incf *shield* *energy*)
    (setq *energy* 0)
    (when (<= *shield* 0)
      (setq *shield* 0))))

;;; Long sensor
;4000
(defun long-range-sensor()
  (let (x y qqq)
    (cond 
     ((<(aref *ddd* 3) 0)
      (format t "LONG RANGE SENSORS ARE INOPERABLE.~%"))
     (t
      (format t "LONG RANGE SCAN FOR QUADRANT ~a,~a~%" *qx* *qy*)
      (dotimes (i 3)
	(dotimes (j 3)
	  (setq x (+ *qx* i -1))
	  (setq y (+ *qy* j -1))
	  (cond
	   ((and (>= x 0)(<= x 7)(>= y 0)(<= y 7))
	    (setq qqq (aref *ggg* x y))
	    (setf (quad-visit qqq) t)
	    (format t " ~1a~1a~1a"
		    (quad-klingon qqq)
		    (quad-base qqq)
		    (quad-star qqq)))
	   (t (format t " ***"))))
	(format t "~%"))))))
	    

(defun noememy()
  (format t "SCIENCE OFFICER SPOCK:  'SENSORS SHOW NO ENEMY SHIPS~%")
  (format t "                         IN THIS QUADRANT'"))


;;; phaser
;4260
(defun phaser()
  (cond 
   ((<(aref *ddd* 4) 0)
    (format t "PHASERS INOPERATIVE.~%"))
   (t
    (cond
     ((<= *c-klingons* 0)
      (noememy))
     (t (phaser1))))))

(defun phaser1 ()
  (let (x)
    (cond 
     ((<(aref *ddd* 8) 0)
      (format t "COMPUTER FAILURE HAMPERS ACCURACY.~%"))
     (t
      (format t "PHASERS LOCKED ON TARGET;  ")))
    (cond
     ((setq x (phaser3))
      (phaser4 x)))))

(defun phaser3 ()
  (let (x)
    (loop
     (format t "PHASERS LOCKED ON TARGET;  ")
     (format t "ENERGY AVAILABLE = ~a UNITS~%" *energy*)
     (format t "NUMBER OF UNITS TO FIRE ?")
     (setq x (read))
     (cond
      ((not(numberp x))(return-from phaser3 nil))
      ((<= x 0)
       (return-from phaser3 nil))
      (t 
       (cond
	((>= (- *energy* x) 0)
	 (return-from phaser3 x))))))))


(defun phaser4 (x)
  (let (h ke kx ky k)
    (decf *energy* x)
    (cond
     ((<(aref *ddd* 8) 0)
      (setq x (random x))))
    (setq h1 (floor (/ x *c-klingons*)))
    (setq *klingon-attack* t)
    (dotimes (i 4)
      (setq k (aref *kkk* i))
      (setq kx (klingon-x k))
      (setq ky (klingon-y k))
      (cond
       ((> (klingon-energy k) 0)
	(setq h (floor (* (/ h1 (klingon-distance i)) (+ 2 (/(random 10)10)))))
	(cond
	 ((<= h (* (klingon-energy k) 0.15))
	  (format t "SENSORS SHOW NO DAMAGE TO ENEMY AT ~a , ~a.~%"
		  kx ky))
	 (t
	  (setq ke (decf (klingon-energy k) h))
	  (format t "~a UNIT HIT ON KLINGON AT SECTOR ~a,~a.~%" h kx ky)
	  (cond
	   ((<= ke 0)
	    (delete-klingon i))
	   (t
	    (format t " (SENSORS SHOW ~3,2f UNITS REMAINING)~%" ke))))))))))



(defun delete-star (x y)
    (setf (aref *sec* x y) nil)
    (decf (quad-star(aref *ggg* *qx* *qy*))))

(defun delete-klingon(i)
  (let (kx ky k)
    (setq k (aref *kkk* i))
    (setq kx (klingon-x k))
    (setq ky (klingon-y k))
    (format t "*** KLINGON DESTROYED ***~%")
    (decf *c-klingons*)
    (decf *klingon-total*)
    (setf (aref *sec* kx ky) nil)
    (setf (klingon-energy k) 0)
    (decf (quad-klingon(aref *ggg* *qx* *qy*)))))

(defun delete-klingon-xy (x y)
  (let (k)
      (dotimes (i 4)
	(setq k (aref *kkk* i))
	(cond
	 ((and
	   (/= (klingon-energy k) 0)
	   (= (klingon-x k) x)
	   (= (klingon-y k) y))
	  (delete-klingon i)
	  (return))
	 (t nil)))))

(defun delete-base (x y)
  (let ()
    (decf *c-bases*)
    (decf *base-total*)
    (setf (aref *sec* x y) nil)
    (decf (quad-base(aref *ggg* *qx* *qy*)))))



(defun torpedo ()
  (let (c1 obj)
    (cond
     ((<= *torpedo* 0)
      (format t "ALL PHOTON TORPEDOES EXPENDED.~%"))
     ((< (aref *ddd* 5) 0)
      (format t "PHOTON TUBES ARE NOT OPERATIONAL.~%"))
     (t
      (format t "PHOTON TORPEDO ")
      (when (not (setq c1 (input-course "ENSIGN CHEKOV")))
	(return-from torpedo t))
;      (format t "c1=~a~%" c1)
      (decf *energy* 2)
      (decf *torpedo* 1)
      (torpedo-fire c1)
      (setq *klingon-attack* t)))
    t))


(defun torpedo-fire(c1)
  (let ((x *ex*) (y *ey*))
    (setq x1 (cal-vec *cx* c1))
    (setq y1 (cal-vec *cy* c1))

    (format t "TORPEDO TRACK:")
    (loop
     (incf x x1)
     (incf y y1)
     (setq x3 (floor (+ x 0.5)))
     (setq y3 (floor (+ y 0.5)))
     (cond
      ((or (< x3 0) (> x3 7) (< y3 0)(> y3 7))
       (format t "~%TORPEDO MISSED.~%")
       (return))
      ((eql (setq obj (aref *sec* x3 y3)) 'k)
       (format t "~%")
       (delete-klingon-xy x3 y3)
       (when (<= *klingon-total* 0)
	 (setq *success* t)
	 (throw 'game-end t))
       (return))
      ((eql obj 's)
       (format t "~%STAR AT ~a, ~a ABSORBED TORPEDO ENERGY.~%" x3 y3)
       (delete-star x3 y3)
       (return))
      ((eql obj 'b)
       (format t "~%*** STARBASE DESTROYED ***~%")
       (delete-base x3 y3)
       (destroy-base)
       (return)))
     (format t "(~a,~a)" x3 y3))))


(defun destroy-base ()
  (cond 
   ((or (> *base-total* 0) (> *klingon-total* (- *time* *time0* *t-period*)))
    (format t "STARFLEET COMMAND REVIEWING YOUR RECORD TO CONSIDER")
    (format t "COURT MARTIAL!")
    (setq *docked* nil)
    (setq *klingon-attack* t))
   (t
    (format t "THAT DOES IT, CAPTAIN!!  YOU ARE HEREBY RELIEVED OF COMMAND")
    (format t "AND SENTENCED TO 99 STARDATES AT HARD LABOR ON CYGNUS 12!!")
    (end-of-mission))))


;;; shield
;5530
(defun shield()
  (let (x y)
    (cond 
     ((<(aref *ddd* 7) 0)
      (format t "SHIELD CONTROL INOPERABLE.~%"))
     (t
      (format t "ENERGY AVAILABLE =~a.  NUMBER OF UNITS TO SHIELDS ?"
	      (+ *energy* *shield*))
      (setq x (read))
      (cond
       ((or (< x 0) (= x *shield*))
	(format t "<SHIELDS UNCHANGED>~%"))
       ((> x (+ *energy* *shield*))
	(format t "SHIELD CONTROL:  'THIS IS NOT THE FEDERATION TREASURY.'" )
	(format t "<SHIELDS UNCHANGED>"))
       (t
	(incf *energy* (- *shield* x))
	(setq *shield* x)
	(format t "DEFLECTOR CONTROL ROOM:")
	(format t "  'SHIELDS NOW AT ~a UNITS PER YOUR COMMAND.'" *shield*)))))))


;;; damage report
;5690
(defun damage-report()
  (cond
   ((< (aref *ddd* 6) 0)
    (format t "DAMAGE CONTROL REPORT NOT AVAILABLE.~%"))
   (t
    (show-stat-repair)))
  (docked-repair))

(defun show-stat-repair()
  (format t "DEVICE             STATE OF REPAIR~%")
  (format t "------             ---------------~%")
  (dotimes (i 8)
    (format t "~a ~3,2f~%" (device-name (1+ i))
	    (* (floor (* 100 (aref *ddd* (1+ i)))) 0.1))))

(defun docked-repair()
  (let (d3)
    (cond
     (*docked*
      (setq d3 0)
      (dotimes (i 8)
	(when (< (aref *ddd* (1+ i)) 0)
	  (incf d3 0.1)))
      (cond
       ((= d3 0) nil)
       (t (need-repair d3)))))))

(defun need-repair (d3)
  (incf d3 *damage-repair-magic-number*)
  (setq d3 (if (>= d3 1)  0.9  d3))
  (format t "TECHNICIANS STANDING BY TO EFFECT REPAIRS TO YOUR SHIP;")
  (format t "ESTIMATED TIME TO REPAIR: ~3,2f STARDATES."
	  (* .01 (floor (* 100 D3))))
  (format t "WILL YOU AUTHORIZE THE REPAIR ORDER (Y/N)")
  (cond ((eql 'y (read))
	 (repair-all)
	 (incf *time* (+ d3 0.1))
	 (show-stat-repair))
	(t nil)))
	    
(defun repair-all()
  (dotimes (i 8)
    (cond((<  (aref *ddd* (1+ i)) 0)
	  (aset *ddd* (1+ i) 0)))))


;;; klingon attack
;6000
(defun klingon-attack()
  (cond
   ((<= *c-klingons* 0) t)
   (*docked*
    (format t "STARBASE SHIELDS PROTECT THE ENTERPRISE.~%")
    t)
   (t
    (dotimes (i 4)
      (let* ((k (aref *kkk* i))(ke (klingon-energy k)) h)
	(when (> ke 0)
	  (setq h (floor (*(/ ke (klingon-distance i))
			   (+ 2 (/(random 10)10)))))
	  (decf *shield* h)
	  (setf (klingon-energy k)(/ ke  (+ 3 (/(random 10)10))))
	  (format t "~a UNIT HIT ON ENTERPRISE FROM SECTOR ~a,~a .~%" 
		  h (klingon-x k)(klingon-y k))
	  (cond
	   ((<= *shield* 0)
	    (enterprise-destroyed)
	    (return-from klingon-attack))
	   (t
	    (format t 
		    "      <SHIELDS DOWN TO ~a UNITS>~%" *shield* )
	    (when(>= h 20)
	      (when (and (<= (random 10) 6) (> (/ h *shield*) 0.02))
		(setq r1 (rnd1-8))
		(decf (aref *ddd* r1) (+ (/ h *shield*) (/ (random 50) 100)))
		(format t "DAMAGE CONTROL: '~a DAMAGED BY THE HIT'"
			(device-name r1))))))))))))




;;; Fail 1 energy==0 or timeout
;6220
(defun fail-mission()
  (format t "IT IS STARDATE ~5,2f.~%" *time*)
  (end-of-mission))

;;; Fail destroyed
;6240
(defun enterprise-destroyed ()
  (format t "~%~%THE ENTERPRISE HAS BEEN DESTROYED.")
  (format t "  THE FEDERATION WILL BE CONQUERED.~%")
  (fail-mission))

;;; end of mission
;6270
(defun end-of-mission()
  (format t "THERE WERE ~a KLINGON BATTLE CRUISERS LEFT AT~%" *klingon-total*)
  (format t "THE END OF YOUR MISSION.~%")
  (setq *mission-end* t)
  (throw 'game-end nil))

(defun more-mission ()
  (cond
   ((/= *base-total* 0)
    (format t "~%THE FEDERATION IS IN NEED OF A NEW STARSHIP COMMANDER~%")
    (format t "FOR A SIMILAR MISSION -- IF THERE IS A VOLUNTEER,~%")
    (format t "LET HIM STEP FORWARD AND ENTER 'AYE'" )
    (eql 'aye (read)))
   (t nil)))



;;; success
;6370
(defun success ()
  (let ((x (/ *klingon-org* (- *time* *time0*))))
    (format t "CONGRATULATIONS, CAPTAIN!  THE LAST KLINGON BATTLE CRUISER~%")
    (format t "MENACING THE FEDERATION HAS BEEN DESTROYED.~%~%")
    (format t "YOUR EFFICIENCY RATING IS ~s"
	    (* x x 1000))))


;;; 
;6430
(defun dockedp()
  (let (x y)
    (dotimes (i 3)
      (dotimes (j 3)
	(setq x (+ *ex* i -1))
	(setq y (+ *ey* j -1))
	(when (and (>= x 0)(<= x 7)(>= y 0)(<= y 7)(eql 'b (aref *sec* x y)))
	  (setq *condi* "DOCKED")
	  (setq *docked* t)
	  (setq *energy* *full-energy*)
	  (setq *torpedo* *full-torpedo*)
	  (setq *shield* 0)
	  (format t "SHIELDS DROPPED FOR DOCKING PURPOSES.~%")
	  (return-from dockedp t))))
    (setq *docked* nil)
    nil))

(defun set-condition()
  (cond ((not(dockedp))
	 (cond
	  ((> *c-klingons* 0) (setq *condi* "*RED*"))
	  ((< *energy* (/ *full-energy* 10)) (setq *condi* "YELLOW"))
	  (t  (setq *condi* "GREEN"))))))
  


;;; 
;;; short range sensor
; f1980
(defmacro srs() (short-range-sensor))
(defun short-range-sensor()
  (let (fff)
    (set-condition)
    (when (<(aref *ddd* 2) 0)
      (format t "*** SHORT RANGE SENSORS ARE OUT ***~%")
      (return-from short-range-sensor nil))
  ;
    (format t "   +0-1-2-3-4-5-6-7-+")
    (setq fff  *disp-info-funcs*)
    (dotimes (i 8)
      (format t "~%  ~a|" i)
      (dotimes (j 8)
	(format t "~a " 
		(case  (aref *sec* i j)
		       ((s) "*")
		       ((k) "K")
		       ((b) "B")
		       ((e) "E")
		       (t   "."))))
      (format t "|")
;      (disp-info i)
      (apply (pop fff) nil)
      )
    (format t "~%")))

  
(defconstant
 *disp-info-funcs*
 (list
   (lambda()
     (format t "        STARDATE           ~5,2f" (/(floor(* *time* 10)) 10)))
   (lambda()
     (format t "        CONDITION          ~a" *condi*))
   (lambda()
     (format t "        QUADRANT           ~a ~a" *qx* *qy*))
   (lambda()
     (format t "        SECTOR             ~a ~a" *ex* *ey*))
   (lambda()
     (format t "        PHOTON TORPEDOES   ~a" *torpedo*))
   (lambda()
     (format t "        TOTAL ENERGY       ~5,2f" (+ *energy* *shield*)))
   (lambda()
     (format t "        SHIELDS            ~5,2f" *shield*))
   (lambda()
     (format t "        KLINGONS REMAINING ~a" *klingon-total*))))

;;;
;;;
;;;

(defun comp-help()
  (format t "FUNCTIONS AVAILABLE FROM LIBRARY-COMPUTER:~%")
  (format t "-----------------------------------------~%")
  (format t "   G = CUMULATIVE GALTIC RECORD~%")
  (format t "   S = STATUS REPORT~%")
  (format t "   T = PHOTON TORPEDO DATA~%")
  (format t "   B = STARBASE NAV DATA~%")
  (format t "   N = DIRECTION/DISTANCE CALCULATOR~%")
  (format t "   Z = GALAXY 'REGION NAME' MAP~%"))

(defun computer()
  (let (a)
    (when (<(aref *ddd* 8) 0)
      (format t "COMPUTER DISABLED.~%")
      (return-from computer nil))
    (format t "COMPUTER ACTIVE AND AWAITING COMMAND")
    (setq a (read))
;    (when (or (not(numberp a)) (< a 0) (> a 5))
;      (comp-help)
;      (return-from computer nil))
    (case a
     (g
      (comp-galaxy-rec))
     (s
      (comp-stat-repo))
     ((t)
      (comp-torpedo))
     (b
      (base-nav))
     (n
      (comp-calc))
     (z
      (comp-galaxy-name-map))
     (t (comp-help)))))



(defun comp-galaxy-name-map()
  (format t "                        THE GALAXY~%")
  (format t "       0     1     2     3     4     5     6     7~%")
  (format t "    +-----+-----+-----+-----+-----+-----+-----+-----+")
  (dotimes (i 8)
    (format t "~%")
    (dotimes (j 2)
      (format t " ~20@a"
	      (quad-name (* j 4) i 1)))))

;galax record
(defun comp-galaxy-rec()
 (let (x qqq)
   (format t "       COMPUTER RECORD OF GALAXY FOR QUADRANT ~a,~a~%"
	   *qx* *qy*)
  (format t "       0     1     2     3     4     5     6     7~%")
  (format t "    +-----+-----+-----+-----+-----+-----+-----+-----+")
  (dotimes (i 8)
    (format t "~%  ~a |" i)
    (dotimes (j 8)
      (setq qqq (aref *ggg* i j))
      (cond 
       ((quad-visit qqq)
	(format t " ~1a~1a~1a  "
		(quad-klingon qqq)
		(quad-base qqq)
		(quad-star qqq)))
       (t (format t " ***  ")))))))


;status
(defun comp-stat-repo()
  (format t "   STATUS REPORT:~%   -------------~%")
  (format t " ~a KLINGON~a LEFT.~%" *klingon-total* (if (> *klingon-total* 1) "S" ""))
  (format t " MISSION MUST BE COMPLETED IN ~5,2f STARDATES.~%"
	  (* (/(floor (+ *time0* *t-period* (- *time*))) 10) 10))
  (cond
   ((> *base-total* 0)
    (format t
	    " THE FEDERATION IS MAINTAINING ~a STARBASE~a IN THE GALAXY.~%~%"
	    *base-total*  (if (> *base-total* 1) "S" "")))
   (t
    (format t "YOUR STUPIDITY HAS LEFT YOU ON YOUR OWN IN~%")
    (format t "  THE GALAXY -- YOU HAVE NO STARBASES LEFT!~%~%")))
  (damage-report))


; torpedo cource
(defun comp-torpedo()
  (cond
   ((<= *c-klingons* 0) (noememy))
   (t
    (format t "FROM ENTERPRISE TO KLINGON BATTLE CRUSER~a~%"
	    (if (> *c-klingons* 1) "S" ""))
    (dotimes (i 4)
      (cond
       ((> (klingon-energy (aref *kkk* i)) 0)
	(comp-torpedo1 i)))))))

(defun comp-torpedo1(i)
  (let* (
	(k(aref *kkk* i))
	(kx (klingon-x k))
	(ky (klingon-y k)))
    (format t "KLINGON at (~a,~a): DIRECTION = ~3,2f~%" kx ky
	    (calc-p2p *ex* *ey* kx ky))))


; calculator
(defun comp-calc()
  (let (x0 y0 x1 y1)
  (format t "DIRECTION/DISTANCE CALCULATOR:~%")
  (format t "YOU ARE AT QUADRANT ~a,~a " *qx* *qy*)
  (format t " SECTOR ~a,~a.~%" *ex* *ey*)

  (format t "PLEASE ENTER INITIAL COORDINATES X?")
  (setq x0 (read))
  (format t "Y?")
  (setq y0 (read))
  (format t "FINAL COORDINATES X?")
  (setq x1 (read))
  (format t "Y?")
  (setq y1 (read))
  (disp-direct-dist x0 y0 x1 y1)))

(defun disp-direct-dist (x0 y0 x1 y1)
  (format t "DIRECTION = ~3,2f~%"
	  (calc-p2p x0 y0 x1 y1))
  (pr-distance (- x0 x1)(- y0 y1)))
  
; starbase nav data
(defun base-nav()
  (cond 
   ((= *c-bases* 0)
    (format t
	    "MR. SPOCK:  'SENSORS SHOW NO STARBASES IN THIS QUADRANT.'~%"))
   (t
    (disp-direct-dist *ex* *ey* 
		      *bx* *by*))))


(defun pr-distance(dx dy)
  (format t "DISTANCE =~5,3f~%"(distance-p2p dx dy)))

(defun distance-p2p (dx dy)
  (sqrt (+ (* dx dx) (* dy dy))))

(defun calc-p2p (x0 y0 x1 y1)
  (let (dx dy)
    (setq dx (- x1 x0))
    (setq dy (- y1 y0))
    (cond
     ((and (= dx 0)(= dy 0))
      0)
     ((and (< dx 0)(>= dy 0))
      (if (> (abs dx) (abs dy))
	  (calc0 0 dx dy)
	(calc1 2 dx dy)))
     ((and (>= dx 0)(>= dy 0))
      (if (< (abs dx)(abs dy))
	  (calc1 2 dx dy)
	(calc2 4 dx dy)))
     ((and (>= dx 0)(< dy 0))
      (if (> (abs dx)(abs dy))
	  (calc2 4 dx dy)
	(calc3 6 dx dy)))
     ((and (< dx 0)(< dy 0))
      (if (< (abs dx)(abs dy))
	  (calc3 6 dx dy)
	(calc0 8 dx dy))))))

(defun calc0 (n dx dy)
  (- n (/ dy dx)))

(defun calc1 (n dx dy)
  (+ n (/ dx dy)))

(defun calc2 (n dx dy)
  (calc0 n dx dy))

(defun calc3 (n dx dy)
  (calc1 n dx dy))

    



;;; random move
(defun klingon-rand-move (i)
  (let (x y
	newx newy xxx
	(k  (aref *kkk* i)))
;(format t "i=~a~%" i)
    (setq x (klingon-x k))
    (setq y (klingon-y k))
;(format t "x=~a y=~a~%" x y)
    (loop
     (setq newx (randmove-vec x))
     (setq newy (randmove-vec y))
;(format t "newx=~a newy=~a~%" newx newy)
     (setq xxx (aref *sec* newx newy))
;(format t "xxx=~a~%" xxx )
     (when (or (null xxx) (eql 'k xxx))
       (return)))
    (cond
     ((null xxx)
;(format t "aset2 x=~a y=~a~%" x y )
      (aset2 *sec* x y nil)
      (aset2 *sec* newx newy 'k)
      (setf (klingon-x k) newx)
      (setf (klingon-y k) newy)
      (format t "Klingon at ~a,~a moves to ~a,~a~%" x y newx newy)))))

(defun randmove-vec(x)
  (let (new
	(d (1- (random 3))))
    (setq new (+ x d))
    (if (< new 0) (setq new 0))
    (if (> new 7) (setq new 7))
    new))


;;;
;;; device name
;;;
(defconstant *device-name*
      (make-array
       '9 :initial-contents
       '("" "WARP ENGINES" "SHORT RANGE SENSORS" "LONG RANGE SENSORS"
	 "PHASER CONTROL" "PHOTON TUBES" "DAMAGE CONTROL" "SHIELD CONTROL"
	 "LIBRARY-COMPUTER")))
;f8790
;(defmacro device-name (x) 
;  `(aref  *device-name* ,x))
(defun device-name (x) 
  (aref  *device-name* x))



;;;
;;; quad name
;;;
(defconstant *quad-name1*
      (make-array
       '8 :initial-contents
       '("ANTARES" "RIGEL" "PROCYON" "VEGA"
	 "CANOPUS" "ALTAIR" "SAGITTARIUS" "POLLUX")))
(defconstant *quad-name2*
      (make-array
       '8 :initial-contents
       '("SIRIUS" "DENEB" "CAPELLA" "BETELGEUSE"
	 "ALDEBARAN" "REGULUS" "ARCTURUS" "SPICA")))
(defconstant *quad-sub*
      (make-array
       '4 :initial-contents
       '(" I" " II" " III" " IV")))

;9030
(defun quad-name(z5 z4 g5)
  (concatenate 'string 
	       (quad-name1 z5 z4)
	       (quad-name-sub g5 z5)))

(defun quad-name1(z5 z4)
  (cond ((< z5 4)
	 (aref *quad-name1* z4))
	(t (aref *quad-name2* z4))))

(defun quad-name-sub(g5 z5)
  (if(eql g5 1)
      ""
    (aref *quad-sub* (mod z5 4))))

;;; start game
#+:ALLEGRO-V8.0 (trek)


#| just my memo

WindowsXP
GCL (GNU Common Lisp)  2.6.6 CLtL1    Feb 10 2005 08:19:54
(:COMPILER :NUMLIB :SDEBUG :DEFPACKAGE :GNU-LD :UNEXEC :TRUNCATE_USE_C
    :MINGW32 :I686 :IEEE-FLOATING-POINT :WIN32 :WINNT :GMP :GCL :AKCL
    :COMMON :KCL)
---
WindowsXP
ALLEGRO-V8.0
(format t "~a" *features*)
(ALLEGRO-CL-TRIAL IDE COMMON-GRAPHICS IPV6 ACL-SOCKET HIPER-SOCKET PROFILER MULTIPROCESSING
 FLAVORS LITTLE-ENDIAN GSGC COMPILER USE-STRUCTS-IN-COMPILER CLOS VERIFY-CAR-CDR DYNLOAD DLWIN
 X86 MSWINDOWS MICROSOFT MICROSOFT-32 ENCAPSULATING-EFS RELATIVE-PACKAGE-NAMES MODULE-VERSIONS
 IEEE IEEE-FLOATING-POINT CONFORMING-IEEE ICS COMMON-LISP ANSI-CL DRAFT-ANSI-CL-2 X3J13 ALLEGRO
 EXCL FRANZ-INC ALLEGRO-VERSION>= ALLEGRO-VERSION= NEW-ENVIRONMENTS OS-THREADS PROCESS7
 SYMBOL-VALUE-VECTOR DYNLOAD-ACL-LIBRARY ALLEGRO-V8.0 SSL-SUPPORT)
---
FreeBSD gedanken.axe-inc.co.jp 5.4-RELEASE FreeBSD 5.4-RELEASE #0
GCL (GNU Common Lisp)  2.6.5 CLtL1    Apr  4 2005 11:14:09
*features*
(:COMPILER :NUMLIB :SDEBUG :DEFPACKAGE :GNU-LD :UNEXEC :READLINE
    :TRUNCATE_USE_C :BSD :FREEBSD5.4 :I386 :SGC :IEEE-FLOATING-POINT
    :UNIX :GMP :GCL :AKCL :COMMON :KCL)
---
Darwin takeBook.local 7.9.0 Darwin Kernel Version 7.9.0: Wed Mar 30 20:11:17 PS\T 2005; root:xnu/xnu-517.12.7.obj~1/RELEASE_PPC  Power Macintosh powerpc
CLISP
(:CLOS :LOOP :COMPILER :CLISP :ANSI-CL :COMMON-LISP :LISP=CL :INTERPRETER
 :SOCKETS :GENERIC-STREAMS :LOGICAL-PATHNAMES :SCREEN :GETTEXT :UNICODE
 :BASE-CHAR=CHARACTER :UNIX)
---
FreeBSD gweep.axe-inc.co.jp 2.2.8-STABLE FreeBSD 2.2.8-STABLE #0
GCL (GNU Common Lisp)  Version(2.0) Thu Mar  7 06:44:49 PST 1996
(:COMPILER :NUMLIB :SDEBUG TRUNCATE_USE_C BSD CLX-LITTLE-ENDIAN
    |FreeBSD| 386BSD SGC IEEE-FLOATING-POINT UNIX GCL AKCL COMMON KCL)

|#
;;; EOF