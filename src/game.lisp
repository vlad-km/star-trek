#|
Star Trek
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

From: http://www.takeoka.org/~take/trek/trek.lsp
http://www.takeoka.org/~take/trek/trek-man-e.html

|#


;;; some kludges
;;;
;;; two dimensional array
;;; read
(defun daref (array row col)
    (let* ((dimensions (array-dimensions array))
           (rowsize (cadr dimensions)))
        (setq offset (+ (* row rowsize) col))
        (aref array offset) ))

;;; write
(defun sdref (array row col value)
    (let* ((dimensions (array-dimensions array))
           (rowsize (cadr dimensions)))
        (setq offset (+ (* row rowsize) col))
        (setf (aref array offset) value) ))

;;; numbers truncate
(defconstant *round-base* #(10 10 100 1000 10000))
(defun roundnum (num &optional (base 1))
    (let ((magic (aref *round-base* base)))
        (/ (floor (* num magic)) magic)))

;;; string constructor
(defun strfor (max str)
    (let ((len (length str))
          (need)
          (res))

        (setq need (- max len))
        (if (> need 0)
            (progn
                (dotimes (i need)
                    (push " " res))
                (apply #'concat str res))
            str)))


;;; klingon
(das:structure klingon (x 0) (y 0) (energy 0))


;;; QUAD
(das:structure quad (visit nil) (base 0) (star 0) (klingon 0))


;;; quad name
(defconstant *quad-name1*
  #("ANTARES" "RIGEL" "PROCYON" "VEGA"
    "CANOPUS" "ALTAIR" "SAGITTARIUS" "POLLUX"))

(defconstant *quad-name2*
  #("SIRIUS" "DENEB" "CAPELLA" "BETELGEUSE"
    "ALDEBARAN" "REGULUS" "ARCTURUS" "SPICA"))

(defconstant *quad-sub*
  #(" I" " II" " III" " IV"))

(defun quad-name (z5 z4 g5)
    (concat (quad-name1 z5 z4) (quad-name-sub g5 z5)))


(defun quad-name1 (z5 z4)
    (cond ((< z5 4)
           (aref *quad-name1* z4))
          (t (aref *quad-name2* z4))))


(defun quad-name-sub (g5 z5)
    (if(eql g5 1)
       ""
       (aref *quad-sub* (mod z5 4))))

;;; direction vector. 9th data is same as 1st data
(defparameter *cx* #(-1 -1 0 1 1 1 0 -1 -1))
(defparameter *cy* #(0 1 1 1 0 -1 -1 -1 0))




;;;
;;; device name
;;;

(defconstant *device-name*
  #(""
    "WARP ENGINES" "SHORT RANGE SENSORS" "LONG RANGE SENSORS"
    "PHASER CONTROL" "PHOTON TUBES" "DAMAGE CONTROL" "SHIELD CONTROL"
    "LIBRARY-COMPUTER"))

(defun device-name (x)
    (aref  *device-name* x))



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
    (format *so* "THE USS ENTERPRISE --- NCC-1701~%")
    (format *so* "                  ,------*------,~%" )
    (format *so* "  ,-------------   '---  ------'~%" )
    (format *so* "   '-------- --'      / /~%" )
    (format *so* "       ,---' '-------/ /--,~%" )
    (format *so* "        '----------------'~%" )
    )

(defun init()
    (setq *damage-repair-magic-number* (/ (random 50) 100))
    (setq *success* nil)
    (setq *mission-end* nil)
    (setq *klingon-attack* nil)
    (setq *klingon-org* 0)
    (dotimes (i 4)
        (setf (aref *kkk* i) (make-klingon)))
    (setq *time* (* (+ (random 20) 20) 100)) ;; current time
    (setq *time0* *time*)                    ;; initial time
    (setq *t-period* (+ (random 10) 25))     ;; end time
    (setq *docked* nil)                      ;; docked
    (setq *energy* *full-energy*)
    (setq *torpedo* *full-torpedo*)     ;;No. of torpedoes
    (setq *shield* 0)                   ;;shield
    )

(defun init2()
    (setq *qx* (random 8)) ;;Quad X
    (setq *qy* (random 8)) ;;Quad Y
    (setq *ex* (random 8)) ;;Sector X
    (setq *ey* (random 8)) ;;Sector Y
    (setq *ddd* (make-array 10 :initial-element 0)) ;no Damage
    )


;;;
(defun klingon-distance(i)
    (let* ((k (aref *kkk* i))
           (xx1 (- (klingon-x k) *ex*))
           (xx2 (- (klingon-y k) *ey*)))
        (floor (+ (sqrt (+ (* xx1 xx1) (* xx2 xx2))) 0.5))))


;;;
(defun rnd1-8()
    (1+ (random 8)))

(defun fnrand()
    (random 8))

;;;
;;; MAIN
;;;

(defvar *started* nil)


;;; game control
;;; launch
(defun launching ()
    (unless *started*
        (st-console-init)
        (setq *started* t))
    (stc/clear)
    ;;(stc/stardate *time*)
    (title)
    (trek1))

;;; to land
;;; just hide console
;;; null *started* flag
(defun toland ()
    (setq *started nil)
    (stc/hide))

;;; drop it
(defun trek ()
    (unless *started*
        (st-console-init)
        (setq *started* t))
    (stc/clear)
    ;;(stc/stardate *time*)
    (title)
    (trek1) )


(defun trek1 ()
    (init)
    (init2)
    (make-galaxy)
    (print-mission)
    (accept-message)
    (state :accept-command)
    (values))


;;; make galaxy
(defun make-galaxy ()
    (make-galaxy1)
    (if (> *klingon-total* *t-period*)
        (setq *t-period* (1+ *klingon-total*)))
    (cond ((zerop *base-total*)
           (cond ((< (quad-klingon (daref *ggg* *qx* *qy*)) 2)
                  (incf *klingon-total*)
                  (incf (quad-klingon (daref *ggg* *qx* *qy*)))))
           (setq *base-total* 1)
           (incf (quad-base (daref *ggg* *qx* *qy*)))
           (setq *qx* (fnrand))
           (setq *qy* (fnrand))))
    (setq *klingon-org* *klingon-total*))

(defun make-galaxy1 ()
    (let ((k3)
          (b3)
          (r))
        (setq *base-total* 0)    ;; Base total No.
        (setq *klingon-total* 0) ;; Klingon total No.
        (dotimes (i 8)
            (dotimes (j 8)
                (incf *klingon-total*
                      (setq k3
                            (cond ((> (setq r (random 100)) 98) 3)
                                  ((> r 95) 2)
                                  ((> r  8) 1)
                                  (t 0))))
                (incf *base-total*
                      (setq b3
                            (cond ((> (random 100) 96) 1)
                                  (t 0))))
                (sdref *ggg* i j
                       (make-quad
                        :klingon k3 :base b3 :star (rnd1-8)))))))


;;; mission
(defun print-mission()
    (format *so* "~%YOUR ORDERS ARE AS FOLLOWS:~%")
    (format *so* "--------------------------~%")
    (format *so* "   DESTROY THE ~a KLINGON WARSHIPS WHICH HAVE INVADED~%"
            *klingon-total*)
    (format *so* "   THE GALAXY BEFORE THEY CAN ATTACK FEDERATION HEADQUARTERS~%")
    (format *so* "   ON STARDATE ~d. THIS GIVES YOU ~a DAYS.~%"
            (+ *time0* *t-period*) *t-period* )
    (format *so*
            "   THERE ~a ~a STARBASE~a IN THE GALAXY FOR RESUPPLYING YOUR SHIP.~%~%"
            (if (= *base-total* 1) "IS" "ARE")
            *base-total*
            (if (= *base-total* 1) "" "S")))


;;; state :accept-command
(defun accept-message ()
    (format *so* "~%ARE YOU READY TO ACCEPT COMMAND (Y/N)?~%"))


;;;
(defun help-com ()
    (format *so* "ENTER ONE OF THE FOLLOWING:~%
--------------------------
   W   WARP
   S   FOR SHORT RANGE SENSOR SCAN
   L   FOR LONG RANGE SENSOR SCAN
   P   TO FIRE PHASERS
   T   TO FIRE PHOTON TORPEDOES
   Z   TO RAISE OR LOWER SHIELDS
   R   FOR DAMAGE CONTROL REPORTS
   C   TO CALL ON LIBRARY-COMPUTER
   X   TO RESIGN YOUR COMMAND~%"))


;;;
(defvar *sec*)

(defun init-sector ()
    (let ((x)
          (y)
          (k))
        (setq *sec* (make-array '(8 8) :initial-element nil))
        (sdref *sec* *ex* *ey* 'e)
        (dotimes (i 4)
            (setf (klingon-energy (aref *kkk* i)) 0))
        (dotimes (i *c-klingons*)
            (loop
              (setq x (fnrand))
              (setq y (fnrand))
              (when (not (daref *sec* x y))
                  (setq k (aref *kkk* i))
                  (setf (klingon-x k) x)
                  (setf (klingon-y k) y)
                  (setf (klingon-energy k)
                        (* *klingon-max-energy* (+ 5 (random 10)) 0.1))
                  (sdref *sec* x y 'k)
                  (return))))
        (dotimes (i *c-bases*)
            (loop
              (setq x (fnrand))
              (setq y (fnrand))
              (when (not (daref *sec* x y))
                  (setq *bx* x)
                  (setq *by* y)
                  (sdref *sec* x y 'b)
                  (return))))
        (dotimes (i *c-stars*)
            (loop
              (setq x (fnrand))
              (setq y (fnrand))
              (when (not (daref *sec* x y))
                  (sdref *sec* x y 's)
                  (return))))))


;;; main game loop
;;; read command from console
(defun mloop()
    (stc/stardate *time*)
    ;; cool end mission
    (if (or *success* (<= *klingon-total* 0))
        (return-from mloop (success)))
    ;; fail mission
    (if (> *time* (+ *time0* *t-period*))
        (return-from mloop (fail-mission)))
    ;; end mission
    (when *mission-end*
        (mordev:rx-emit :trek1)
        (return-from mloop))
    ;; klingon attack
    (when *klingon-attack*
        (klingon-attack)
        (setq *klingon-attack* nil))
    ;; fail mission
    (when (not (energy-check))
        (fail-mission)
        (return-from mloop nil))
    (state :mloop-command) )


;;; main game loop
;;; execute command
(defun mloop-command (cmd)
    (case cmd
      ((W)
       ;; warp
       (input-course-message "LT. SULU")
       (state :input-course-check))
      ((S)
       ;; short range radar
       (stc/clear)
       (short-range-sensor))
      ((L)
       ;; long range radar
       (long-range-sensor))
      ((P)
       ;; phaser
       (phaser-message)
       (state :phaser))
      ((T)
       ;; torpedo
       (torpedo-message)
       (state :torpedo-course))
      ((Z)
       ;; shield
       (shield-message)
       (state :shield))
      ((R)
       ;; damage report
       (stc/clear)
       (damage-report))
      ((C)
       ;; computer
       (computer-message)
       (state :computer))
      ((x)
       ;; done
       (end-of-mission))
      (otherwise
       ;; help
       (stc/clear)
       (help-com))))

;;; how many time in warp
(defun warp-time (w1)
    (let ((t8 1))
        (if (< w1 1)
            (setq t8 (/ (floor (* 10 w1)) 10)))
        (incf *time* t8)
        (cond
          ((> *time* (+ *time0* *t-period*))
           (fail-mission))
          (t t))))


;;; input warp/torpedo course
(defvar *new-course* 0)
(defvar *last-name* nil)

;;; input message
(defun input-course-message (man)
    (setq *last-name* man)
    (format *so* "COURSE (0-8, -1)"))

;;; input state
(defun input-course-check (c1)
    (cond
      ((not (numberp c1)) nil)
      ((= c1 -1) nil)
      (t
       (cond
         ((or (< c1 0) (> c1 8))
          (stc/terpri)
          (format *so* "   ~a: INCORRECT COURSE DATA, SIR!~%" *last-name*)
          nil)
         (t
          (if (= c1 8)
              (setq *new-course* 0)
              (setq *new-course* c1))
          t) ))))


;;; warp message
(defun nav-factor-message (x)
    (format *so* "WARP FACTOR (0-~a)" x))

;;; warp state1
(defun nav-factor (w1)
    (let ((wdamage (aref *ddd* 1)))
        (cond
          ((= w1 0)
           nil)
          ((and (< wdamage 0)(> w1 0.2))
           (stc/terpri)
           (format *so* "WARP ENGINES ARE DAMAGED.  MAXIUM SPEED = WARP 0.2~%")
           nil)
          ((or (< w1 0)(> w1 8))
           (stc/terpri)
           (format *so* "CHIEF ENGINEER SCOTT: THE ENGINES WON'T TAKE WARP ~a!~%" w1)
           nil)
          (t w1))))

;;; warp state2
(defun nav-energy (w1)
    (let ((n (floor (+ (* w1 8) 0.5))))
        (cond
          ((< *energy* n)
           (stc/terpri)
           (format *so* "ENGINEERING:   INSUFFICIENT ENERGY AVAILABLE~%")
           (format *so* "               FOR MANEUVERING AT WARP~a!~%" w1)
           (cond
             ((or (< *shield* (- n *energy*)) (< (aref *ddd* 7) 0))
              t)
             (t
              (stc/terpri)
              (format *so* "DEFLECTOR CONTROL ROOM:  ~a UNITS OF ENERGY~%" *shield*)
              (format *so* "                         PRESENTLY DEPLOYED TO SHIELDS~%")))
           t)
          (t n))))


;;; entering quad
(defun enter-quad ()
    (stc/clear)
    (stc/stardate *time*)
    (setf (quad-visit (daref *ggg* *qx* *qy*)) t) ;; make known Quad
    (when (not (or (< *qx* 0) (> *qx* 7) (< *qy* 0) (> *qy* 7)))
        (disp-quad-name (quad-name *qy* *qx* 0))
        (enter-quad1)
        (repo-entering-quad-stat))
    (init-sector)
    (short-range-sensor))


;;;
(defun disp-quad-name(qq)
    (stc/quad qq)
    (cond
      ((= *time0* *time*)
       (format *so* "YOUR MISSION BEGINS WITH YOUR STARSHIP LOCATED~%")
       (format *so* "IN THE GALACTIC QUADRANT ~a~%" qq))
      (t
       (format *so* "NOW ENTERING ~a QUADRANT...~%" qq))))

;;;
(defun repo-entering-quad-stat ()
    (when (/= *c-klingons* 0)
        (format *so* "COMBAT AREA CONDITION RED ~%"))
    (when (<= *shield* 200)
        (format *so* "SHIELDS DANGEROUSLY LOW ~%")))

;;;
(defun enter-quad1 ()
    (let ((g (daref *ggg* *qx* *qy*)))
        (setq *c-klingons* (quad-klingon g))
        (setq *c-bases* (quad-base g))
        (setq *c-stars* (quad-star g))))


;;;
(defun energy-check()
    (if (and (> (+ *shield* *energy*) 10)
             (or (> *energy* 10) (zerop (aref *ddd* 7))))
        (return-from energy-check t))
    (stc/terpri)
    (format *so* "** FATAL ERROR **~%")
    (format *so* "YOU'VE JUST STRANDED YOUR SHIP IN SPACE~%")
    (format *so* "YOU HAVE INSUFFICIENT MANEUVERING ENERGY~%")
    (format *so* "AND SHIELD CONTROL IS PRESENTLY INCAPABLE OF~%")
    (format *so* "CROSS-CIRCUITING TO ENGINE ROOM!!~%")
    nil
    )


(defun klingon-attack-warp()
    (dotimes (i 4)
        (cond
          ((/= 0 (klingon-energy (aref *kkk* i)))
           (klingon-rand-move i))))
    (klingon-attack))


(defun repair-by-warp (w1)
    (let ((flag nil)
          (x)
          (ii))
        (dotimes (i 9)
            (setq ii (1+ i))
            (cond
              ((< (aref *ddd* ii) 0)
               (setq x (incf (aref *ddd* ii)))
               (cond
                 ((>= x 0)
                  (setf (aref *ddd* ii) 0)
                  (cond
                    ((not flag)
                     (stc/terpri)
                     (format *so* "DAMAGE CONTROL REPORT:  ~%")
                     (setq flag t)))
                  (format *so* "~a REPAIR COMPLETED.~%" (device-name ii)))))))))

(defun damage-by-warp()
    (let ((damdev))
        (cond
          ((<= (random 10) 2)
           (stc/terpri)
           (setq damdev (rnd1-8))
           (incf (aref *ddd* damdev)
                 (cond
                   ((< (random 10) 6)
                    (format *so* "DAMAGE CONTROL REPORT:  ~a DAMAGED~%"
                            (device-name damdev))
                    (* -1 (1+ (/ (random 500) 100))))
                   (t
                    (format *so* "DAMAGE CONTROL REPORT:  ~a STATE OF REPAIR IMPROVED~%"
                            (device-name damdev))
                    (1+ (/ (random 300) 100)))))))))

(defun cal-vec (va c1)
    (let ((ci)
          (cr))
        (setq ci(floor c1))
        (setq cr (- c1 ci))
        (+ (aref va ci)
           (* (- (aref va (+ ci 1)) (aref va ci))
              cr))))


(defun nav4 (c1 n w1)
    (let
        ((x *ex*)
         (y *ey*)
         (dx (cal-vec *cx* c1))
         (dy (cal-vec *cy* c1))
         (x0 *ex*)
         (y0 *ey*)
         (old-qx *qx*)
         (old-qy *qy*))
        (stc/clear)
        (stc/stardate *time*)
        (sdref *sec* *ex* *ey* nil)
        (dotimes (i n)
            (incf x dx)
            (incf y dy)
            (setq *ex* (floor (+ x 0.5)))
            (setq *ey* (floor (+ y 0.5)))
            (cond
              ((or (< *ex* 0)(> *ex* 7)
                   (< *ey* 0)(> *ey* 7))
               (return-from nav4
                   (cond ((exit-quad n x0 y0 dx dy w1 old-qx old-qy)
                          (enter-quad) t)
                         (t nil))))
              (t (format *so* "(~a,~a)" *ex* *ey*)))
            (when (daref *sec* *ex* *ey*)
                (stc/terpri)
                (setq *ex* (floor(- x dx)))
                (setq *ey* (floor(- y dy)))
                (format *so* "WARP ENGINES SHUT DOWN AT ")
                (format *so* "SECTOR ~a , ~a DUE TO BAD NAVAGATION" *ex* *ey*)
                (return)))
        (sdref *sec* *ex* *ey* 'E)
        (dec-energy n)
        (stc/terpri)
        (short-range-sensor)
        t))



;;; exit from quad
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
               (stc/terpri)
               (format *so* "LT. UHURA: MESSAGE FROM STARFLEET COMMAND:~%")
               (format *so*
                       "      PERMISSION TO ATTEMPT CROSSING OF GALACTIC PERIMETER~%")
               (format *so* "      IS HEREBY *DENIED*.  SHUT DOWN YOUR ENGINES.~%~%")
               (format *so* "CHIEF ENGINEER SCOTT:  WARP ENGINES SHUT DOWN~%")
               (format *so* "                       AT SECTOR ~a,~a OF QUADRANT ~a,~a~%"
                       *ex* *ey* *qx* *qy*)
               (sdref *sec* *ex* *ey* 'E)
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
        (stc/terpri)
        (format *so* "SHIELD CONTROL SUPPLIES ENERGY TO COMPLETE THE MANEUVER.~%")
        (incf *shield* *energy*)
        (setq *energy* 0)
        (when (<= *shield* 0)
            (setq *shield* 0))))

;;; Long sensor
(defun long-range-sensor()
    (let ((x)
          (y)
          (qqq))
        (cond
          ((< (aref *ddd* 3) 0)
           (format *so* "~%LONG RANGE SENSORS ARE INOPERABLE.~%"))
          (t
           (format *so* "~%LONG RANGE SCAN FOR QUADRANT ~a,~a~%" *qx* *qy*)
           (dotimes (i 3)
               (dotimes (j 3)
                   (setq x (+ *qx* i -1))
                   (setq y (+ *qy* j -1))
                   (cond
                     ((and (>= x 0)(<= x 7)(>= y 0)(<= y 7))
                      (setq qqq (daref *ggg* x y))
                      (setf (quad-visit qqq) t)
                      (format *so* " ~a~a~a"
                              (quad-klingon qqq)
                              (quad-base qqq)
                              (quad-star qqq)))
                     (t (format *so* " ***"))))
               (format *so* "~%"))))))

;;; phaser
;;; no enemy msg
(defun noenemy ()
    (format *so* "~%")
    (format *so* "SCIENCE OFFICER SPOCK:   SENSORS SHOW NO ENEMY SHIPS~%")
    (format *so* "                         IN THIS QUADRANT~%"))

;;; phaser message
(defun phaser-message ()
    (cond
      ((< (aref *ddd* 4) 0)
       (stc/terpri)
       (format *so* "PHASERS INOPERATIVE.~%")
       (return-from phaser-message (values)))
      ((<= *c-klingons* 0)
       (noenemy)
       (return-from phaser-message (values)))
      ((< (aref *ddd* 8) 0)
       (stc/terpri)
       (format *so* "COMPUTER FAILURE HAMPERS ACCURACY.~%"))
      (t
       (stc/terpri)
       (format *so* "PHASERS LOCKED ON TARGET; ~%")))
    (format *so* "ENERGY AVAILABLE = ~a UNITS~%" *energy*)
    (format *so* "NUMBER OF UNITS TO FIRE ?"))

;;; phaser state
(defun phaser4 (x)
    (let ((h)
          (ke)
          (kx)
          (ky)
          (k))
        (decf *energy* x)
        (cond
          ((< (aref *ddd* 8) 0)
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
                  (stc/terpri)
                  (format *so* "SENSORS SHOW NO DAMAGE TO ENEMY AT ~a,~a~%"
                          kx ky))
                 (t
                  (setq ke (decf (klingon-energy k) h))
                  (format *so* "~a UNIT HIT ON KLINGON AT SECTOR ~a,~a~%" h kx ky)
                  (cond
                    ((<= ke 0)
                     (delete-klingon i))
                    (t
                     (format *so* "SENSORS SHOW ~d UNITS REMAINING~%" ke))))))))))


;;; drop star
(defun delete-star (x y)
    (sdref *sec* x y nil)
    (decf (quad-star (daref *ggg* *qx* *qy*))))

;;; drop klingon
(defun delete-klingon(i)
    (let ((kx)
          (ky)
          (k))
        (setq k (aref *kkk* i))
        (setq kx (klingon-x k))
        (setq ky (klingon-y k))
        (format *so* "*** KLINGON DESTROYED ***~%")
        (decf *c-klingons*)
        (decf *klingon-total*)
        (sdref *sec* kx ky nil)
        (setf (klingon-energy k) 0)
        (decf (quad-klingon (daref *ggg* *qx* *qy*)))))

(defun delete-klingon-xy (x y)
    (let ((k))
        (dotimes (i 4)
            (setq k (aref *kkk* i))
            (cond
              ((and
                (/= (klingon-energy k) 0)
                (= (klingon-x k) x)
                (= (klingon-y k) y))
               (delete-klingon i)
               (return-from delete-klingon-xy t))
              (t nil)))))

;;; drop base
(defun delete-base (x y)
    (decf *c-bases*)
    (decf *base-total*)
    (sdref *sec* x y nil)
    (decf (quad-base (daref *ggg* *qx* *qy*))))



;;; torpedo
(defun torpedo-message ()
    (cond
      ((<= *torpedo* 0)
       (format *so* "~%ALL PHOTON TORPEDOES EXPENDED.~%"))
      ((< (aref *ddd* 5) 0)
       (format *so* "~%PHOTON TUBES ARE NOT OPERATIONAL.~%"))
      (t
       (format *so* "PHOTON TORPEDO ")))
    (input-course-message "ENSIGN CHEKOV"))



;;; torpedo fire state
(defun torpedo-fire (course)
    (let ((repeat t)
          (x *ex*)
          (y *ey*)
          (obj))
        (setq x1 (cal-vec *cx* course))
        (setq y1 (cal-vec *cy* course))
        (format *so* "TORPEDO TRACK: ")
        (while repeat
            (incf x x1)
            (incf y y1)
            (setq x3 (floor (+ x 0.5)))
            (setq y3 (floor (+ y 0.5)))
            (setq obj (daref *sec* x3 y3))
            (cond
              ((or (< x3 0) (> x3 7) (< y3 0)(> y3 7))
               (format *so* "~%TORPEDO MISSED.~%")
               (return-from torpedo-fire (values)))
              ((eql obj 'k)
               (format *so* "~%")
               (delete-klingon-xy x3 y3)
               (when (<= *klingon-total* 0)
                   (setq *success* t))
               (return-from torpedo-fire (values)))
              ((eql obj 's)
               (format *so* "~%STAR AT ~a, ~a ABSORBED TORPEDO ENERGY.~%" x3 y3)
               (delete-star x3 y3)
               (return-from torpedo-fire (values)))
              ((eql obj 'b)
               (format *so* "~%*** STARBASE DESTROYED ***~%")
               (delete-base x3 y3)
               (destroy-base)
               (return-from torpedo-fire (values))))
            (format *so* "(~a,~a)" x3 y3))))


(defun destroy-base ()
    (cond
      ((or (> *base-total* 0) (> *klingon-total* (- *time* *time0* *t-period*)))
       (format *so* "~%STARFLEET COMMAND REVIEWING YOUR RECORD TO CONSIDER~%")
       (format *so* "COURT MARTIAL!~%")
       (setq *docked* nil)
       (setq *klingon-attack* t))
      (t
       (format *so* "~%THAT DOES IT, CAPTAIN!!  YOU ARE HEREBY RELIEVED OF COMMAND~%")
       (format *so* "AND SENTENCED TO 99 STARDATES AT HARD LABOR ON CYGNUS 12!!~%")
       (end-of-mission))))


;;; shield
;;; shield message
(defun shield-message ()
    (if (< (aref *ddd* 7) 0)
        (progn
            (format *so* "~%SHIELD CONTROL INOPERABLE.~%")
            (state :mloop-command))
        (format *so* "~%ENERGY AVAILABLE =~a.  NUMBER OF UNITS TO SHIELDS ?"
                (+ *energy* *shield*))))


;;; shield state
(defun shield (x)
    (cond
      ((or (< x 0) (= x *shield*))
       (format *so* "SHIELDS UNCHANGED~%"))
      ((> x (+ *energy* *shield*))
       (format *so* "~%SHIELD CONTROL:  THIS IS NOT THE FEDERATION TREASURY~%" )
       (format *so* "SHIELDS UNCHANGED~%"))
      (t
       (incf *energy* (- *shield* x))
       (setq *shield* x)
       (format *so* "DEFLECTOR CONTROL ROOM:~%")
       (format *so* "SHIELDS NOW AT ~a UNITS PER YOUR COMMAND~%" *shield*))))


;;; damage report
(defun damage-report()
    (cond
      ((< (aref *ddd* 6) 0)
       (format *so* "~%DAMAGE CONTROL REPORT NOT AVAILABLE.~%"))
      (t
       (show-stat-repair)))
    (docked-repair))

(defun show-stat-repair()
    (format *so* "DEVICE             STATE OF REPAIR~%")
    (format *so* "------             ---------------~%")
    (dotimes (i 8)
        (format *so* "~a ~d~%" (strfor 19 (device-name (1+ i)))
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
             (t (need-repair-message d3)))))))


;;; readfn
(defvar *d3-repair*)

(defun need-repair-message (d3)
    (incf d3 *damage-repair-magic-number*)
    (setq d3 (if (>= d3 1)  0.9  d3))
    (format *so* "TECHNICIANS STANDING BY TO EFFECT REPAIRS TO YOUR SHIP~%")
    (format *so* "ESTIMATED TIME TO REPAIR: ~d STARDATES~d~%"
            (* .01 (floor (* 100 D3))))
    (format *so* "WILL YOU AUTHORIZE THE REPAIR ORDER (Y/N) ?:")
    (setq *d3-repair* d3)
    (state :need-repair))


(defun repair-all()
    (dotimes (i 8)
        (cond((<  (aref *ddd* (1+ i)) 0)
              (aset *ddd* (1+ i) 0)))))


;;; klingon attack
(defun klingon-attack ()
    (cond
      ((<= *c-klingons* 0) t)
      (*docked*
       (format *so* "~%STARBASE SHIELDS PROTECT THE ENTERPRISE.~%")
       t)
      (t
       (dotimes (i 4)
           (let* ((k (aref *kkk* i))(ke (klingon-energy k)) h)
               (when (> ke 0)
                   (setq h (floor (*(/ ke (klingon-distance i))
                                    (+ 2 (/ (random 10) 10)))))
                   (decf *shield* h)
                   (setf (klingon-energy k)(/ ke  (+ 3 (/ (random 10) 10))))
                   (format *so* "~a UNIT HIT ON ENTERPRISE FROM SECTOR ~a,~a ~%"
                           h (klingon-x k)(klingon-y k))
                   (cond
                     ((<= *shield* 0)
                      (enterprise-destroyed)
                      (return-from klingon-attack))
                     (t
                      (format *so*
                              "SHIELDS DOWN TO ~a UNITS~%" *shield* )
                      (when(>= h 20)
                          (when (and (<= (random 10) 6) (> (/ h *shield*) 0.02))
                              (setq r1 (rnd1-8))
                              (decf (aref *ddd* r1) (+ (/ h *shield*) (/ (random 50) 100)))
                              (format *so* "DAMAGE CONTROL: ~a DAMAGED BY THE HIT~%"
                                      (device-name r1))))))))))))

;;; Fail 1 energy==0 or timeout
(defun fail-mission()
    (format *so* "~%IT IS STARDATE ~d.~%" *time*)
    (end-of-mission ))

;;; Fail destroyed
(defun enterprise-destroyed ()
    (format *so* "~%THE ENTERPRISE HAS BEEN DESTROYED.~%")
    (format *so* "  THE FEDERATION WILL BE CONQUERED.~%")
    (fail-mission))

;;; end of mission
(defun end-of-mission()
    (format *so* "~%THERE WERE ~a KLINGON BATTLE CRUISERS LEFT AT~%" *klingon-total*)
    (format *so* "THE END OF YOUR MISSION.~%")
    (setq *mission-end* t))

;;; more-mission
(defun more-mission-message ()
    (cond
      ((/= *base-total* 0)
       (format *so* "~%THE FEDERATION IS IN NEED OF A NEW STARSHIP COMMANDER~%")
       (format *so* "FOR A SIMILAR MISSION -- IF THERE IS A VOLUNTEER,~%")
       (format *so* "LET HIM STEP FORWARD AND ENTER 'AYE'" )
       (state :more-mission) )))

;;; success
;;;
(defun success ()
    (let ((x (/ *klingon-org* (- *time* *time0*))))
        (format *so* "CONGRATULATIONS, CAPTAIN!  THE LAST KLINGON BATTLE CRUISER~%")
        (format *so* "MENACING THE FEDERATION, HAS BEEN DESTROYED.~%~%")
        (format *so* "YOUR EFFICIENCY RATING IS ~s"
                (* x x 1000))))

;;; DOCKED
(defun dockedp()
    (let ((x)
          (y))
        (dotimes (i 3)
            (dotimes (j 3)
                (setq x (+ *ex* i -1))
                (setq y (+ *ey* j -1))
                (when (and (>= x 0)(<= x 7)(>= y 0)(<= y 7)(eql 'b (daref *sec* x y)))
                    (setq *condi* "DOCKED")
                    (setq *docked* t)
                    (setq *energy* *full-energy*)
                    (setq *torpedo* *full-torpedo*)
                    (setq *shield* 0)
                    (format *so* "SHIELDS DROPPED FOR DOCKING PURPOSES.~%")
                    (return-from dockedp t))))
        (setq *docked* nil)
        nil))

(defun set-condition ()
    (cond ((not (dockedp))
           (cond
             ((> *c-klingons* 0) (setq *condi* "*RED*"))
             ((< *energy* (/ *full-energy* 10)) (setq *condi* "YELLOW"))
             (t  (setq *condi* "GREEN"))))))

;;;
;;; short range sensor
;;;(defmacro srs() (short-range-sensor))

(defun short-range-sensor ()
    (let ((fff))
        (set-condition)
        (when (< (aref *ddd* 2) 0)
            (format *so* "*** SHORT RANGE SENSORS ARE OUT ***~%")
            (return-from short-range-sensor nil))
        (format *so* "   +0-1-2-3-4-5-6-7-+")
        (setq fff  *disp-info-funcs*)
        (dotimes (i 8)
            (format *so* "~%  ~a|" i)
            (dotimes (j 8)
                (format *so* "~a "
                        (case  (daref *sec* i j)
                          ((s) "*")
                          ((k) "K")
                          ((b) "B")
                          ((e) "E")
                          (t   "."))))
            (format *so* "|")
            (apply (pop fff) nil)
            )
        (format *so* "~%")))


(defconstant
    *disp-info-funcs*
  (list
   (lambda()
       (format *so* "        STARDATE           ~d" (roundnum *time* 4) ))
   (lambda()
       (format *so* "        CONDITION          ~a" *condi*))
   (lambda()
       (format *so* "        QUADRANT           ~a ~a" *qx* *qy*))
   (lambda()
       (format *so* "        SECTOR             ~a ~a" *ex* *ey*))
   (lambda()
       (format *so* "        PHOTON TORPEDOES   ~a" *torpedo*))
   (lambda()
       (format *so* "        TOTAL ENERGY       ~d" (+ *energy* *shield*)))
   (lambda()
       (format *so* "        SHIELDS            ~d" *shield*))
   (lambda()
       (format *so* "        KLINGONS REMAINING ~a" *klingon-total*))))

;;;
;;; COMPUTER
;;;
(defun comp-help ()
    (format *so* "FUNCTIONS AVAILABLE FROM LIBRARY-COMPUTER:~%")
    (format *so* "-----------------------------------------~%")
    (format *so* "   G   CUMULATIVE GALTIC RECORD~%")
    (format *so* "   S   STATUS REPORT~%")
    (format *so* "   T   PHOTON TORPEDO DATA~%")
    (format *so* "   B   STARBASE NAV DATA~%")
    (format *so* "   N   DIRECTION/DISTANCE CALCULATOR~%")
    (format *so* "   Z  GALAXY 'REGION NAME' MAP~%"))


;;;
(defun computer-message ()
    (if (< (aref *ddd* 8) 0)
        (format *so* "~%COMPUTER DISABLED.~%")
        (format *so* "~%COMPUTER ACTIVE AND AWAITING COMMAND~%")))


;;;
(defun computer (a)
    (case a
      ((g)
       (stc/clear)
       (comp-galaxy-rec)
       (state :mloop-command))
      ((s)
       (stc/clear)
       (comp-stat-repo)
       (state :mloop-command))
      ((t)
       (comp-torpedo)
       (state :mloop-command))
      ((b)
       (base-nav)
       (state :mloop-command))
      ((n)
       (comp-calc-message)
       (state :comp-calc) )
      ((z)
       (stc/clear)
       (comp-galaxy-name-map)
       (state :mloop-command))
      (otherwise
       (stc/clear)
       (comp-help))))



(defun comp-galaxy-name-map()
    (format *so* "                        THE GALAXY~%")
    (format *so* "       0     1     2     3     4     5     6     7~%")
    (format *so* "    +-----+-----+-----+-----+-----+-----+-----+-----+")
    (dotimes (i 8)
        (format *so* "~%")
        (dotimes (j 2)
            (format *so* " ~a"
                    (quad-name (* j 4) i 1)))))

;;;galax record
(defun comp-galaxy-rec()
    (let ((x)
          (qqq))
        (format *so* "       COMPUTER RECORD OF GALAXY FOR QUADRANT ~a,~a~%"
                *qx* *qy*)
        (format *so* "       0     1     2     3     4     5     6     7~%")
        (format *so* "    +-----+-----+-----+-----+-----+-----+-----+-----+")
        (dotimes (i 8)
            (format *so* "~%  ~a |" i)
            (dotimes (j 8)
                (setq qqq (daref *ggg* i j))
                (cond
                  ((quad-visit qqq)
                   (format *so* " ~a~a~a  "
                           (quad-klingon qqq)
                           (quad-base qqq)
                           (quad-star qqq)))
                  (t (format *so* " ***  ")))))
        (stc/terpri) ))


;;; status report
;;;
(defun comp-stat-repo()
    (format *so* "   STATUS REPORT:~%   -------------~%")
    (format *so* " ~a KLINGON~a LEFT.~%" *klingon-total* (if (> *klingon-total* 1) "S" ""))
    (format *so* " MISSION MUST BE COMPLETED IN ~d STARDATES.~%"
            (* (/(floor (+ *time0* *t-period* (- *time*))) 10) 10))
    (cond
      ((> *base-total* 0)
       (format *so*
               " THE FEDERATION IS MAINTAINING ~a STARBASE~a IN THE GALAXY.~%~%"
               *base-total*  (if (> *base-total* 1) "S" "")))
      (t
       (format *so* "YOUR STUPIDITY HAS LEFT YOU ON YOUR OWN IN~%")
       (format *so* "  THE GALAXY -- YOU HAVE NO STARBASES LEFT!~%~%")))
    (damage-report))


;;; torpedo cource
;;;
(defun comp-torpedo()
    (cond
      ((<= *c-klingons* 0) (noenemy))
      (t
       (format *so* "FROM ENTERPRISE TO KLINGON BATTLE CRUSER~a~%"
               (if (> *c-klingons* 1) "S" ""))
       (dotimes (i 4)
           (cond
             ((> (klingon-energy (aref *kkk* i)) 0)
              (comp-torpedo1 i)))))))

(defun comp-torpedo1(i)
    (let* ((k (aref *kkk* i))
           (kx (klingon-x k))
           (ky (klingon-y k)))
        (format *so* "KLINGON at (~a,~a): DIRECTION = ~d~%" kx ky
                (roundnum (calc-p2p *ex* *ey* kx ky) 3))))



;;; calculator
;;;

(defun comp-calc-message()
    (format *so* "DIRECTION/DISTANCE CALCULATOR:~%")
    (format *so* "YOU ARE AT QUADRANT ~a,~a " *qx* *qy*)
    (format *so* " SECTOR ~a,~a.~%" *ex* *ey*)
    (format *so* "PLEASE ENTER INITIAL COORDINATES X Y?"))


(defun disp-direct-dist (x0 y0 x1 y1)
    (format *so* "DIRECTION = ~d "
            (roundnum (calc-p2p x0 y0 x1 y1) 3))
    (pr-distance (- x0 x1)(- y0 y1)))


;;; starbase nav data
;;;
(defun base-nav()
    (cond
      ((= *c-bases* 0)
       (format *so*
               "MR. SPOCK:  SENSORS SHOW NO STARBASES IN THIS QUADRANT.~%"))
      (t
       (disp-direct-dist *ex* *ey*
                         *bx* *by*))))


(defun pr-distance(dx dy)
    (format *so* "DISTANCE =~d~%" (roundnum (distance-p2p dx dy) 3)))

(defun distance-p2p (dx dy)
    (sqrt (+ (* dx dx) (* dy dy))))

(defun calc-p2p (x0 y0 x1 y1)
    (let ((dx)
          (dy))
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
    (let ((repeat t)
          (x)
          (y)
          (newx)
          (newy)
          (xxx)
          (k  (aref *kkk* i)))
        (setq x (klingon-x k))
        (setq y (klingon-y k))
        (while repeat
            (setq newx (randmove-vec x))
            (setq newy (randmove-vec y))
            (setq xxx (daref *sec* newx newy))
            (when (or (null xxx) (eql 'k xxx))
                (setq repeat nil)))
        (cond
          ((null xxx)
           (sdref *sec* x y nil)
           (sdref *sec* newx newy 'k)
           (setf (klingon-x k) newx)
           (setf (klingon-y k) newy)
           (format *so* "KLINGON AT ~a,~a MOVES TO ~a,~a~%" x y newx newy)))))

(defun randmove-vec (x)
    (let ((_new)
          (d (1- (random 3))))
        (setq _new (+ x d))
        (if (< _new 0) (setq _new 0))
        (if (> _new 7) (setq _new 7))
        _new))


;;; EOF
