;;; -*- mode:lisp; coding:utf-8 -*-

#|

            /\___/\
            )     (
           =\     /=                  if this code is not work, i dont know who wrote this code
             )   (                    Copyright © 2017,2018,2023  @vlad-km
            /     \                   2017, Original https://github.com/vlad-km/moren-electron
            )     (                   2023, Code redesign
           /       \                  Electron >= electron@21.2.2
           \       /                  JSCL Moren edition 
      jgs   \__ __/                   Node.js
               ))
              //
             ((
              \)
|#


#|
                           Star Trek

JSCL Moren Edition port of the Start Trek game from Common Lisp port from http://www.takeoka.org/~take/trek/trek.lsp, 
rewritter by Shozo TAKEOKA (take@axe-inc.co.jp) (2006,2007)

original BASIC version by Mike Mayfield's, (1972), http://www.dunnington.u-net.com/public/startrek/startrek.txt
revision original code (1973) by Terry Newton http://newton.freehostia.com/hp/bas/TREKPT.txt

|#

(defmacro display (fmt-str &body args)
  `(format *so* ,fmt-str ,@args))

(defmacro @clt (label &body args)
  `(#j:console:log
    (format nil
            "   ~a : state: ~a  data: ~a"
            ,label
            *state-fsm*
            (mapcar (lambda (x) x) (list ,@args)))))

;;;

(defmacro %def-pgm (index &body body)
  `(cons ,index (lambda (&rest data) ,@body)))

(defvar *pgm-abend* (lambda (&optional x) (error "ABEND")))

(defun %exec-pgm (pgm index &optional (unknow-index *pgm-abend*))
  (let (f)
    (setq fn (assoc index pgm :test 'eq))
    (if fn (cdr fn) unknow-index)))


;;; some kludge for two dimensional array (not implemented in JSCL)
;;; reader
(defun daref (array row col)
  (let* ((dimensions (array-dimensions array))
         (rowsize (cadr dimensions)))
    (setq offset (+ (* row rowsize) col))
    (aref array offset) ))

;;; writer
(defun sdref (array row col value)
  (let* ((dimensions (array-dimensions array))
         (rowsize (cadr dimensions)))
    (setq offset (+ (* row rowsize) col))
    (setf (aref array offset) value) ))

;;; the function is about nothing
;;; with this generation of pseudo-random date
;;; you can just give the number back without checking
(defconstant *round-base* #(10 10 100 1000 10000))

(defun roundnum (num &optional (base 1))
  (let ((magic (aref *round-base* base)))
    (/ (floor (* num magic)) magic)))



;;; pads this string with a given string (repeated, if needed)
;;; so that the resulting string reaches a given length.
;;; the padding is applied from the end of this string.

#+nil
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

(defun strfor (max str)
  (ffi:|String| str "padEnd" max))

;;; klingon
#+nil (das:structure klingon (x 0) (y 0) (energy 0))
(defstruct (klingon (:type vector) :named) (x 0) (y 0) (energy 0))


;;; QUAD
#+nil (das:structure quad (visit nil) (base 0) (star 0) (klingon 0))
(defstruct (quad  (:type vector) :named) (visit nil) (base 0) (star 0) (klingon 0))


;;; quad name
(defconstant *quad-name1*
  #("ANTARES" "RIGEL" "PROCYON" "VEGA"
    "CANOPUS" "ALTAIR" "SAGITTARIUS" "POLLUX"))

(defconstant *quad-name2*
  #("SIRIUS" "DENEB" "CAPELLA" "BETELGEUSE"
    "ALDEBARAN" "REGULUS" "ARCTURUS" "SPICA"))

(defconstant *quad-sub*
  #(" I" " II" " III" " IV"))

;;; quad accessors
(defun quad-name (z5 z4 g5)
  (jscl::concat (quad-name1 z5 z4) (quad-name-sub g5 z5)))

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


;;; Starship devices

(defconstant *device-name*
  #(""
    "WARP ENGINES" "SHORT RANGE SENSORS" "LONG RANGE SENSORS"
    "PHASER CONTROL" "PHOTON TUBES" "DAMAGE CONTROL" "SHIELD CONTROL"
    "LIBRARY-COMPUTER"))

(defun device-name (x)
  (aref  *device-name* x))



;;;Constants
(defconstant *full-energy* 3000)
(defconstant *full-torpedo* 10) 
(defconstant *klingon-max-energy* 200)


;;; Global vars
(defvar *ggg* (make-array '(8 8) :initial-element 0))
(defvar *kkk* (make-array '(4) :initial-element 0))
(defvar *time* 0)          ;; timers
(defvar *time0* 0)
(defvar *t-period* 0)
(defvar *base-total* 0)    ;; Base total No.
(defvar *klingon-total* 0) ;; Klingon total No.
(defvar *c-klingons* 0)    ;; current-Klingons
(defvar *c-bases* 0)       ;; current-Bases
(defvar *c-stars* 0)       ;; current-Stars
(defvar *bx* 0)            ;; Base-X pos in Quad
(defvar *by* 0)            ;; Base-Y pos in Quad
(defvar *ex* 0)            ;; Enterprise X pos in Quad
(defvar *ey* 0)            ;; Y pos in Quad
(defvar *qx* 0)            ;; Quadrant X
(defvar *qy* 0)            ;; Quadrant Y
(defvar *energy* 0)        ;; Energy level
(defvar *torpedo* 0)       ;; No. of torpedoes
(defvar *shield* 0)        ;; shield
(defvar *docked* nil)      ;; docked
(defvar *condi* "DOCED")   ;; condition: doced/red/yellow/green
(defvar *klingon-attack* nil)  ;; turn of Klingon
(defvar *success* nil)         ;; success flag
(defvar *mission-end* nil)     ;; mission terminated
(defvar *started* nil)
(defvar *new-course* nil)
(defvar *new-factor* nil)


#+nil
(defun title ()
  (display "THE USS ENTERPRISE --- NCC-1701~%")
  (display "                  +------*-------,~%" )
  (display "  ,--------------/------. .------'~%" )
  (display "  '--------+ +--.      / /~%" )
  (display "       ,---+ +--------/ /--,~%" )
  (display "       '-------------------'~%" )
  )

(defun title ()
  (display "THE USS ENTERPRISE --- NCC-1701~%")
  (display "                   +------*-------,~%" )
  (display "  ,--------------,  `---. .-------'~%" )
  (display "  '--------+ +--+      / /~%" )
  (display "       ,---+ +--------+ +--,~%" )
  (display "       '-------------------'~%" )
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
    (setq *torpedo* *full-torpedo*)          ;; No. of torpedoes
    (setq *shield* 0)                        ;; shield
    )

(defun init2()
    (setq *qx* (random 8)) ;; Quad X
    (setq *qy* (random 8)) ;; Quad Y
    (setq *ex* (random 8)) ;; Sector X
    (setq *ey* (random 8)) ;; Sector Y
    (setq *ddd* (make-array 10 :initial-element 0)) ;; no Damage
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

;;; MAIN

;;; game control

;;; start game
(defun launching ()
  (unless *started*
    (console-init)
    (setq *started* t))
  (stc/clear)
  ;;(stc/stardate *time*)
  (title)
  (trek1))

;;; to land. just hide console, nulled *started* flag
(defun toland ()
  (setq *started nil)
  (stc/hide))

;;; note: unused func, see trek1
(defun trek ()
  (unless *started*
    (st-console-init)
    (setq *started* t))
  (stc/clear)
  ;;(stc/stardate *time*)
  (title)
  (trek1))

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
  (let (k3 b3 r)
    (setq *base-total* 0)        ;; Base total No.
    (setq *klingon-total* 0)     ;; Klingon total No.
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
               (make-quad :klingon k3 :base b3 :star (rnd1-8)))))))


;;; new mission display
(defun print-mission()
  (display "~%YOUR ORDERS ARE AS FOLLOWS:~%")
  (display "--------------------------~%")
  (display "   DESTROY THE ~a KLINGON WARSHIPS WHICH HAVE INVADED~%"
          *klingon-total*)
  (display "   THE GALAXY BEFORE THEY CAN ATTACK FEDERATION HEADQUARTERS~%")
  (display "   ON STARDATE ~d. THIS GIVES YOU ~a DAYS.~%"
          (+ *time0* *t-period*) *t-period* )
  (display
          "   THERE ~a ~a STARBASE~a IN THE GALAXY FOR RESUPPLYING YOUR SHIP.~%~%"
          (if (= *base-total* 1) "IS" "ARE")
          *base-total*
          (if (= *base-total* 1) "" "S")))


;;; state :accept-command display prompt
(defun accept-message ()
  (display "~%ARE YOU READY TO ACCEPT COMMAND (Y/N)?~%"))


;;; help display
(defun help-com ()
  (display "ENTER ONE OF THE FOLLOWING:~%
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

(defvar *sec*) ;; galaxy sector

(defun init-sector ()
  (let (x y k)
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


;;; main game loop. read command from console, handle it 
(defun mloop()
  (@clt "MLOOP")
  (stc/stardate *time*)
  ;; cool end mission
  (if (or *success* (<= *klingon-total* 0))
      (return-from mloop (success)))
  ;; fail mission
  (if (> *time* (+ *time0* *t-period*))
      (return-from mloop (fail-mission)))
  ;; end mission
  ;; todo: rewrite this, wrong
  (when *mission-end*
    (rx:emit :trek1)
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


;;; main game loop. execute entered command
#+nil
(defun mloop-command (cmd)
  (@clt "MLOOP-COMMAND" cmd (consp cmd) (symbolp cmd))
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
    ((M)
     ;; manual
     (operational-manual))
    ((x)
     ;; done
     (end-of-mission))
    (otherwise
     ;; help
     (stc/clear)
     (help-com))))

#+nil
(defun mloop-command (data)
  (let ((cmd (car data))
        (args (rest data)))
    (@clt "MLOOP-COMMAND" data cmd args)
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
       (cond (args (computer (car args)))
             (t  (computer-message)
                 (state :computer))))
      ((M)
       ;; manual
       (operational-manual))
      ((x)
       ;; done
       (end-of-mission))
      (otherwise
       ;; help
       (stc/clear)
       (help-com)))))

#+nil
(defun mloop-command (data)
  (let ((cmd (car data))
        (args (rest data)))
    (@clt "MLOOP-COMMAND" data cmd args)
    (case cmd
      ((W) ;; warp
       (input-course-message "LT. SULU")
       (state :input-course-check))
      ((S) ;; short range radar
       (stc/clear)
       (short-range-sensor))
      ((L) ;; long range radar
       (long-range-sensor))
      ((P) ;; phaser
       (phaser-message)
       (state :phaser))
      ((T) ;; torpedo
       (cond (args (let ((course (input-course-check(car args))))
                     ;; enter command t course
                     ;; may be this code move to FSM:torpedo-fair
                     (when course
                       (decf *energy* 2)
                       (decf *torpedo* 1)
                       (torpedo-fire *new-course*)
                       (setq *klingon-attack* t))
                     (state :mloop-command)))                     
             (t (torpedo-message)
                ;; enter command t
                (state :torpedo-course))))
      ((Z) ;; shield
       (shield-message)
       (state :shield))
      ((R) ;; damage report
       (stc/clear)
       (damage-report))
      ((C) ;; computer
       (cond (args (computer (car args)))
             (t  (computer-message)
                 (state :computer))))
      ((M) ;; manual
       (operational-manual))
      ((x) ;; done
       (end-of-mission))
      (otherwise
       ;; help
       (stc/clear)
       (help-com)))))


(defun mloop-command (data)
  (let ((cmd (car data))
        (args (rest data)))
    (@clt "MLOOP-COMMAND" data cmd args)
    (case cmd
      ((W) ;; warp => w direction factor
       (let ((part (length args)))
         (@clt "W-MLOOP" part)
         (cond ((= part 0)
                ;; only command prefix
                (input-course-message "LT. SULU")
                (state :input-course-check))
               #+nil ((= part 1)
                ;; partial warp command direction
                (setq *с1* (input-course-check (car args)))
                (when *c1*
                  (@clt "W-LOOP-part-1" *c1* *new-course*)
                  (nav-factor-message (if (< (aref *ddd* 1) 0) 0.2 8))
                  (state :nav-factor))
                (return-from mloop-command (values)))
               ((= part 2)
                ;; full warp command direction factor
                (let ((w-direction (first args))
                      (w-factor (second args)))
                  (setq *c1* (input-course-check w-direction))
                  (@clt " W partial " part *c1* *new-course*)
                  (cond  (*c1*
                          (setq *w1* (nav-factor w-factor))
                          (when *w1*
                            (display "Wfull nav-factor: ~a~%" *w1*)
                            (setq *n* (nav-energy *w1*))
                            (unless *n* (return-from mloop-command (values)))
                            (klingon-attack-warp)
                            (repair-by-warp *w1*)
                            (damage-by-warp)
                            (when (not (nav4 *new-course* *n* *w1*))
                              (return-from mloop-command (values)))
                            (warp-time *w1*))
                          (return-from mloop-command (values)))
                         (t
                          ;; wtf?
                          (input-course-message "LT. SULU")
                          (state :input-course-check)
                          )))))))
      ((S) ;; short range radar
       (stc/clear)
       (short-range-sensor))
      ((L) ;; long range radar
       (long-range-sensor))
      ((P) ;; phaser
       (phaser-message)
       (state :phaser))
      ((T) ;; torpedo
       (cond (args (let ((course (input-course-check(car args))))
                     ;; enter command t course
                     ;; may be this code move to FSM:torpedo-fair
                     (when course
                       (decf *energy* 2)
                       (decf *torpedo* 1)
                       (torpedo-fire *new-course*)
                       (setq *klingon-attack* t))
                     (state :mloop-command)))                     
             (t (torpedo-message)
                ;; enter command t
                (state :torpedo-course))))
      ((Z) ;; shield
       (shield-message)
       (state :shield))
      ((R) ;; damage report
       (stc/clear)
       (damage-report))
      ((C) ;; computer
       (cond (args (computer (car args)))
             (t  (computer-message)
                 (state :computer))))
      ((M) ;; manual
       (operational-manual))
      ((x) ;; done
       (end-of-mission))
      (otherwise
       ;; help
       (stc/clear)
       (help-com)))))


#|

;;; MLOOP begin

;;; warp command wrapper

(defun w-single-cmd ()
  ;; entire only command prefix
  (input-course-message "LT. SULU")
  (state :input-course-check))

(defun w-full-cmd (direction factor)
  ;; entire full 'W': direction factor
  (setq *c1* (input-course-check direction))
  ;;(@clt " W partial " part *c1* *new-course*)
  (cond  (*c1*
          (setq *w1* (nav-factor factor))
          (when *w1*
            (setq *n* (nav-energy *w1*))
            (unless *n* (return-from w-full-cmd (values)))
            (klingon-attack-warp)
            (repair-by-warp *w1*)
            (damage-by-warp)
            (when (not (nav4 *new-course* *n* *w1*))
              (return-from w-full-cmd (values)))
            (warp-time *w1*)))
         (t (return-from w-full-cmd (values)))))

(defun warp-handler (data)
  (let ((part (length data)))
    (cond ((= part 0) (w-single-cmd))
          ((= part 2) (w-full-cmd (first data) (second data)))
          (t (w-single-cmd)))))

(defvar *loop-pgm
  (list
   ;; warp 
   (%def-pgm 'w  (warp-handler data))
   ;; short sensor
   (%def-pgm 's  (stc/clear)(short-range-sensor))
   ;; long sensor
   (%def-pgm 'l  (long-range-sensor))
   ;; phaser
   (%def-pgm 'p  (phaser-message)(state :phaser))
   ;; torpedo
   (%def-pgm 't
             (cond (args (let ((course (input-course-check (car args))))
                           ;; enter command t course
                           ;; may be this code move to FSM:torpedo-fair
                           (when course
                             (decf *energy* 2)
                             (decf *torpedo* 1)
                             (torpedo-fire *new-course*)
                             (setq *klingon-attack* t))
                           (state :mloop-command)))                     
                   (t (torpedo-message)
                      ;; enter command t
                      (state :torpedo-course))))
   ;; shield
   (%def-pgm 'z  (shield-message)(state :shield))
   ;; repair
   (%def-pgm 'r (stc/clear)(damage-report))
   ;; computer
   (%def-pgm 'c (cond (args (computer (car args)))
                      (t  (computer-message)
                          (state :computer))))
   ;; end ofmission
   (%def-pgm 'x  (end-of-mission))
   ))


(defun mloop-command (data)
  (@clt "MLOOP-receive" data)
  (funcall (%exec-pgm *loop-pgm
                      (first data)
                      (lambda () (stc/clear)(comp-help)))
           (car data)
           (rest data)))


;;;; end

|#


;;; how many time in warp
(defun warp-time (w1)
  (let ((t8 1))
    (if (< w1 1)
        (setq t8 (/ (floor (* 10 w1)) 10)))
    (incf *time* t8)
    (cond ((> *time* (+ *time0* *t-period*))
           (fail-mission))
          (t t))))


;;; input warp/torpedo course
(defvar *new-course* 0)
(defvar *last-name* nil)

;;; input message
(defun input-course-message (man)
  (@clt "   input-course-message " *last-name* man)
  (setq *last-name* man)
  (display "COURSE (0-8, -1)"))

;;; input state
(defun input-course-check (c1)
  (@clt "   input-course-check " c1)
  (cond ((not (numberp c1)) nil)
        ((= c1 -1) nil)
        (t  (cond ((or (< c1 0) (> c1 8))
                   (stc/terpri)
                   (display "   ~a: INCORRECT COURSE DATA, SIR!~%" *last-name*)
                   nil)
                  (t (if (= c1 8)
                         (setq *new-course* 0)
                         (setq *new-course* c1))
                     t) ))))


;;; warp message
(defun nav-factor-message (x)
  ;;(@clt "   nav-factor-message " x)
  (display "WARP FACTOR (0-~a)" x))

;;; warp state1
(defun nav-factor (w1)
  (@clt "   nav-factor" w1)
  (let ((wdamage (aref *ddd* 1)))
    (cond ((= w1 0)
           nil)
          ((and (< wdamage 0)(> w1 0.2))
           (stc/terpri)
           (display "WARP ENGINES ARE DAMAGED.  MAXIUM SPEED = WARP 0.2~%")
           nil)
          ((or (< w1 0)(> w1 8))
           (stc/terpri)
           (display "CHIEF ENGINEER SCOTT: THE ENGINES WON'T TAKE WARP ~a!~%" w1)
           nil)
          (t w1))))

;;; warp state2
(defun nav-energy (w1)
  (@clt "   nav-energy" w1)
  (let ((n (floor (+ (* w1 8) 0.5))))
    (cond ((< *energy* n)
           (stc/terpri)
           (display "ENGINEERING:   INSUFFICIENT ENERGY AVAILABLE~%")
           (display "               FOR MANEUVERING AT WARP~a!~%" w1)
           (cond ((or (< *shield* (- n *energy*)) (< (aref *ddd* 7) 0))
                  t)
                 (t (stc/terpri)
                    (display "DEFLECTOR CONTROL ROOM:  ~a UNITS OF ENERGY~%" *shield*)
                    (display "                         PRESENTLY DEPLOYED TO SHIELDS~%")))
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

;;; quad name display
(defun disp-quad-name(qq)
  (stc/quad qq)
  (cond ((= *time0* *time*)
         (display "YOUR MISSION BEGINS WITH YOUR STARSHIP LOCATED~%")
         (display "IN THE GALACTIC QUADRANT ~a~%" qq))
        (t (display "NOW ENTERING ~a QUADRANT...~%" qq))))

;;; display alarm: damaged
(defun repo-entering-quad-stat ()
  (@clt "   repo-entering-quad-stat")
  (when (/= *c-klingons* 0)
    (display "COMBAT AREA CONDITION RED ~%"))
  (when (<= *shield* 200)
    (display "SHIELDS DANGEROUSLY LOW ~%")))

;;;
(defun enter-quad1 ()
  (let ((g (daref *ggg* *qx* *qy*)))
    (setq *c-klingons* (quad-klingon g))
    (setq *c-bases* (quad-base g))
    (setq *c-stars* (quad-star g))))


;;; display alarm: fatal error
(defun energy-check()
  (if (and (> (+ *shield* *energy*) 10)
           (or (> *energy* 10) (zerop (aref *ddd* 7))))
      (return-from energy-check t))
  (stc/terpri)
  (display "** FATAL ERROR **~%")
  (display "YOU'VE JUST STRANDED YOUR SHIP IN SPACE~%")
  (display "YOU HAVE INSUFFICIENT MANEUVERING ENERGY~%")
  (display "AND SHIELD CONTROL IS PRESENTLY INCAPABLE OF~%")
  (display "CROSS-CIRCUITING TO ENGINE ROOM!!~%")
  nil )

(defun klingon-attack-warp()
  (dotimes (i 4)
    (cond ((/= 0 (klingon-energy (aref *kkk* i)))
           (klingon-rand-move i))))
  (klingon-attack))

(defun repair-by-warp (w1)
  (let ((flag nil)
        (x)
        (ii))
    (dotimes (i 9)
      (setq ii (1+ i))
      (cond ((< (aref *ddd* ii) 0)
             (setq x (incf (aref *ddd* ii)))
             (cond ((>= x 0)
                    (setf (aref *ddd* ii) 0)
                    (cond ((not flag)
                           (stc/terpri)
                           (display "DAMAGE CONTROL REPORT:  ~%")
                           (setq flag t)))
                    (display "~a REPAIR COMPLETED.~%" (device-name ii)))))))))

;;; note:
(defun damage-by-warp()
  (let ((damdev))
    (cond ((<= (random 10) 2)
           (stc/terpri)
           (setq damdev (rnd1-8))
           (incf (aref *ddd* damdev)
                 (cond ((< (random 10) 6)
                        (display "DAMAGE CONTROL REPORT:  ~a DAMAGED~%"
                                 (device-name damdev))
                        (* -1 (1+ (/ (random 500) 100))))
                       (t (display "DAMAGE CONTROL REPORT:  ~a STATE OF REPAIR IMPROVED~%"
                                   (device-name damdev))
                          (1+ (/ (random 300) 100)))))))))

(defun cal-vec (va c1)
  (let (ci cr)
    (setq ci (floor c1))
    (setq cr (- c1 ci))
    (+ (aref va ci)
       (* (- (aref va (+ ci 1)) (aref va ci))
          cr))))

(defun nav4 (c1 n w1)
  (@clt "    repo-entering-quad-stat" c1 n w1)
  (let ((x *ex*)
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
      (cond ((or (< *ex* 0)(> *ex* 7)
                 (< *ey* 0)(> *ey* 7))
             (return-from nav4
               (cond ((exit-quad n x0 y0 dx dy w1 old-qx old-qy)
                      (enter-quad) t)
                     (t nil))))
            (t (display "(~a,~a)" *ex* *ey*)))
      (when (daref *sec* *ex* *ey*)
        (stc/terpri)
        (setq *ex* (floor(- x dx)))
        (setq *ey* (floor(- y dy)))
        (display "WARP ENGINES SHUT DOWN AT ")
        (display "SECTOR ~a , ~a DUE TO BAD NAVAGATION" *ex* *ey*)
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
    (when (< *qx* 0) (setq flag t) (setq *qx* 0) (setq *ex* 0))
    (when (> *qx* 7) (setq flag t) (setq *qx* 7) (setq *ex* 7))
    (when (< *qy* 0) (setq flag t) (setq *qy* 0) (setq *ey* 0))
    (when (> *qy* 7) (setq flag t) (setq *qy* 7) (setq *ey* 7))
    (cond (flag
           (stc/terpri)
           (display "LT. UHURA: MESSAGE FROM STARFLEET COMMAND:~%")
           (display "      PERMISSION TO ATTEMPT CROSSING OF GALACTIC PERIMETER~%")
           (display "      IS HEREBY *DENIED*.  SHUT DOWN YOUR ENGINES.~%~%")
           (display "CHIEF ENGINEER SCOTT:  WARP ENGINES SHUT DOWN~%")
           (display "                       AT SECTOR ~a,~a OF QUADRANT ~a,~a~%"
                   *ex* *ey* *qx* *qy*)
           (sdref *sec* *ex* *ey* 'E)
           (when (> *time* (+ *time0* *t-period*))
             (fail-mission)
             nil))
          (t (cond ((and (eql *qx* old-qx)(eql *qy* old-qy))
                    (warp-time w1))
                   (t (incf *time*)
                      (dec-energy n)
                      t))))))

;;; dec energy
(defun dec-energy (n)
  (when (< (decf *energy* (+ n 10)) 0)
    (stc/terpri)
    (display "SHIELD CONTROL SUPPLIES ENERGY TO COMPLETE THE MANEUVER.~%")
    (incf *shield* *energy*)
    (setq *energy* 0)
    (when (<= *shield* 0)
      (setq *shield* 0))))

;;; calculate Long sensor, display report
(defun long-range-sensor()
  (@clt "    long-range-sensor")
  (let ((x) (y) (qqq))
    (cond ((< (aref *ddd* 3) 0)
           (display "~%LONG RANGE SENSORS ARE INOPERABLE.~%"))
          (t (display "~%LONG RANGE SCAN FOR QUADRANT ~a,~a~%" *qx* *qy*)
             (dotimes (i 3)
               (dotimes (j 3)
                 (setq x (+ *qx* i -1))
                 (setq y (+ *qy* j -1))
                 (cond ((and (>= x 0)(<= x 7)(>= y 0)(<= y 7))
                        (setq qqq (daref *ggg* x y))
                        (setf (quad-visit qqq) t)
                        (display " ~a~a~a"
                                (quad-klingon qqq)
                                (quad-base qqq)
                                (quad-star qqq)))
                       (t (display " ***"))))
               (display "~%"))))))

;;; display report "no enemy"  by  Spok
(defun noenemy ()
  (display "~%")
  (display "SCIENCE OFFICER SPOCK:   SENSORS SHOW NO ENEMY SHIPS~%")
  (display "                         IN THIS QUADRANT~%"))

;;; display phaser message
(defun phaser-message ()
  (cond  ((< (aref *ddd* 4) 0)
          (stc/terpri)
          (display "PHASERS INOPERATIVE.~%")
          (return-from phaser-message (values)))
         ((<= *c-klingons* 0)
          (noenemy)
          (return-from phaser-message (values)))
         ((< (aref *ddd* 8) 0)
          (stc/terpri)
          (display "COMPUTER FAILURE HAMPERS ACCURACY.~%"))
         (t (stc/terpri)
            (display "PHASERS LOCKED ON TARGET.~%")))
  (display "ENERGY AVAILABLE = ~a UNITS~%" *energy*)
  (display "NUMBER OF UNITS TO FIRE ?"))

;;; display current phaser state
(defun phaser4 (x)
  (let ((h) (ke) (kx)
        (ky) (k))
    (decf *energy* x)
    (cond ((< (aref *ddd* 8) 0)
           (setq x (random x))))
    (setq h1 (floor (/ x *c-klingons*)))
    (setq *klingon-attack* t)
    (dotimes (i 4)
      (setq k (aref *kkk* i))
      (setq kx (klingon-x k))
      (setq ky (klingon-y k))
      (cond ((> (klingon-energy k) 0)
             (setq h (floor (* (/ h1 (klingon-distance i)) (+ 2 (/(random 10)10)))))
             (cond ((<= h (* (klingon-energy k) 0.15))
                    (stc/terpri)
                    (display "SENSORS SHOW NO DAMAGE TO ENEMY AT ~a,~a~%"  kx ky))
                   (t (setq ke (decf (klingon-energy k) h))
                      (display "~a UNIT HIT ON KLINGON AT SECTOR ~a,~a~%" h kx ky)
                      (cond ((<= ke 0)
                             (delete-klingon i))
                            (t (display "SENSORS SHOW ~d UNITS REMAINING~%" ke))))))))))

;;; drop star
(defun delete-star (x y)
  (sdref *sec* x y nil)
  (decf (quad-star (daref *ggg* *qx* *qy*))))

;;; drop klingon
(defun delete-klingon(i)
  (let ((kx) (ky) (k))
    (setq k (aref *kkk* i))
    (setq kx (klingon-x k))
    (setq ky (klingon-y k))
    (display "*** KLINGON DESTROYED ***~%")
    (decf *c-klingons*)
    (decf *klingon-total*)
    (sdref *sec* kx ky nil)
    (setf (klingon-energy k) 0)
    (decf (quad-klingon (daref *ggg* *qx* *qy*)))))

(defun delete-klingon-xy (x y)
  (let ((k))
    (dotimes (i 4)
      (setq k (aref *kkk* i))
      (cond ((and
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
  (@clt "    torpedo-message")
  (cond ((<= *torpedo* 0)
         (display "~%ALL PHOTON TORPEDOES EXPENDED.~%"))
        ((< (aref *ddd* 5) 0)
         (display "~%PHOTON TUBES ARE NOT OPERATIONAL.~%"))
        (t (display "PHOTON TORPEDO ")))
  (input-course-message "ENSIGN CHEKOV"))

;;; torpedo fire state
(defun torpedo-fire (course)
  (@clt "    torpedo-fire" course)
  (let ((repeat t)
        (x *ex*)
        (y *ey*)
        (obj))
    (setq x1 (cal-vec *cx* course))
    (setq y1 (cal-vec *cy* course))
    (display "TORPEDO TRACK: ")
    (jscl::while repeat
                 (incf x x1)
                 (incf y y1)
                 (setq x3 (floor (+ x 0.5)))
                 (setq y3 (floor (+ y 0.5)))
                 (setq obj (daref *sec* x3 y3))
                 (cond ((or (< x3 0) (> x3 7) (< y3 0)(> y3 7))
                        (display "~%TORPEDO MISSED.~%")
                        (return-from torpedo-fire (values)))
                       ((eql obj 'k)
                        (display "~%")
                        (delete-klingon-xy x3 y3)
                        (when (<= *klingon-total* 0)
                          (setq *success* t))
                        (return-from torpedo-fire (values)))
                       ((eql obj 's)
                        (display "~%STAR AT ~a, ~a ABSORBED TORPEDO ENERGY.~%" x3 y3)
                        (delete-star x3 y3)
                        (return-from torpedo-fire (values)))
                       ((eql obj 'b)
                        (display "~&*** STAR BASE DESTROYED ***  .......CONGRATULATIONS~&")
                        (delete-base x3 y3)
                        (destroy-base)
                        (return-from torpedo-fire (values))))
                 (display "(~a,~a)" x3 y3))))

(defun destroy-base ()
  (cond ((or (> *base-total* 0) (> *klingon-total* (- *time* *time0* *t-period*)))
         (display "~%STARFLEET COMMAND REVIEWING YOUR RECORD TO CONSIDER~%")
         (display "COURT MARTIAL!~%")
         (setq *docked* nil)
         (setq *klingon-attack* t))
        (t (display "~%THAT DOES IT, CAPTAIN!!  YOU ARE HEREBY RELIEVED OF COMMAND~%")
           (display "AND SENTENCED TO 99 STARDATES AT HARD LABOR ON CYGNUS 12!!~%")
           (end-of-mission))))

;;; shield message
(defun shield-message ()
  (@clt "    shield-message")
  (if (< (aref *ddd* 7) 0)
      (progn
        (display "~%SHIELD CONTROL INOPERABLE.~%")
        (state :mloop-command))
      (display "~%ENERGY AVAILABLE =~a.  NUMBER OF UNITS TO SHIELDS ?"
               (+ *energy* *shield*))))

;;; shield state
(defun shield (x)
  (@clt "    shield" x)
  (cond ((or (< x 0) (= x *shield*))
         (display "SHIELDS UNCHANGED~%"))
        ((> x (+ *energy* *shield*))
         (display "~%SHIELD CONTROL:  THIS IS NOT THE FEDERATION TREASURY~%" )
         (display "SHIELDS UNCHANGED~%"))
        (t (incf *energy* (- *shield* x))
           (setq *shield* x)
           (display "DEFLECTOR CONTROL ROOM:~%")
           (display "SHIELDS NOW AT ~a UNITS PER YOUR COMMAND~%" *shield*))))

;;; damage report
(defun damage-report()
  (cond ((< (aref *ddd* 6) 0)
         (display "~%DAMAGE CONTROL REPORT NOT AVAILABLE.~%"))
        (t (show-stat-repair)))
  (docked-repair))

(defun show-stat-repair()
  (display "DEVICE             STATE OF REPAIR~%")
  (display "------             ---------------~%")
  (dotimes (i 8)
    (display "~a ~d~%" (strfor 19 (device-name (1+ i)))
            (* (floor (* 100 (aref *ddd* (1+ i)))) 0.1))))

(defun docked-repair()
  (let (d3)
    (cond (*docked*
           (setq d3 0)
           (dotimes (i 8)
             (when (< (aref *ddd* (1+ i)) 0)
               (incf d3 0.1)))
           (cond ((= d3 0) nil)
                 (t (need-repair-message d3)))))))

(defvar *d3-repair*)

(defun need-repair-message (d3)
  (incf d3 *damage-repair-magic-number*)
  (setq d3 (if (>= d3 1)  0.9  d3))
  (display "TECHNICIANS STANDING BY TO EFFECT REPAIRS TO YOUR SHIP~%")
  (display "ESTIMATED TIME TO REPAIR: ~d STARDATES~d~%"
          (* .01 (floor (* 100 D3))))
  (display "WILL YOU AUTHORIZE THE REPAIR ORDER (Y/N) ?:")
  (setq *d3-repair* d3)
  (state :need-repair))

(defun repair-all()
  (dotimes (i 8)
    (cond((<  (aref *ddd* (1+ i)) 0)
          (setf (aref *ddd* (1+ i)) 0)))))


;;; klingon attack
(defun klingon-attack ()
  (cond ((<= *c-klingons* 0) t)
        (*docked*
         (display "~%STARBASE SHIELDS PROTECT THE ENTERPRISE.~%")
         t)
        (t (dotimes (i 4)
             (let* ((k (aref *kkk* i))(ke (klingon-energy k)) h)
               (when (> ke 0)
                 (setq h (floor (*(/ ke (klingon-distance i))
                                  (+ 2 (/ (random 10) 10)))))
                 (decf *shield* h)
                 (setf (klingon-energy k)(/ ke  (+ 3 (/ (random 10) 10))))
                 (display "~a UNIT HIT ON ENTERPRISE FROM SECTOR ~a,~a ~%"
                          h (klingon-x k)(klingon-y k))
                 (cond ((<= *shield* 0)
                        (enterprise-destroyed)
                        (return-from klingon-attack))
                       (t (display  "SHIELDS DOWN TO ~a UNITS~%" *shield* )
                          (when(>= h 20)
                            (when (and (<= (random 10) 6) (> (/ h *shield*) 0.02))
                              (setq r1 (rnd1-8))
                              (decf (aref *ddd* r1) (+ (/ h *shield*) (/ (random 50) 100)))
                              (display "DAMAGE CONTROL: ~a DAMAGED BY THE HIT~%"
                                       (device-name r1))))))))))))

;;; Fail 1 energy==0 or timeout
(defun fail-mission()
  (display "~%IT IS STARDATE ~d.~%" *time*)
  (end-of-mission ))

;;; Fail destroyed
(defun enterprise-destroyed ()
  (display "~%THE ENTERPRISE HAS BEEN DESTROYED.~%")
  (display "  THE FEDERATION WILL BE CONQUERED.~%")
  (fail-mission))

;;; end of mission
(defun end-of-mission()
  (display "~%THERE WERE ~a KLINGON BATTLE CRUISERS LEFT AT~%" *klingon-total*)
  (display "THE END OF YOUR MISSION.~%")
  (setq *mission-end* t))

;;; more-mission
(defun more-mission-message ()
  (cond ((/= *base-total* 0)
         (display "~%THE FEDERATION IS IN NEED OF A NEW STARSHIP COMMANDER~%")
         (display "FOR A SIMILAR MISSION -- IF THERE IS A VOLUNTEER,~%")
         (display "LET HIM STEP FORWARD AND ENTER 'AYE'" )
         (state :more-mission) )))

;;; success
(defun success ()
  (let ((x (/ *klingon-org* (- *time* *time0*))))
    (display "CONGRATULATIONS, CAPTAIN!  THE LAST KLINGON BATTLE CRUISER~%")
    (display "MENACING THE FEDERATION, HAS BEEN DESTROYED.~%~%")
    (display "YOUR EFFICIENCY RATING IS ~s" (* x x 1000))))

;;; DOCKED
;;; todo: update statuc location on console
(defun dockedp()
  (@clt "    docedp" *condi*)
  (let ((x) (y))
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
          (display "SHIELDS DROPPED FOR DOCKING PURPOSES.~%")
          (return-from dockedp t))))
    (setq *docked* nil)
    nil))

(defun set-condition ()
  (@clt "    set-condition " *condi*)
  (cond ((not (dockedp))
         (cond ((> *c-klingons* 0) (setq *condi* "*RED*"))
               ((< *energy* (/ *full-energy* 10)) (setq *condi* "YELLOW"))
               (t  (setq *condi* "GREEN"))))))


;;; short range sensor
(defun short-range-sensor ()
  (@clt "   short-range-sensor ")
  (let ((fff))
    (set-condition)
    (when (< (aref *ddd* 2) 0)
      (display "*** SHORT RANGE SENSORS ARE OUT ***~%")
      (return-from short-range-sensor nil))
    (display "   +0-1-2-3-4-5-6-7-+")
    (setq fff  *disp-info-funcs*)
    (dotimes (i 8)
      (display "~%  ~a|" i)
      (dotimes (j 8)
        (display "~a "
                (case  (daref *sec* i j)
                  ((s) "*")
                  ((k) "K")
                  ((b) "B")
                  ((e) "E")
                  (t   "."))))
      (display "|")
      (apply (pop fff) nil))
    (display "~%")))

;;; some messages handler's
(defconstant *disp-info-funcs*
  (list
   (lambda() (display "        STARDATE           ~d" (roundnum *time* 4) ))
   (lambda() (display "        CONDITION          ~a" *condi*))
   (lambda() (display "        QUADRANT           ~a ~a" *qx* *qy*))
   (lambda() (display "        SECTOR             ~a ~a" *ex* *ey*))
   (lambda() (display "        PHOTON TORPEDOES   ~a" *torpedo*))
   (lambda() (display "        TOTAL ENERGY       ~d" (+ *energy* *shield*)))
   (lambda() (display "        SHIELDS            ~d" *shield*))
   (lambda() (display "        KLINGONS REMAINING ~a" *klingon-total*))))


;;; COMPUTER

;;; computer help display
(defun comp-help ()
  (display "FUNCTIONS AVAILABLE FROM LIBRARY-COMPUTER:~%")
  (display "-----------------------------------------~%")
  (display "   G   CUMULATIVE GALATIC RECORD~%")
  (display "   S   STATUS REPORT~%")
  (display "   T   PHOTON TORPEDO DATA~%")
  (display "   B   STARBASE NAV DATA~%")
  (display "   N   DIRECTION/DISTANCE CALCULATOR~%")
  (display "   Z   GALAXY 'REGION NAME' MAP~%")
  (display "   O   OPERATIONAL WRAP/TORPEDO MANUAL~&"))


;;; computer state report display
(defun computer-message ()
    (if (< (aref *ddd* 8) 0)
        (display "~%COMPUTER DISABLED.~%")
        (display "~%COMPUTER ACTIVE AND AWAITING COMMAND~%")))

;;; computer itself
(defvar *comp-pgm
  (list
   (%def-pgm 'g  (stc/clear)(comp-galaxy-rec)(state :mloop-command))
   (%def-pgm 's  (stc/clear)(comp-stat-repo)(state :mloop-command))
   (%def-pgm 't  (comp-torpedo)(state :mloop-command))
   (%def-pgm 'b  (base-nav)(state :mloop-command))
   (%def-pgm 'n  (comp-calc-message)(state :comp-calc))
   (%def-pgm 'z  (stc/clear)(comp-galaxy-name-map)(state :mloop-command))
   (%def-pgm 'o  (stc/clear)(comp-direction-help)(state :mloop-command))))

(defun computer (a)
  (@clt "COMP" a)
  (#j:console:log (format nil "Comp receive args ~a" a))
  (funcall (%exec-pgm *comp-pgm a (lambda () (stc/clear)(comp-help)))))

#+nil
(defun computer (a)
  (@clt "COMP")
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

;;; galaxy map display
(defun comp-galaxy-name-map()
  (display "                        THE GALAXY~%")
  (display "       0     1     2     3     4     5     6     7~%")
  (display "    +-----+-----+-----+-----+-----+-----+-----+-----+")
  (dotimes (i 8)
    (display "~%")
    (dotimes (j 2)
      (display " ~a" (quad-name (* j 4) i 1)))))

;;; galaxy quadrant display
(defun comp-galaxy-rec()
  (let ((x) (qqq))
    (display "       COMPUTER RECORD OF GALAXY FOR QUADRANT ~a,~a~%" *qx* *qy*)
    (display "       0     1     2     3     4     5     6     7~%")
    (display "    +-----+-----+-----+-----+-----+-----+-----+-----+")
    (dotimes (i 8)
      (display "~%  ~a |" i)
      (dotimes (j 8)
        (setq qqq (daref *ggg* i j))
        (cond ((quad-visit qqq)
               (display " ~a~a~a  "
                       (quad-klingon qqq)
                       (quad-base qqq)
                       (quad-star qqq)))
              (t (display " ***  ")))))
    (stc/terpri) ))

;;; status report display
(defun comp-stat-repo()
  (display "   STATUS REPORT:~%   -------------~%")
  (display " ~a KLINGON~a LEFT.~%" *klingon-total* (if (> *klingon-total* 1) "S" ""))
  (display " MISSION MUST BE COMPLETED IN ~d STARDATES.~%"
          (* (/(floor (+ *time0* *t-period* (- *time*))) 10) 10))
  (cond ((> *base-total* 0)
         (display " THE FEDERATION IS MAINTAINING ~a STARBASE~a IN THE GALAXY.~%~%"
                 *base-total*  (if (> *base-total* 1) "S" "")))
        (t (display "YOUR STUPIDITY HAS LEFT YOU ON YOUR OWN IN~%")
           (display "  THE GALAXY -- YOU HAVE NO STARBASES LEFT!~%~%")))
  (damage-report))

;;; torpedo attack display
(defun comp-torpedo()
  (cond ((<= *c-klingons* 0) (noenemy))
        (t (display "FROM ENTERPRISE TO KLINGON BATTLE CRUSER~a~%"
                   (if (> *c-klingons* 1) "S" ""))
           (dotimes (i 4)
             (cond ((> (klingon-energy (aref *kkk* i)) 0)
                    (comp-torpedo1 i)))))))

(defun comp-torpedo1(i)
  (let* ((k (aref *kkk* i))
         (kx (klingon-x k))
         (ky (klingon-y k)))
    (display "KLINGON at (~a,~a): DIRECTION = ~d~%" kx ky
            (roundnum (calc-p2p *ex* *ey* kx ky) 3))))

;;; calculator
(defun comp-calc-message()
  (display "DIRECTION/DISTANCE CALCULATOR:~%")
  (display "YOU ARE AT QUADRANT ~a,~a " *qx* *qy*)
  (display " SECTOR ~a,~a.~%" *ex* *ey*)
  (display "PLEASE ENTER INITIAL COORDINATES X Y?"))

(defun disp-direct-dist (x0 y0 x1 y1)
  (display "DIRECTION = ~d " (roundnum (calc-p2p x0 y0 x1 y1) 3))
  (pr-distance (- x0 x1)(- y0 y1)))

(defun pr-distance(dx dy)
    (display "DISTANCE =~d~%" (roundnum (distance-p2p dx dy) 3)))

;;; starbase nav data
(defun base-nav()
  (cond ((= *c-bases* 0)
         (display "MR. SPOCK:  SENSORS SHOW NO STARBASES IN THIS QUADRANT.~%"))
        (t (disp-direct-dist *ex* *ey* *bx* *by*))))

(defun distance-p2p (dx dy)
  (sqrt (+ (* dx dx) (* dy dy))))

(defun calc-p2p (x0 y0 x1 y1)
  (let ((dx) (dy))
    (setq dx (- x1 x0))
    (setq dy (- y1 y0))
    (cond ((and (= dx 0)(= dy 0))
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

;;; klingon random moves display
(defun klingon-rand-move (i)
  (let ((repeat t)
        (x) (y)
        (newx) (newy) (xxx)
        (k  (aref *kkk* i)))
    (setq x (klingon-x k))
    (setq y (klingon-y k))
    (jscl::while repeat
           (setq newx (randmove-vec x))
           (setq newy (randmove-vec y))
           (setq xxx (daref *sec* newx newy))
           (when (or (null xxx) (eql 'k xxx))
             (setq repeat nil)))
    (cond ((null xxx)
           (sdref *sec* x y nil)
           (sdref *sec* newx newy 'k)
           (setf (klingon-x k) newx)
           (setf (klingon-y k) newy)
           (display "KLINGON AT ~a,~a MOVES TO ~a,~a~%" x y newx newy)))))

(defun randmove-vec (x)
  (let ((_new)
        (d (1- (random 3))))
    (setq _new (+ x d))
    (if (< _new 0) (setq _new 0))
    (if (> _new 7) (setq _new 7))
    _new))


(defun comp-direction-help ()
  (display "               WARP / TORPEDO~&")
  (display "          =======================~&~&")
  (display "  DIRECTION IS IN A CIRCULAR NUMERICAL       7    0    1~&")
  (display "  VECTOR ARRANGEMENT AS SHOWN.                `.  :  .' ~&")
  (display "  INTERGER AND REAL VALUES MAY BE               `.:.'   ~&")
  (display "  USED.  THEREFORE COURSE 1.5 IS             6---<*>---2~&")
  (display "  HALF WAY BETWEEN 1 AND 2.                     .':`.   ~&")
  (display "                                              .'  :  `. ~&")
  (display "                                             5    4    3~&")
  (display "~&")
  (display "                                               COURSE~&")
  (display "~&")
  (display "  ONE 'WARP FACTOR' IS THE SIZE OF ONE QUADRANT. THEREFORE~&")
  (display "  TO GET FROM QUADRANT 5,6 TO 5,5 YOU WOULD USE COURSE 3~&")
  (display "  WARP FACTOR 1. COORDINATES ARE SPECIFIED USING X Y NOTATION~&")
  (display "  WITH X 1-8 FROM LEFT-RIGHT AND Y 1-8 FROM TOP-BOTTOM.~&")
  )

#|
;;; manual section
;;; console screen dimension: 26x70
(defparameter +manual+
  `(
    (D
     (
      "COMMAND's W OR T WARP-ENGINE-CONTROL/TORPEDO ~&~&"
      "  DIRECTION IS IN A CIRCULAR NUMERICAL       7    0    1~&"
      "  VECTOR ARRANGEMENT AS SHOWN.                `.  :  .' ~&"
      "  INTERGER AND REAL VALUES MAY BE               `.:.'   ~&"
      "  USED.  THEREFORE COURSE 1.5 IS             6---<*>---2~&"
      "  HALF WAY BETWEEN 1 AND 2.                     .':`.   ~&"
      "                                              .'  :  ` .~&"
      "                                              5   4    3~&"
      "~&"
      "                                               COURSE~&"
      "~&"
      "  ONE 'WARP FACTOR' IS THE SIZE OF ONE QUADRANT. THEREFORE~&"
      "  TO GET FROM QUADRANT 5,6 TO 5,5 YOU WOULD USE COURSE 3~&"
      "  WARP FACTOR 1. COORDINATES ARE SPECIFIED USING X Y NOTATION~&"
      "  WITH X 1-8 FROM LEFT-RIGHT AND Y 1-8 FROM TOP-BOTTOM.~&"
      ))

    (S
     (
      "COMMAND L = SHORT RANGE SENSOR SCAN~&"
      "  PRINTS THE QUADRANT YOU ARE CURRENTLY IN, INCLUDING~&"
      "  STARS, KLINGONS, STARBASES, AND THE ENTERPRISE; ALONG~&"
      "  WITH OTHER PERTINATE INFORMATION.~&"
      "~&"
      ))

    (L
     (
      "COMMAND L = LONG RANGE SENSOR SCAN~&"
      "  SHOWS CONDITIONS IN SPACE FOR ONE QUADRANT ON EACH SIDE~&"
      "  OF THE ENTERPRISE IN THE MIDDLE OF THE SCAN.  THE SCAN~&"
      "  IS CODED IN THE FORM XXX, WHERE THE UNITS DIGIT IS THE~&"
      "  NUMBER OF STARS, THE TENS DIGIT IS THE NUMBER OF STAR-~&"
      "  BASES, THE HUNDREDS DIGIT IS THE NUMBER OF KLINGONS.~&"
      "~&"
      ))

    (P
     (
      "COMMAND P = PHASER CONTROL~&"
      "  ALLOWS YOU TO DESTROY THE KLINGONS BY HITTING HIM WITH~&"
      "  SUITABLY LARGE NUMBERS OF ENERGY UNITS TO DEPLETE HIS~&"
      "  SHIELD POWER.  KEEP IN MIND THAT WHEN YOU SHOOT AT~&"
      "  HIM, HE GONNA DO IT TO YOU TOO.~&"
      "~&"
      ))

    (T
     (
      "COMMAND T = PHOTON TORPEDO CONTROL~&"
      "  COURSE IS THE SAME AS USED IN WARP ENGINE CONTROL~&"
      "  IF YOU HIT THE KLINGON, HE IS DESTROYED AND CANNOT FIRE~&"
      "  BACK AT YOU.  IF YOU MISS, HE WILL SHOOT HIS PHASERS AT~&"
      "  YOU.~&"
      "   NOTE: THE LIBRARY COMPUTER (COMMAND C) HAS AN OPTION~&"
      "   TO COMPUTE TORPEDO TRAJECTORY FOR YOU (OPTION N).~&"
      "~&"
      ))

    (Z
     (
      "COMMAND Z = SHIELD CONTROL~&"
      "  DEFINES NUMBER OF ENERGY UNITS TO ASSIGN TO SHIELDS~&"
      "  ENERGY IS TAKEN FROM TOTAL SHIP'S ENERGY.~&"
      "~&"
      ))

    (C
     (
      "COMMAND C = CALL ON LIBRARY-COMPUTER~&"
      "  GIVES STATE OF REPAIRS OF ALL DEVICES.~&"
      "  A STATE OF REPAIR~&"
      "  LESS THAN ZERO SHOWS THAT THAT DEVICE IS TEMPORARALY~&"
      "  DAMAGED.~&"
      "~&"
      ))

    (X
     (
      "COMMAND 7 = LIBRARY COMPUTER~&"
      "  THE LIBRARY COMPUTER CONTAINS THREE OPTIONS:~&"
      "    OPTION 0 = CUMULATIVE GALACTIC RECORD~&"
      "     SHOWS COMPUTER MEMORY OF THE RESULTS OF ALL PREVIOUS~&"
      "     LONG RANGE SENSOR SCANS~&"
      "    OPTION 1 = STATUS REPORT~&"
      "     SHOWS NUMBER OF KLINGONS, STARDATESC AND STARBASES~&"
      "     LEFT.~&"
      "    OPTION 2 = PHOTON TORPEDO DATA~&"
      "     GIVES TRAJECTORY AND DISTANCE BETWEEN THE ENTERPRISE~&"
      "     AND ALL KLINGONS IN YOUR QUADRANT~&"
      ))

    ))
|#



;;; EOF
