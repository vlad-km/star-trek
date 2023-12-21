;;; -*- mode:lisp; coding:utf-8  -*-


(defvar *state-fsm*)

(defun state (lbl)
    (setq *state-fsm* lbl))


(defvar *c1*)
(defvar *w1*)
(defvar *n*)
(defvar *x0*)
(defvar *y0*)
(defvar *x1*)
(defvar *y1*)


(mordev:rx-listen :fsm (lambda (data) (fsm data)))

(defun fsm (input)

    (case *state-fsm*

      (:accept-command
       (when (eql (car input) 'n)
           (end-of-game)
           (return-from fsm (values)))
       (enter-quad)
       (mloop))

      (:mloop-command
       (mloop-command (car input))
       (return-from fsm (values)))

      (:navigate
       (input-course-message "LT. SULU")
       (state :input-course-check)
       (return-from fsm (values)))

      (:input-course-check
       (setq *c1* (input-course-check (car input)))
       (when *c1*
           (format *so* ": ~a " *new-course*)
           (nav-factor-message (if (< (aref *ddd* 1) 0) 0.2 8))
           (state :nav-factor))
       (return-from fsm (values)))

      (:nav-factor
       (state :mloop-command)
       (setq *w1* (nav-factor (car input)))
       (when *w1*
           (format *so* ": ~a~%" *w1*)
           (setq *n* (nav-energy *w1*))
           (unless *n* (return-from fsm (values)))
           (klingon-attack-warp)
           (repair-by-warp *w1*)
           (damage-by-warp)
           (when (not (nav4 *new-course* *n* *w1*))
               (return-from fsm t))
           (warp-time *w1*))
       ;;(state :mloop-command)
       (return-from fsm (values)))

      (:torpedo-course
       ;;(state :mloop-command)
       (setq *c1* (input-course-check (car input)))
       (when *c1*
           (format *so* ": ~a~%" *new-course*)
           (decf *energy* 2)
           (decf *torpedo* 1)
           (torpedo-fire *new-course*)
           (setq *klingon-attack* t))
       (state :mloop-command)
       (return-from fsm (values)))

      (:phaser
       (format *so* ": ~a~%" input)
       (phaser4 (car input))
       (state :mloop-command)
       (return-from fsm (values)))

      (:shield
       (format *so* ": ~a~%" input)
       (shield (car input))
       (state :mloop-command)
       (return-from fsm (values)))

      (:computer
       (computer (car input))
       ;;(state :mloop-command)
       (return-from fsm (values)))

      (:comp-calc
       (format *so* ": ~a~%" input)
       (setq *x0* (car input))
       (setq *y0* (cadr input))
       (format *so* "FINAL COORDINATES X Y?")
       (state :comp-calc-final-co)
       (return-from fsm (values)))

      (:comp-calc-y
       (setq *y0* input)
       (format t "FINAL COORDINATES X?")
       (state :comp-calc-final-x)
       (return-from fsm (values)))

      (:comp-calc-final-x
       (setq *x1* input)
       (format t "Y?")
       (state :comp-calc-final-y)
       (return-from fsm (values)))

      (:comp-calc-final-co
       (format *so* ": ~a~%" input)
       (setq *x1* (car input))
       (setq *y1* (cadr input))
       (disp-direct-dist *x0* *y0* *x1* *y1*)
       (state :mloop-command)
       (return-from fsm (values)))

      (:need-repair
       (format *so* "~a~%" input)
       (cond ((eql 'y (car input))
              (repair-all)
              (incf *time* (+ *d3-repair* 0.1))
              (show-stat-repair)))
       (state :mloop-command)
       (return-from fsm (values)))

      (:more-mission
       (if (eql (car input) 'aye)
           (mordev:rx-emit :new-mission))
       (return-from fsm (values)))

      ))



;;; EOF
