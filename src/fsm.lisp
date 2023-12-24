;;; -*- mode:lisp; coding:utf-8  -*-
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


(defvar *state-fsm*)
(defvar *c1*)
(defvar *w1*)
(defvar *n*)
(defvar *x0*)
(defvar *y0*)
(defvar *x1*)
(defvar *y1*)

(defun state (lbl)
    (setq *state-fsm* lbl))

(rx:listen :fsm (lambda (data) (fsm data)))

(defun fsm (input)
  (#j:console:log "FSM state" *state-fsm* "Data" input)
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
       (display ": ~a " *new-course*)
       (nav-factor-message (if (< (aref *ddd* 1) 0) 0.2 8))
       (state :nav-factor))
     (return-from fsm (values)))
    (:nav-factor
     (state :mloop-command)
     (setq *w1* (nav-factor (car input)))
     (when *w1*
       (display ": ~a~%" *w1*)
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
       (display ": ~a~%" *new-course*)
       (decf *energy* 2)
       (decf *torpedo* 1)
       (torpedo-fire *new-course*)
       (setq *klingon-attack* t))
     (state :mloop-command)
     (return-from fsm (values)))
    (:phaser
     (display ": ~a~%" input)
     (phaser4 (car input))
     (state :mloop-command)
     (return-from fsm (values)))
    (:shield
     (display ": ~a~%" input)
     (shield (car input))
     (state :mloop-command)
     (return-from fsm (values)))
    (:computer
     (computer (car input))
     ;;(state :mloop-command)
     (return-from fsm (values)))
    (:comp-calc
     (display ": ~a~%" input)
     (setq *x0* (car input))
     (setq *y0* (cadr input))
     (display "FINAL COORDINATES X Y?")
     (state :comp-calc-final-co)
     (return-from fsm (values)))
    (:comp-calc-y
     (setq *y0* input)
     ;; note: wtf?
     ;; note: the state never use
     (format t "FINAL COORDINATES X?")
     (state :comp-calc-final-x)
     (return-from fsm (values)))
    (:comp-calc-final-x
     (setq *x1* input)
     ;; note: wtf?
     ;; note: the state never use
     (format t "Y?")
     (state :comp-calc-final-y)
     (return-from fsm (values)))
    (:comp-calc-final-co
     (display ": ~a~%" input)
     (setq *x1* (car input))
     (setq *y1* (cadr input))
     (disp-direct-dist *x0* *y0* *x1* *y1*)
     (state :mloop-command)
     (return-from fsm (values)))
    (:need-repair
     (display "~a~%" input)
     (cond ((eql 'y (car input))
            (repair-all)
            (incf *time* (+ *d3-repair* 0.1))
            (show-stat-repair)))
     (state :mloop-command)
     (return-from fsm (values)))
    (:more-mission
     (if (eql (car input) 'aye)
         (rx:emit :new-mission nil))
     (return-from fsm (values)))
    ))



;;; EOF
