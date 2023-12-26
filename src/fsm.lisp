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


(defvar *state-fsm* nil)
(defvar *c1*)
(defvar *w1*)
(defvar *n*)
(defvar *x0*)
(defvar *y0*)
(defvar *x1*)
(defvar *y1*)

(defun state (lbl)
  (#j:console:log (format nil "STATE ~a ==> ~a" *state-fsm* lbl))
    (setq *state-fsm* lbl))

(rx:listen :fsm (lambda (data) (fsm data)))

(defun fsm (input)
  (@clt "FSM" input)
  (case *state-fsm*
    (:accept-command
     (when (eql (car input) 'n)
       (end-of-game)
       (return-from fsm (values)))
     (enter-quad)
     (mloop))
    (:mloop-command
     (@clt "FSM :mloop-command" input)
     #+nil (mloop-command (car input))
     (mloop-command input)
     (return-from fsm (values)))
    (:navigate
     (@clt "FSM :navigate" input)
     (input-course-message "LT. SULU")
     (state :input-course-check)
     (return-from fsm (values)))
    (:input-course-check
     (@clt "FSM :input-course-check" input)
     (setq *c1* (input-course-check (car input)))
     (when *c1*
       (display ": ~a " *new-course*)
       (nav-factor-message (if (< (aref *ddd* 1) 0) 0.2 8))
       (state :nav-factor))
     (return-from fsm (values)))
    (:nav-factor
     (@clt "FSM :nav-factor" input)
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
    #+nil(:full-warp-command
     (@clt "FSM :nav-factor" input)
     (state :mloop-command)
     (setq *w1* (nav-factor *new-factor*))
     (when *w1*
       ;;(display ": ~a~%" *w1*)
       (setq *n* (nav-energy *w1*))
       (unless *n* (return-from fsm (values)))
       (klingon-attack-warp)
       (repair-by-warp *w1*)
       (damage-by-warp)
       (when (not (nav4 *new-course* *n* *w1*))
         (return-from fsm t))
       (warp-time *w1*)
       )
     (return-from fsm (values)))
    (:torpedo-course
     (@clt "FSM :torpedo-course" input)
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
     (@clt "FSM :phaser" input)
     (display ": ~a~%" input)
     (phaser4 (car input))
     (state :mloop-command)
     (return-from fsm (values)))
    (:shield
     (@clt "FSM :shield" input)
     (display ": ~a~%" input)
     (shield (car input))
     (state :mloop-command)
     (return-from fsm (values)))
    (:computer
     (@clt "FSM :computer" input)
     (computer (car input))
     ;;(state :mloop-command)
     (return-from fsm (values)))
    (:comp-calc
     (@clt "FSM :comp-calc" input)
     (display ": ~a~%" input)
     (setq *x0* (car input))
     (setq *y0* (cadr input))
     (display "FINAL COORDINATES X Y?")
     (state :comp-calc-final-co)
     (return-from fsm (values)))
    (:comp-calc-y
     (@clt "FSM :comp-calc-y" input)
     (setq *y0* input)
     ;; note: wtf?
     ;; note: the state never use
     (format t "FINAL COORDINATES X?")
     (state :comp-calc-final-x)
     (return-from fsm (values)))
    (:comp-calc-final-x
     (@clt "FSM :comp-calc-final-x" input)
     (setq *x1* input)
     ;; note: wtf?
     ;; note: the state never use
     (format t "Y?")
     (state :comp-calc-final-y)
     (return-from fsm (values)))
    (:comp-calc-final-co
     (@clt "FSM :comp-calc-final-co" input)
     (display ": ~a~%" input)
     (setq *x1* (car input))
     (setq *y1* (cadr input))
     (disp-direct-dist *x0* *y0* *x1* *y1*)
     (state :mloop-command)
     (return-from fsm (values)))
    (:need-repair
     (@clt "FSM :need-repair" input)
     (display "~a~%" input)
     (cond ((eql 'y (car input))
            (repair-all)
            (incf *time* (+ *d3-repair* 0.1))
            (show-stat-repair)))
     (state :mloop-command)
     (return-from fsm (values)))
    (:more-mission
     (@clt "FSM :more-mission" input)
     (if (eql (car input) 'aye)
         (rx:emit :new-mission nil))
     (return-from fsm (values)))
    ))



;;; EOF
