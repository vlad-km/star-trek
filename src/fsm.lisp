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

;;; (fsm input) type-of input -> list
;;; perform the command received from the keyboard
;;; the command context is set in the *state-fsm*

#|
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
|#

;;; Keyboard input
;;;
;;; The following samples are expected
;;;
;;; (symbol) - control command without parameters, in this case the question-answer script is executed
;;;            ex: (w) | (t) | (s) | (l) | (c)etc. etc.
;;;         - the answer to the question asked earlier in the script, something like: o | y | 1.333
;;;
;;; (integer|float) - the answer to a question asked, the missing parameter of a previously entered command,
;;;                   or any current scenario parameter
;;;                   ex: (1) | (y) | (.333)
;;;
;;; (symbol symbol) - on-board computer control command/subcommand
;;;                   ex: (c g) | (cn) | (c t) | (c o) etc. etc.
;;;
;;; (symbol integer integer|float) - starship engine control command.
;;;                               ex: (w 1 1) | (w 0.91 0.777)
;;;
;;; (symbol integer|float) - command to control torpedoes or phaser
;;;                          ex: (t 1) | (p 10)
;;;


;;; The FSM receives the command or command parameters from the reader.
;;;     depending on the context, performs actions with this command

(defvar *fsm-pgm
  (list

   ;; launch point
   (@def-pgm :accept-command
       (@clt "FSM" data (kbr-a1 data) (kbr-a2 data) (kbr-a3 data))
     (cond ((eql (kbr-a1 data) 'n)
            (end-of-game))
           (t (enter-quad)
              (mloop))))
   ;; all input will be processed at (mloop-command)
   (@def-pgm :mloop-command
       (@clt "FSM" data (kbr-a1 data) (kbr-a2 data) (kbr-a3 data))
     (mloop-command data)
     (values))

   (@def-pgm :more-mission
       (@clt "FSM" data)
     (cond ((eql (aref data 1) 'ave)(rx:emit :new-mission nil))
           (t (fuck-all-them)))
     (values))
   
   ;; issue an invitation to enter a course
   ;; and switch context
   (@def-pgm :navigate
       (input-course-message "LT. SULU")
     (state :input-course-check)
     (values))

   ;; check the validity of the course
   ;; display course
   ;; issue an invitation to enter the warp factor
   ;; switch context
   (@def-pgm :input-course-check
       (@clt "FSM" data)
     (setq *c1* (input-course-check (aref data 1)))
     (when *c1*
       (display ": ~a " *new-course*)
       (nav-factor-message (if (< (aref *ddd* 1) 0) 0.2 8))
       (state :nav-factor))
     (values))

   ;; perform attack script,
   ;; switch context to command input   
   (@def-pgm :nav-factor
       (@clt "FSM" data)
     (state :mloop-command)
     (setq *w1* (nav-factor (aref data 1)))
     (when *w1*
       (display ": ~a~%" *w1*)
       (setq *n* (nav-energy *w1*))
       (when *n* 
         (klingon-attack-warp)
         (repair-by-warp *w1*)
         (damage-by-warp)
         (cond  ((not (nav4 *new-course* *n* *w1*)) nil)
                (t (warp-time *w1*)))))
     (values))

   ;; the T command was previously given without a parameter
   ;; a prompt to enter a torpedo course is displayed
   ;; get the course from the reader
   ;; execute the command and display the result
   ;; switch the context to processing starship control commands
   (@def-pgm :torpedo-course
       (@clt "FSM" data)
     (setq *c1* (input-course-check (aref data 1)))
     (when *c1*
       (display ": ~a~%" *new-course*)
       (decf *energy* 2)
       (decf *torpedo* 1)
       (torpedo-fire *new-course*)
       (setq *klingon-attack* t))
     (state :mloop-command)
     (values))

   ;; the P command was previously given without a parameter
   ;; a prompt to enter a phaser units is displayed
   ;; get the units from the reader
   ;; execute the command PHASER and display the result
   ;; switch the context to processing starship control commands
   (@def-pgm :phaser
       (@clt "FSM" data)
     (display ": ~a~%" (aref data 1))
     (phaser4 (aref data 1))
     (state :mloop-command)
     (values))

   (@def-pgm :shield
       (@clt "FSM" data)
     (display ": ~a~%" (aref data 1))
     (shield (aref data 1))
     (state :mloop-command)
     (values))

   (@def-pgm :computer
       (@clt "FSM" data)
     (computer (aref data 1))
     (state :mloop-command)
     (values))

   (@def-pgm :comp-calc
       (@clt "FSM" data)
     (display ": ~a~%" (aref data 1))
     (setq *x0* (aref data 1))
     (setq *y0* (aref data 2))
     (display "FINAL COORDINATES X Y?")
     (state :comp-calc-final-co)
     (values))

   (@def-pgm :comp-calc-y
       (setq *y0* data)
     ;; note: wtf?
     ;; note: the state never use
     (format t "FINAL COORDINATES X?")
     (state :comp-calc-final-x)
     (values))

   (@def-pgm :comp-calc-final-x
       (setq *x1* data)
     ;; note: wtf?
     ;; note: the state never use
     (format t "Y?")
     (state :comp-calc-final-y)
     (values))

   (@def-pgm :comp-calc-final-co
       (display ": ~a~%" (list (aref data 1) (aref data 2)))
     (@clt "FSM" data)
     (setq *x1* (aref data 1))
     (setq *y1* (aref data 2))
     (disp-direct-dist *x0* *y0* *x1* *y1*)
     (state :mloop-command)
     (values))

   (@def-pgm :need-repair
       (@clt "FSM" data)
     (display "~a~%" (aref data 1))
     (cond ((eql 'y (aref data 1))
            (repair-all)
            (incf *time* (+ *d3-repair* 0.1))
            (show-stat-repair)))
     (state :mloop-command)
     (values))
   ))

;;; The FSM receives the command or command parameters from the reader
;;;     depending on the context, performs actions with this command
(defun fsm (data)
  (@clt "FSM receive:" data (aref data 1) (aref data 2) (aref data 3))
  (funcall (@exec-pgm *fsm-pgm
                      *state-fsm*
                      (lambda (&optional x) (error "FSM ABEND ~a ~a~&" *state-fsm* data)))
           data))

;;; subscribe FSM
(rx:listen :fsm (lambda (data) (fsm data)))


;;; EOF
