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

;;; Star Trek widget


;;; html stream
#|
#(STREAM #<FUNCTION> NIL NIL NIL NIL :OUT NIL)
   0         1       2   3   4   5   6    7
0 signature
1 write-fn rx:emit
2 read-char
3 peek-char
4 kind
5 data
6 direction :out
7 at-line-start
8 :html-stream
9 <pre>.innerHTML  for clear

|#

(defvar *so* nil)   ;; html stream from (stc/make-html-output-stream *stc-stream*)
(defvar *stc* nil)  ;; html.div id=stc-console 
(defvar *stc-stardate* nil)
(defvar *stc-location* nil)

(defun stc/make-html-output-stream (exists-pre-element)
  (let* ((buffer exists-pre-element)
         (fn-append ((jscl::oget buffer "appendChild" "bind") buffer))
         (fn-scroll ((jscl::oget buffer "scrollIntoView" "bind") buffer)))
    (vector 'stream                 ;; 0
            (lambda (string)        ;; 1
              (rx:emit :write-pre (list string fn-append fn-scroll)))
            nil                     ;; 2
            nil                     ;; 3
            nil                     ;; 4
            nil                     ;; 5
            :out                    ;; 6
            nil                     ;; 7
            :html-stream            ;; 8
            (lambda ()              ;; 9
              (setf (jscl::oget buffer "innerHTML") "")))))

(defun %write-to-html-stream (args)
  (let ((span (html::empty-span)))
    (ffi:setprop (span "innerHTML") (car args))
    ;; (setf (jscl::oget span "innerHTML") (car args))
    (funcall (second args) span)
    (funcall (third args) nil)))

(rx:listen :write-pre #'%write-to-html-stream)

;;; clear console
(defun stc/clear ()
  (if (arrayp *so*)
      (if (eq (aref *so* 8) :html-stream)
          (progn
            (funcall (aref *so* 9))
            (setf (aref *so* 7) nil)
            (return-from stc/clear (values))))
      (error "StarTrek: something went wrong with Console buffer")))

;;; terpri
(defun stc/terpri () (format *so* "~%"))
;;; hide
(defun stc/hide () (ffi:setprop (*stc* "style" "display") "none"))
;;; show
(defun stc/show () (ffi:setprop (*stc* "style" "display") "unset"))
;;; display quad name
(defun stc/quad (name)
  (ffi:setprop (*stc-location* "innerText") (jscl::concat "|" name "|")))
;;; display stardate
(defun stc/stardate (num-date)
  (ffi:setprop (*stc-stardate* "innerText") (string (roundnum num-date 4))))


(defvar *stc-header* nil)
#+nil (defvar *stc-stardate* nil)
#+nil (defvar *stc-location* nil)
(defvar *stc-uss* nil)
(defvar *stc-state* nil)
(defvar *stc-command* nil)
(defvar *stc-input* nil)
(defvar *stc-output* nil)
#+nil (defvar *so* nil)
(defvar *stc-stream* nil)
#+nil (defvar *stc* nil)

(html:declare-element label)
(html:declare-element pre)
(html:declare-element input)


(defparameter +stc-bord-css+
  (css:inline '(:border-style "groove" :border-width "1px" :border-radius "3px")))

(defparameter +stc-back-css+
  (css:inline  `(:background-color "rgba(78, 78, 70, 1.0)")))

(defparameter +stc-stardate-css+
  (css:inline `(:position "relative" :width "20px" :display "inline" :border "unset")))

(defparameter +stc-location-css+
  (css:inline `(:position "relative" :width "30px" :display "inline" :border "unset")))

(defparameter +stc-uss-css+
  (css:inline `(:position "relative" :width "50px" :display "inline" :border "unset")))

#|
:|style.ground-color| "#24502a"
:|style.color| "#28c428"
:|style.font-family| "Consolas"
:|style.font-size| "12px"
|#


(defun stc-regs-init ()
  (setq  *stc-header* nil
         *stc-stardate* nil
         *stc-location* nil
         *stc-uss* nil
         *stc-input* nil
         *stc-output* nil
         *so* nil
         *stc-stream* nil
         *stc* nil))

(defun console-init()
  (stc-regs-init)
  
  (setq *stc-uss*
        (html:span :|id| "stc-uss-id"
                   :style (css:inline `(:display "inline"
                                        :border-style "groove"
                                        :border-radius "10px"
                                        :padding-right "10px"
                                        :padding-left "10px"
                                        :background-color "#24502a"))
                   :append " USS ENTERPRISE NCC-1701 COMMAND CONSOLE "))

  (setq *stc-location*
        (html:span  :|id| "stc-location-id"
                    :|style.border-style| "groove"
                    :|style.border-radius| "10px"
                    :|style.padding-left| "10px"
                    :|style.padding-right| "10px"
                    :|style.border-right-style| "none"
                    :|style.border-top-right-radius| "unset"
                    :|style.border-bottom-right-radius| "unset"
                    :|style.margin-left| "40px"
                    ;;:|style.margin-right| "40px"
                    :|style.background-color| "#24502a"
                    :append " STARBASE DOCK "))

  (setq *stc-stardate*
        (html:span  :|id| "stc-stardate-id"
                    :|style.position| "relative"
                    :|style.border-bottom-style| "groove"
                    :|style.border-top-style| "groove"
                    :|style.border-radius| "10px"
                    :|style.border-left-style| "none"
                    :|style.border-top-left-radius| "unset"
                    :|style.border-bottom-left-radius| "unset"
                    :|style.padding-right| "10px"
                    :|style.background-color| "#24502a"
                    :append  "0000"))

  (setq *stc-header*
        (html:div :|id| "stc-header-id"
                  ;;:|style.background-color| "#24502a"
                  :|style.color| "#28c428"
                  :|style.font-family| "Consolas"
                  :|style.font-size| "12px"
                  :|style.padding-bottom| "5px"
                  :|style.margin-left| "23px"
                  :append *stc-uss*
                  :append *stc-location*
                  :append (html:span :|style.display| "inine"
                                     :|style.width| "20px"
                                     :|style.border-style| "groove"
                                     :|style.border-left-style| "none"
                                     :|style.border-right-style| "none"
                                     :append " STARDATE : ")
                  :append *stc-stardate*))

  #+nil
  (setq *stc-input*
        (html:textarea :|id| "stc-input-id"
                       :|style| (css:inline
                                 '(:color "#28c428"
                                   :background-color "#24502a"
                                   :resize "none"
                                   :display "inline-table"
                                   :autocapitalize "characters"
                                   :spellcheck "false"))
                       :|rows| "1"
                       :|cols| "55"))

  (setq *stc-input*
        (html:input :|id| "stc-input-id"
                       :|style| (css:inline
                                 '(:color "#28c428"
                                   :background-color "#24502a"
                                   :display "inline-table"))))

  (setq *stc-talk*
        (html:label :|id| "stc-talk-id"
                    :|style.background-color| "#24502a"
                    :|style.color| "#28c428"
                    :|style.border-style| "groove"
                    :|style.border-radius| "10px"
                    :|style.font-family| "Consolas"
                    :|style.font-size| "12px"
                    :|style.padding-left| "10px"
                    :|for| "stc-input-id"
                    :append "ENTER COMMAND  >"))

  (setq *stc-stream*
        (html:pre :|id| "stc-stream"
                  :|style.margin| "0px"
                  :|style.position| "relative"
                  :|style.min-height| "100%"
                  :|style.box-sizing| "border-box"))

  (setq *so* (stc/make-html-output-stream *stc-stream*))

  (setq *stc-output*
        (html:div :|id| "stc-output-id"
                  :|style.position| "relative"
                  :|style.left| "1px"
                  :|style.width| "600px"
                  :|style.height| "390px"
                  :|style.overflow| "auto"
                  :|style.border-style| "groove"
                  :|style.border-width| "10px"
                  :|style.border-radius| "12px"
                  :|style.color| "#b4e7a9"
                  :|style.background-color| "#081c0b"
                  :|style.font-family| "Consolas"
                  :|style.font-size| "13px"
                  :|style.margin-bottom| "4px"
                  :|style.margin-top| "4px"
                  :append *stc-stream*
                  ))

  (setq *stc*
        (html:div :|id| "stc-console"
                  :|style| (css:inline `(:position "absolute" :let "500px" :top "50px"
                                         :height "400px" :width "600px"))
                  :append *stc-header*
                  :append *stc-output*
                  :append *stc-talk*
                  :append *stc-input*
                  :append-to (html:document-body)))

  (%draggable *stc*)
  (stc/kb-reader-setup)  
  nil)

(defun %jq-on (jse event handler)
    (funcall ((jscl::oget jse "on" "bind") jse event handler)))

(defun stc/kb-reader-setup ()
  (%jq-on (#j:$ *stc-input*)
         "keyup"
              (lambda (event)
                ;;(print (list :event event))
                (stc/kb-handler event))))

;;; jquery features. just adds a class `draggable`to the element
(defun %draggable (ip)
  (let ((pvp (#j:$ ip)))
    (funcall ((jscl::oget pvp "draggable" "bind") pvp))))

;;; bug:
#+nil
(defun %draggable (ip)
  (let* ((pvp (#j:$ ip))
        (fn (ffi:getprop pvp "draggable")))
    (ffi:bind-call fn)))


;;; console reader
(defparameter reg-del (ffi:regexp "[\\n\\r]" "g"))

(defun stc/kb-handler (evt)
  ;;(#j:console:log "KBH" evt)
  ;;(#j:console:log "Read keycode" (ffi:getprop  evt "keyCode"))
  (let ((el (ffi:getprop evt "target"))
        (data)
        (from-value))
    (cond ((equal (ffi:getprop  evt "keyCode") 13)
           (setq data (ffi:|String|  (ffi:getprop *stc-input*  "value") "replace"  reg-del ""))
           (ffi:setprop (*stc-input* "value") "")
           (ffi:setprop (*stc-input* "value")
                        (ffi:|String| (ffi:getprop *stc-input*  "value") "replace"  reg-del ""))
           (rx:emit :fsm (stc/kbd-reader data)))
          ))
  (values))

(defun stc/kbd-reader (data)
  ;;(#j:console:log "kbr" data)
  (let ((stream (make-string-input-stream data))
        (sentinel (gensym "EOF"))
        (s)
        (res '()))
    (tagbody parser-fsm
     feeder
       (setq s (jscl::ls-read stream nil sentinel))
       (if (eql s sentinel) (go rdr-eof))
       (push s res)
       (go feeder)
     rdr-eof)
    (reverse res)))

;;; EOF
