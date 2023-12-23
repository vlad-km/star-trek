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


(defvar *stc-header* nil)
(defvar *stc-stardate* nil)
(defvar *stc-location* nil)
(defvar *stc-uss* nil)
(defvar *stc-state* nil)
(defvar *stc-command* nil)
(defvar *stc-input* nil)
(defvar *stc-output* nil)
(defvar *so* nil)
(defvar *stc-stream* nil)
(defvar *stc* nil)

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


+stc-header-css+
(defparameter +stc-header-css+
  (css:inline `(:position "relative" :left "1px" :width "600px"
                :border-style "groove" :border-width "1px" :border-radius "3px"
                :background-color "#24502a" :color "#28c428"
                :font-family "Consolas" :font-size "12px")))

(defparameter +stc-input-css+
  (css:inline `(:color "black" :background-color "rgba(232, 232, 87, 1.0)")))

(defparameter +stc-output-css+
  (css:inline  `(:position "relative" :left "1px" :width "600px" :height "390px"
                 :overflow "auto"
                 :color "#19ec34" :background-color "rgba(0, 0, 0, 1.0)"
                 :font-family "Consolas" :font-size "13px")))


(defun stc-console-init()
  (setq *stc-uss*
        (html:span :|id| "stc-uss-id"
                   :style +stc-header-css+
                   :append "  USS ENTERPRISE NCC-1701 command console "))

    (setq *stc-location*
        (html:span  :|id| "stc-location-id"  :style +stc-location-css+
                    :append "|     STARBASE DOCK      |"))

  (setq *stc-stardate*
        (html:span  :|id| "stc-stardate-id"  :style +stc-stardate-css+
                    :append  "0000"))

  (setq *stc-header*
        (html:div :|id| "stc-header-id"
                  :style +stc-header-css+
                  :append *stc-uss*
                  :append *stc-location*
                  :append (html:span :append " STARDATE ")
                  :append *stc-stardate*
                  :append " |"))

  (setq *stc-command*
        (html:div  :|id| "stc-command-id"
                   :style (css:inline '(:background-color "rgba(0, 0, 0, 1.0)"
                                        :font-family "Consolas"
                                        :font-szie "13px" :color "#19ec34" ))))

  (setq *stc-state*
        (html:div :|id| "stc-state-id"
                  :style  (css:inline `(:width "40px" :color "black"
                                        :font-family "Consolas"
                                        :font-szie "13px" :color "#19ec34"))))
  (setq *stc-input*
        (html:textarea :|id| "stc-talk-id"
                       :style +stc-input-css+ 
                       :|rows| "1"
                       :|spellcheck| "false"))


  (setq *stc-output*
        (html:div :|id| "stc-stream-id"  :style +stc-output-css+))
  

  (setq *stc*
        (html:div :|id| "stc-console"
                  :|style| (css:inline `(:position "absolute" :left "500px" :top "50px"
                                         :height "400px" :width "600px"
                                         :border ,+stc-bord-css+
                                         :background ,+stc-back-css+))
                  :append *stc-header*
                  :append *stc-output*
                  :append *stc-input*
                  :append-to (html:document-body)))
  
  (let ((pvp (#j:$ *stc* )))
    (funcall ((jscl::oget pvp "draggable" "bind") pvp)))
  nil)


;;; hide
(defun stc/hide () (ffi:setprop (*stc* "style" "display") "none"))
;;; show
(defun stc/show () (ffi:setprop (*stc* "style" "display") "unset"))
;;; display quad name
(defun stc/quad (name) (%set-inner-text *stc-location* (jscl::concat "|  " name "  |")))
;;; display stardate
(defun stc/stardate (date)(%set-inner-text *stc-stardate* (roundnum date 4)))


;;; terpri
#+nil (defun stc/terpri ()
    (format *so* "~%"))

;;; clear console
#+nil (defun stc/clear ()
    (output-stream-reset *stc-stream*))


#+nil (defparameter reg-del (reg-exp "[\\n\\r]" "g"))

;;; reader
#+nil (defun stc/kb-handler (evt)
    (let ((el (oget evt "target"))
          (data))
        (cond ((equal (oget evt "keyCode") 13)
               (setq data (jstring:replace (oget *stc-input*  "value") reg-del ""))
               (setf (oget *stc-input* "value") "")
               (setf (oget *stc-input* "value")
                     (jstring:replace (oget *stc-input*  "value") reg-del ""))
               (mordev:rx-emit :fsm (stc/kbd-reader data)))
              ))
    (values) )


#+nil (defun stc/kbd-reader (data)
    (let ((stream (jscl::make-string-stream data))
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
