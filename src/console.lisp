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

(html:declare-element label)
(html:declare-element pre)

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
<div id="stc-console"
class="ui-draggable ui-draggable-handle"
style="position: absolute; left: 284.987px; top: 40.9957px;
       height: 400px; width: 600px; border-width: 1px; border-radius: 3px;">

<div id="stc-header-id"
style="position:relative;left:1px;width:600px;
     /* border-style:groove; */
     border-width: 12px;
     /* border-width: 2px; */
border-radius: 42px;
background-color:#24502a;
color:#28c428;
font-family:Consolas;
font-size:12px;">

<span id="stc-uss-id"
"position:relative
left:1px
width:600px;
border-style:groove;
border-width: 12px;
border-radius: 4px;
ground-color: #24502a;
color:#28c428;
font-family:Consolas;
font-size:12px;">

USS ENTERPRISE NCC-1701 command console </span>



<span id="stc-location-id"
style="position:relative;width: 115px;
border-style: groove;border-radius: 12px;display:inline;
/* border:unset; */">|     STARBASE DOCK      |</span>

<span> STARDATE </span>

<span id="stc-stardate-id"
style="position:relative; width:20px; display:inline; border:unset; ">0000</span>

|</div>

<div id="stc-stream-id"

style="

:position:relative;
left:1px;
width:600px;
height:390px;
border-style: ridge;
overflow:auto;
border-radius: 31px;
color:#19ec34;
background-color:rgba(0, 0, 0, 1.0);
font-family:Consolas;
font-size:13px;"

></div>

<div style="
    border-style: groove;
    background-color: green;
    border-radius: 10px;
">

<label id="stc-talk-id"
style="background-color: green;baorder-style: groove;">Wait you command&gt;</label>

<textarea id="stc-input-id"

style="color:black; background-color:rgba(232, 232, 87, 1.0); resize:none; display:inline-table; " rows="1" cols="50" autocapitalize="characters" spellcheck="true"></textarea></div></div>

|#


#|
:|style.ground-color| "#24502a"
:|style.color| "#28c428"
:|style.font-family| "Consolas"
:|style.font-size| "12px"
|#


#|
(defparameter +stc-header-css+
  (css:inline `(:position "relative" :left "1px" :width "600px"
                :border-style "groove" :border-width "1px" :border-radius "3px"
                :background-color "#24502a" :color "#28c428"
                :font-family "Consolas" :font-size "12px")))

(defparameter +stc-input-css+
  (css:inline `(:color "black" :background-color "rgba(232, 232, 87, 1.0)"
                       :resize "none"
                       :display "inline-table")))

(defparameter +stc-output-css+
  (css:inline  `(:position "relative" :left "1px" :width "600px" :height "390px"
                 :overflow "auto"
                 :color "#19ec34" :background-color "rgba(0, 0, 0, 1.0)"
               :font-family "Consolas" :font-size "13px")))
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
                                        :border-radius: "10px"
                                        :padding-right "10px"
                                        :padding-left "10px"
                                        :background-color "#24502a"))
                   :append " USS ENTERPRISE NCC-1701 COMMAND CONSOLE ")))

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
                    :|style.margin-right| "40px"
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

  (setq *stc-output*
        (html:div :|id| "stc-output-id"
                  :|style.position| "relative"
                  :|style.left| "1px"
                  :|style.width| "600px"
                  :|style.height| "390px"
                  :|style.overflow| "auto"
                  :|style.border-style| "groove"
                  :|style.border-radius| "31px"
                  :|style.color| "#28c428"
                  :|style.background-color| "#24502a"
                  :|style.font-family| "Consolas"
                  :|style.font-size| "13px"
                  :|style.margin-bottom| "4px"
                  :|style.margin-top| "4px"
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
