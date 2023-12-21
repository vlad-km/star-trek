;;; -*- mode:lisp; coding:utf-8  -*-


(defvar *stc-header*)
(defvar *stc-stardate*)
(defvar *stc-location*)
(defvar *stc-state*)
(defvar *stc-command*)
(defvar *stc-input*)
(defvar *stc-output*)
(defvar *so*)
(defvar *stc-stream*)
(defparameter *stc-bord* (viewport:make-border :style "groove" :width "1px" :radius "3px"))
(defparameter *stc-back* (viewport:make-background :color "rgba(78, 78, 70, 0.431373)"))

(defvar *stc*)

(defun st-console-init()
    (setq *stc-stardate*
          (html:span
           :id (klib:gen-uid "stc" "stardate")
           :style "position:relative;width:20px;display:inline;"
           :text "0000"))

    (setq *stc-location*
          (html:span
           :id "stc-location-id"
           :style "position:relative;width:30px;display:inline;"
           :text "| STARBASE DOCK |"))

    (setq *stc-header*
          (html:div
           :id (klib:gen-uid "stc" "header")
           :style (concat
                   "position:relative;left:1px;width:600px;"
                   "border-style:groove;border-width:1px;border-radius:3px;"
                   "background-color:rgba(0,0,0,0.8);color:green")
           :text "USS ENTERPRISE command console | NCC-1701 "))

    (dom:mount *stc-header*
               *stc-location*
               " STARDATE: "
               *stc-stardate*)

    (setq *stc-command*
          (html:div
           :id (klib:gen-uid "stc" "command")
           :style
           (concat
            ;;"display:inline-block;"
            "background-color:rgba(0, 0, 0, 0.8);")))

    (setq *stc-state*
          (html:div
           :id (klib:gen-uid "stc" "state")
           :style
           (concat
            ;;"display:inline-block;"
            "width:" "40px;"
            "font-family:Consolas;"
            "font-size:13px;"
            "color:" "#19ec34" )
           :text "COMMAND:"))

    (setq *stc-input*
          (html:textarea
           :id (concat (string (gensym "talk")) "-id")
           :color "black"
           :style "background-color:rgba(232, 232, 87, 0.38);"
           :rows "1"
           ;;:cols "120"
           :spellcheck "false"))

    (dom:mount *stc-command* *stc-state* *stc-input*)

    (setq *stc-output*
          (html:div
           :id (klib:gen-uid "stc" "output")
           :style
           (concat
            "position:relative;left:1px;width:600px;height:390px;overflow:auto;"
            "background-color:rgba(0, 0, 0, 0.8);"
            "font-family:Consolas;"
            "font-size:13px;"
            "color:" "#19ec34")))

    (setq *stc-stream*
          (dom:make-dom-output-stream
           :name (concat (string (gensym "stream")) "-id")))

    (setq *so* *stc-stream*)

    (dom:output-stream-open *stc-stream* *stc-output*)


    (setq *stc*
          (viewport:create :left 500
                           :top 50
                           :height 400
                           :width 600
                           :drag t
                           :border *stc-bord*
                           :background *stc-back*
                           :childs '(*stc-header* *stc-output* *stc-command*)
                           :parent dom:*body*))

    (setq *stc-location* (dom:get-element-by-id "stc-location-id"))

    (jq:on (jq:$ *stc-input*) "keyup"
           (lambda (event) (stc/kb-handler event))) )


;;; hide
(defun stc/hide ()
    (viewport:hide *stc*))

;;; show
(defun stc/show ()
    (viewport:show *stc*))

;;; display quad name
(defun stc/quad (name)
    (dom:set-inner-html *stc-location* (concat "| " name " |")))

;;; display stardate
(defun stc/stardate (date)
    (dom:set-inner-html *stc-stardate* (roundnum date 4)))


;;; terpri
(defun stc/terpri ()
    (format *so* "~%"))

;;; clear console
(defun stc/clear ()
    (dom:output-stream-reset *stc-stream*))


(defparameter reg-del (reg-exp "[\\n\\r]" "g"))

;;; reader
(defun stc/kb-handler (evt)
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


(defun stc/kbd-reader (data)
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
