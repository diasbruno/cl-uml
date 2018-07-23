;;;; cl-uml.lisp

(in-package #:cl-uml)

(export 'main)

(defvar w 512)
(defvar h 512)


(defun render->button (context &key (x 0) (y 0) (w 0) (h 0))
  (cl-cairo2:set-source-rgba 1 1 1 1 context)
  (cl-cairo2:rectangle x y w h context)
  (cl-cairo2:fill-path context)
  ;; (cl-cairo2:set-source-rgba 0 0 0 1 context)
  ;; (cl-cairo2:set-line-width 1 context)
  ;; (cl-cairo2:stroke context)
  )

(defun render->label-centered (text context &key (x 0) (y 0) (w 0) (h 0))
  (let ((extents nil))
    (setf extents (cl-cairo2:get-text-extents text context))
    (cl-cairo2:set-source-rgba 0 0 0 1 context)
    (cl-cairo2:move-to (- (+ x (* w 0.5))
                          (* (cl-cairo2:text-width extents) 0.5))
                       (- (+ y (* h 0.5))
                          (* (cl-cairo2:text-y-bearing extents) 0.5))
                       context)
    (cl-cairo2:show-text text context)))

(defun render->labeled-button (text context &key (x 0) (y 0) (w 0) (h 0))
  (render->button context :x x :y y :w w :h h)
  (render->label-centered text context :x x :y y :w w :h h))

(defun render (w h surface context)
  (cl-cairo2:set-source-rgba 0 0.5 0.5 1 context)
  (cl-cairo2:rectangle 0 0 w h context)
  (cl-cairo2:fill-path context)
  (render->labeled-button "utf8" context :x 200 :y 200 :w 200 :h 32))

(defun main ()
  (let* ((display (xcb.clx:open-display ""))
         (screen nil)
         (xwin nil))
    (xcb.clx:with-display
     display
     (let ((event-mask (xcb.clx:make-event-mask :exposure
                                                :key-press
                                                :key-release
                                                :button-press
                                                :button-release
                                                :enter-window
                                                :leave-window
                                                :pointer-motion
                                                :pointer-motion-hint
                                                :structure-notify)))
       (setf screen (nth 0 (xcb.clx:display-roots display)))
       (setf xwin (xcb.clx:create-window :parent (xcb.clx:screen-root screen)
                                         :x 0
                                         :y 0
                                         :width w
                                         :height h
                                         :event-mask event-mask))
       (xcb.clx:change-property xwin
                                :wm_protocols
                                (list (xcb.clx:find-atom display
                                                         :wm_delete_window))
                                :atom 32))
     (xcb.clx:map-window xwin)
     (xcb.clx:display-force-output display))
    (let* ((surface (cl-cairo2:create-xcb-surface xwin))
           (context (cl-cairo2:create-context surface)))
      (cl-cairo2:select-font-face "monospace" :normal :normal context)
      (cl-cairo2:set-font-size 12.0 context)
      (unwind-protect
          (xcb.clx:event-case
           (display :force-output-p t :discard-p t)
           (:exposure (count)
                      (when (= 0 count)
                        (funcall #'render w h surface context)))
           (:pointer-motion ()
                            (format t "pointer-motion ~%")
                            (funcall #'render w h surface context))
           (:button-press (x y)
                          (format t "button-press x: ~a y: ~a~%" x y)
                          (funcall #'render w h surface context))
           (:button-release (x y)
                            (format t "button-release ~%")
                            (funcall #'render w h surface context))
           (:key-press (window code)
                       (let ((ch (xlib:keysym->character
                                  display
                                  (xlib:keycode->keysym display code 0))))
                         (format t "key-press ~a~%" ch)
                         (funcall #'render w h surface context)))
           (:key-release (window code)
                         (let ((ch (xlib:keysym->character
                                    display
                                    (xlib:keycode->keysym display
                                                          code 0))))
                           (progn
                             (format t "key-press ~a~%" ch)
                             (case ch
                                 (#\q t)
                                 (t (funcall #'render w h surface context))))))
           (:resize-redirect (width height)
                             (setf w width)
                             (setf h height)
                             (funcall #'render w h surface context))
           (:configure-notify (height width)
                              (progn
                                (format t "configure-notify ~a ~a~%" width height)
                                (setf w width)
                                (setf h height)
                                (cl-cairo2:xcb-surface-set-size surface width height)
                                (funcall #'render width height surface context)))
           (:client-message (type data)
                            (format t "client-message ~a~%" type)
                            (and (eq type :wm_protocols)
                                 (= (elt data 0)
                                    (xcb.clx:find-atom display
                                                       :wm_delete_window)))))
        (cairo:destroy surface)
        (xcb.clx:destroy-window xwin)
        (xcb.clx:close-display display)))))
