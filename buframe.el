;;; buframe.el --- Local previews  -*- lexical-binding:t -*-
;; Version: 0.1
;; URL: https://github.com/hajiali/buframe
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Buframe provides utilities to create, manage, and update local child
;; frames, associated with a single buffer, for previews or inline overlays.
;; By default, these child frames are:
;; - Minimal (no mode-line, tool-bar, tab-bar, etc.)
;; - Non-focusable, non-disruptive, and dedicated to a buffer
;; - Dynamically positioned relative to overlays or buffer positions
;; - Automatically updated or hidden depending on buffer selection
;;
;; This package is designed to support UI components like popup
;; previews, completions, or inline annotations, without interfering
;; with normal Emacs windows or focus behaviour

(require 'cl-lib)

;;; Code:
(defvar buframe--default-buf-parameters
  '((mode-line-format . nil)
    (header-line-format . nil)
    (tab-line-format . nil)
    (tab-bar-format . nil) ;; Emacs 28 tab-bar-format
    (frame-title-format . "")
    (truncate-lines . t)
    (cursor-in-non-selected-windows . nil)
    (cursor-type . nil)
    (show-trailing-whitespace . nil)
    (display-line-numbers . nil)
    (left-fringe-width . nil)
    (right-fringe-width . nil)
    (left-margin-width . 0)
    (right-margin-width . 0)
    (fringes-outside-margins . 0)
    (fringe-indicator-alist . nil)
    (indicate-empty-lines . nil)
    (indicate-buffer-boundaries . nil)
    (buffer-read-only . t))
  "Default child frame buffer parameters for preview frames.")

(defvar buframe--default-parameters
  '((no-accept-focus . t)
    (no-focus-on-map . t)
    (min-width . t)
    (min-height . t)
    (border-width . 0)
    (outer-border-width . 0)
    (internal-border-width . 1)
    (child-frame-border-width . 1)
    (left-fringe . 0)
    (right-fringe . 0)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (tab-bar-lines . 0)
    (no-other-frame . t)
    (unsplittable . t)
    (undecorated . t)
    (cursor-type . nil)
    (no-special-glyphs . t)
    (desktop-dont-save . t))
  "Default child frame parameters for preview frames.")

(defvar buframe-update-debounce-delay 0.5
  "Delay in seconds before debounced frame update functions run.")

(defvar buframe--frame-mouse-ignore-map
  (let ((map (make-sparse-keymap)))
    (dotimes (i 7)
      (dolist (k '(mouse down-mouse drag-mouse double-mouse triple-mouse))
        (keymap-set map (format "<%s-%s>" k (1+ i)) #'ignore)))
    (when (boundp 'mouse-wheel--installed-bindings-alist)
      (cl-loop for (key . fun) in mouse-wheel--installed-bindings-alist
               do
               (define-key map key #'ignore)))
    map)
  "Ignore all mouse clicks.")

(defun buframe--right-visible (start end)
  "Return buffer position of the right-most column between START and END."
  (save-excursion
    (goto-char start)
    (let ((right (point))
          (cur-column (current-column)))
      (while (< (point) end)
        (goto-char (min (pos-eol) end))
        (when (and (> (current-column) cur-column)
                   (pos-visible-in-window-p))
          (setq right (point)
                cur-column (current-column)))
        (forward-line 1))
      right)))

(defun buframe-position-right-of-overlay (frame ov)
  "Return pixel position (X . Y) for FRAME, placed to the right of overlay OV."
  (when-let* ((buffer (overlay-buffer ov))
              (window (get-buffer-window buffer 'visible)))
    (let* ((posn
            (posn-at-point
             (with-current-buffer (overlay-buffer ov)
               (buframe--right-visible
                (overlay-start ov)
                (overlay-end ov)))
             window))
           (posn-start (posn-at-point (overlay-start ov) window))
           (posn-end (posn-at-point (overlay-end ov) window)))
      (when (or posn posn-start posn-end)
        (let* ((pframe-width (frame-pixel-width frame))
               (pframe-height (frame-pixel-height frame))
               (parent-frame (frame-parent frame))
               (xmax (frame-pixel-width parent-frame))
               (ymax (frame-pixel-height parent-frame))
               (x (+ (car (window-inside-pixel-edges window))
                     (default-font-width) ;; Add another character for the cursor
                     (- (or (car (posn-x-y (or posn posn-end posn-start))) 0)
                        (or (car (posn-object-x-y (or posn posn-end posn-start)))
                            0))))
               (top-xy (posn-x-y (or posn-start posn posn-end)))
               (bottom-xy (posn-x-y (or posn-end posn posn-start)))
               (font-height (default-font-height))
               (y-mid (/ (+ (cdr top-xy) (cdr bottom-xy) font-height) 2))
               (y-top (+ (cadr (window-pixel-edges window))
                         (or (frame-parameter frame 'tab-line-height) 0)
                         (or (frame-parameter frame 'header-line-height) 0)
                         (- (/ pframe-height 2))
                         y-mid)))
          (cons (max 0 (min x (- xmax (or pframe-width 0))))
                (max 0 (min y-top (- ymax (or pframe-height 0))))))))))

(defun buframe--make-buffer (name &optional locals)
  "Return a buffer with NAME configured for preview frames."
  (let ((fr face-remapping-alist)
        (ls line-spacing)
        (buffer (get-buffer-create name)))
    (with-current-buffer buffer
      ;;; XXX HACK from corfu install mouse ignore map
      (use-local-map buframe--frame-mouse-ignore-map)
      (dolist (vars (list buframe--default-buf-parameters locals))
        (dolist (var vars)
          (set (make-local-variable (car var)) (cdr var))))
      (setq-local face-remapping-alist (copy-tree fr)
                  line-spacing ls)
      buffer)))

(defun buframe--find (&optional frame-or-name buffer parent noerror)
  "Return frame displaying BUFFER with PARENT.
FRAME-OR-NAME can be a frame object or name.
If BUFFER is non-nil, restrict search to that buffer.
If PARENT is non-nil, restrict to frames with that parent.
If NOERROR is nil and no frame is found, signal an error."
  (or
   (if (framep frame-or-name)
       (and (frame-parameter frame-or-name 'buframe)
            frame-or-name)
     (cl-find-if
      (lambda (frame)
        (when-let ((buffer-info (frame-parameter frame 'buframe)))
          (and
           (or (null frame-or-name)
               (equal (frame-parameter frame 'name) frame-or-name))
           (or (null parent)
               (equal (frame-parent frame) parent))
           (or (null buffer)
               (equal (buffer-name buffer) (plist-get buffer-info :buf-name))
               (equal buffer (plist-get buffer-info :buf))))))
      (frame-list)))
   (unless noerror
     (error "Frame not found"))))

(cl-defun buframe-make (frame-or-name
                        fn-pos
                        buffer
                        &optional
                        (parent-buffer (window-buffer))
                        (parent-frame (window-frame))
                        parameters)
  "Create or reuse a child FRAME displaying BUFFER, positioned using FN-POS.

By default, the frame is configured to be minimal, dedicated,
non-focusable, and properly sized to its buffer. Positioning is
delegated to FN-POS. If an existing child frame matching
FRAME-OR-NAME and BUFFER exists, it is reused; otherwise, a new
one is created.

FRAME-OR-NAME is either the frame to reuse or its name.
FN-POS is a function called with the frame and overlay/position,
returning (X . Y).
BUFFER is the buffer to display in the child frame.
Optional PARENT-BUFFER and PARENT-FRAME default to the current
buffer and frame.
PARAMETERS is an alist of frame parameters overriding the
defaults."
  (let* ((window-min-height 1)
         (window-min-width 1)
         (inhibit-redisplay t)
         ;; The following is a hack from posframe and from corfu
         ;; (x-gtk-resize-child-frames corfu--gtk-resize-child-frames)
         (before-make-frame-hook)
         (after-make-frame-functions)
         (frame (buframe--find frame-or-name buffer nil t))
         (frm-params (cl-copy-list buframe--default-parameters)))
    (dolist (pair parameters frm-params)
      (setf (alist-get (car pair) frm-params nil t #'equal) (cdr pair)))

    (setq buffer (or (get-buffer buffer) buffer))
    (unless (and (bufferp buffer) (buffer-live-p buffer))
      (setq buffer (buframe--make-buffer buffer)))

    (if (and (frame-live-p frame)
             (eq (frame-parent frame)
                 (and (not (bound-and-true-p exwm--connection))
                      parent-frame))
             ;; If there is more than one window, `frame-root-window' may
             ;; return nil.  Recreate the frame in this case.
             (window-live-p (frame-root-window frame)))
        (progn
          ;; TODO: Should this always be done? Seems to be an overkill. But
          ;; some images do get out-of-cache requiring this and it needs to be
          ;; done before fitting/updating.
          (clear-image-cache frame)
          (force-window-update (frame-root-window frame)))
      (when frame (delete-frame frame))
      (setq frame (make-frame
                   `((name . ,frame-or-name)
                     (parent-frame . ,parent-frame)
                     (minibuffer . nil)
                     ;; (minibuffer . ,(minibuffer-window parent))
                     (width . 0) (height . 0) (visibility . nil)
                     ,@frm-params))))
    ;; Reset frame parameters if they changed.  For example `tool-bar-mode'
    ;; overrides the parameter `tool-bar-lines' for every frame, including child
    ;; frames.  The child frame API is a pleasure to work with.  It is full of
    ;; lovely surprises.
    (let* ((is (frame-parameters frame))
           (should frm-params)
           (diff (cl-loop for p in should for (k . v) = p
                          unless (equal (alist-get k is) v) collect p)))
      (when diff (modify-frame-parameters frame diff)))

    (let ((win (frame-root-window frame)))
      (unless (eq (window-buffer win) buffer)
        (set-window-buffer win buffer))
      ;; Disallow selection of root window (gh:minad/corfu#63)
      (set-window-parameter win 'no-delete-other-windows t)
      (set-window-parameter win 'no-other-window t)
      ;; Mark window as dedicated to prevent frame reuse (gh:minad/corfu#60)
      (set-window-dedicated-p win t)
      ;; Reset view to show the full frame.
      (set-window-hscroll win 0)
      (set-window-vscroll win 0))
    (set-frame-parameter frame
                         'buframe
                         (list
                          :buf-name (buffer-name buffer)
                          :buf buffer
                          :parent-buffer parent-buffer
                          :fn-pos fn-pos))
    (redirect-frame-focus frame parent-frame)
    (fit-frame-to-buffer frame)
    (buframe-update frame)
    ;; Unparent child frame if EXWM is used, otherwise EXWM buffers are drawn on
    ;; top of the Corfu child frame.
    (when (and (bound-and-true-p exwm--connection) (frame-parent frame))
      (set-frame-parameter frame 'parent-frame nil))
    frame))

(defun buframe-update (frame-or-name)
  "Reposition and show FRAME-OR-NAME using its stored positioning function.
Also ensure frame is made visible."
  (let* ((frame (buframe--find frame-or-name))
         (info (frame-parameter frame-or-name 'buframe))
         (fn-pos (plist-get info :fn-pos)))
    (when (and frame
               (frame-live-p frame)
               (not (buframe-disabled-p frame)))
      (with-current-buffer (plist-get info :parent-buffer)
        (if-let (pos (funcall fn-pos frame))
            (pcase-let ((`(,px . ,py) (frame-position frame))
                        (`(,x . ,y) pos))
              (unless (and (= x px) (= y py))
                (set-frame-position frame x y))
              (unless (frame-visible-p frame)
                (make-frame-visible frame)
                (add-hook 'post-command-hook 'buframe-autohide)
                (add-hook 'post-command-hook 'buframe-autoupdate nil t)))
          (buframe-hide frame))))))

(defun buframe-disabled-p (frame-or-name)
  "Return non-nil if FRAME-OR-NAME is disabled."
  (let ((frm (buframe--find frame-or-name)))
    (plist-get (frame-parameter frm 'buframe) :disabled)))

(defun buframe-disable (frame-or-name &optional enable)
  "Disable and hide FRAME-OR-NAME.
If ENABLE is non-nil, re-enable and show it."
  (when-let (frm (buframe--find frame-or-name))
    (when (frame-live-p frm)
      (set-frame-parameter
       frm 'buframe
       (plist-put
        (frame-parameter frm 'buframe)
        :disabled
        (not enable)))
      (if enable
          (buframe-update frm)
        (buframe-hide frm)))))

(defun buframe-hide (frame-or-name)
  "Make FRAME-OR-NAME invisible."
  (when-let (frm (buframe--find frame-or-name))
    (when (and (frame-live-p frm)
               (frame-visible-p frm))
      (make-frame-invisible frm)))
  (unless
      (cl-find-if
       (lambda (frame)
         (and (frame-parameter frame 'buframe)
              (frame-live-p frame)
              (frame-visible-p frame)))
       (frame-list))
    (remove-hook 'post-command-hook 'buframe-autohide)))

(defmacro buframe--defun-debounced (name arglist &rest body)
  "Define a debounced function NAME.
ARGLIST may include `&delay SECONDS` to specify idle delay.
BODY is executed after SECONDS of idle time since last call.

Note that the current buffer and window might be different
compared to when the function is called."
  (declare (doc-string 3) (indent defun))
  (let* ((delay-pos (cl-position '&delay arglist))
         ;; TODO: Consider a better method if you can be bothered
         ;; TODO: If DELAY is not specified issue a warning.
         (delay (nth (1+ delay-pos) arglist))
         (real-args (cl-remove-if (lambda (x) (memq x '(&delay))) arglist))
         (real-args (remove delay real-args))
         (timer-var (intern (format "%s--debounce-timer" name)))
         (parsed (macroexp-parse-body body)))
    `(progn
       (defvar ,timer-var nil)
       (defun ,name (&rest args)
         ,@(car parsed) ;; docstring, declare, interactive
         (when ,timer-var
           (cancel-timer ,timer-var))
         (if (and ,delay (> ,delay 0))
             (let* ((def (lambda ,real-args
                           ,@(car parsed)
                           (setq ,timer-var nil)
                           ;; TODO: Figure out a way to allow the user to
                           ;; preserve current window/buffer... etc
                           ,@(cdr parsed))))
               (setq ,timer-var
                     (apply
                      'run-with-idle-timer
                      ,delay nil
                      def
                      args)))
           (let ((def (lambda ,real-args
                        ,@(cdr parsed))))
             (apply def args)))))))

(defun buframe-autohide (&optional frame-or-name)
  "Hide FRAME-OR-NAME if its parent buffer is not selected."
  (buframe--auto* frame-or-name 'buframe-hide 'not-parent))

(buframe--defun-debounced
  buframe-autoupdate ( ;
                        &optional frame-or-name
                        &delay buframe-update-debounce-delay)
  "Update FRAME-OR-NAME if its parent buffer is currently selected."
  (buframe--auto* frame-or-name 'buframe-update 'parent))

(defun buframe--auto* (frame-or-name fn buffer)
  "Run FN on FRAME-OR-NAME based on BUFFER selection rules.

If FRAME-OR-NAME is nil, run FN on all buframes.
BUFFER can be:
  \\='parent      – run only if parent buffer is current
  \\='not-parent  – run only if parent buffer is not current
  a buffer     – run only if BUFFER is current."
  (if frame-or-name
      (when-let (frame (buframe--find frame-or-name))
        (let ((is-parent (equal (window-buffer)
                                (plist-get (frame-parameter frame 'buframe)
                                           :parent-buffer))))
          (when (or (and (eq buffer 'parent) is-parent)
                    (and (eq buffer 'not-parent) (not is-parent))
                    (equal (window-buffer) buffer))
            ;; If buffer is not selected, we should hide the frame
            (funcall fn frame))))
    (cl-mapc
     (lambda (frame)
       (when-let ((buffer-info (frame-parameter frame 'buframe)))
         (buframe--auto* frame fn buffer)))
     (frame-list))))

(provide 'buframe)
;;; buframe.el ends here
