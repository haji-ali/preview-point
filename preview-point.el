;;; preview-point.el --- Local previews around point for AUCTeX preview -*- lexical-binding:t -*-

;; Author: Al Haji-Ali <abdo.haji.ali@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (auctex "13.2"))
;; Keywords: tex, preview, convenience
;; URL: https://github.com/hajiali/preview-point

;;; Commentary:

;; This package provides an alternative preview mechanism for AUCTeX's
;; `preview-latex', displaying previews of LaTeX fragments directly around
;; point. Unlike the default preview overlay system, this can use lightweight
;; child frames (via buframe) or overlay strings to show previews inline.
;;
;; Features:
;; - Previews shown only when point enters overlays.
;; - Configurable display method: overlay before-string, after-string,
;;   or buframe popup.
;; - Disabled and "under construction" previews shown with indicators.
;; - Automatic handling of buffer changes and point motion.
;; - Advice over `preview-toggle' and related internals to integrate
;;   seamlessly with AUCTeX.
;;
;; To enable, load this file and ensure `preview-mode' is active. Customise
;; `preview-point-show-in' and related variables for display style.

;;; Code:

(require 'preview)
(require 'texmathp)
(require 'preview-dvi)
(require 'buframe)

(defgroup preview-point nil
  "Display previews around point using overlays or child frames."
  :group 'preview)

(defcustom preview-point-show-in 'after-string
  "Specifies where to show the preview.
Can be `before-string', `after-string' or `buframe'. Can also be
\\='(buframe FN-POS FRAME-PARAMETERS BUF-PARAMETERS) where FN-POS
is a position function (default is
`buframe-position-right-of-overlay') and FRAME-PARAMETERS is an
alist of additional frame parameters, default is nil and
BUF-PARAMETERS is an alist of buffer local variables and their
values."
  :type '(choice
          (const :tag "Before string" before-string)
          (const :tag "After string (default)" after-string)
          (const :tag "Buframe" buframe)
          (list :tag "Buframe with parameters"
                (function :tag "Position function")
                (alist :tag "Frame parameters")
                (alist :tag "Buffer parameters"))))

(defcustom preview-point-auto-p 'texmathp
  "Function to test if a point should be previewed automatically.
This is called with no arguments at (point) when there is not a
preview already."
  :type 'symbol)

(defcustom preview-point-progress-indicators 'icons
  "What to show for progress indication.

Can be:
- `icons' : use preview icons.
- `faces' : use faces for highlighting.
Any other value (including nil) disables progress display."
  :type '(choice
          (const :tag "Icons (default)" icons)
          (const :tag "Faces" faces)))

(defface preview-point-disabled-face
  '((t (:inherit shadow)))
  "Face used when preview is disabled."
  :group 'preview-point)

(defface preview-point-processing-face
  '((t (:inherit preview-point-disabled-face)))
  "Face used when preview is processing."
  :group 'preview-point)

(defvar preview-point--frame nil
  "The last active preview popup frame.")
(defvar preview-point--frame-overlay nil
  "The overlay currently shown in the preview popup frame.")

(defun preview-point-popup-frame (ov str)
  "Show overlay OV displaying STR in a buframe popup at PT.
If PT is nil, use the current point in OV's buffer."
  (let* ((buf (buframe--make-buffer
               " *preview-point-buffer*"
               (car-safe (cddr (cdr-safe preview-point-show-in)))))
         (max-image-size
          (if (integerp max-image-size)
              max-image-size
            ;; Set the size max-image-size using the current frame
            ;; since the popup frame will be small to begin with
            (* max-image-size (frame-width)))))
    (with-current-buffer buf
      ;;(get-buffer-create buf)
      (let (buffer-read-only)
        (with-silent-modifications
          (erase-buffer)
          (insert (propertize str
                              ;;'parent-overlay ov
                              'help-echo nil
                              'keymap nil
                              'mouse-face nil))
          (goto-char 0))))
    (funcall 'buframe-make
             "preview-point"
             (lambda (frame)
               (when preview-point--frame-overlay
                 (funcall
                  (or (car-safe (cdr-safe preview-point-show-in))
                      'buframe-position-right-of-overlay)
                  frame preview-point--frame-overlay)))
             buf
             (overlay-buffer ov)
             (window-frame)
             (car-safe (cdr (cdr-safe preview-point-show-in))))))

(defun preview-point-inside-overlay-p (ov &optional pt)
  "Return PT if inside overlay OV, nil otherwise.
If PT is nil, use point of OV's buffer."
  (let ((pt (or pt
                (and
                 (overlay-buffer ov)
                 (with-current-buffer (overlay-buffer ov)
                   (point))))))
    (and
     (equal (overlay-buffer ov) (window-buffer))
     (>= pt (overlay-start ov))
     (< pt (overlay-end ov))
     pt)))

(defun preview-point-p ()
  "Return non-nil if current preview type is a point-preview."
  (memq preview-image-type '(point-dvisvgm
                             point-dvipng)))

(defun preview-point-disable (ovr)
  "Disable overlay OVR after source edits.

Adapted from `preview-disable', without the file deletion."
  ;; Do not reset queued, a disabled image will be shown anyways.
  ;; More importantly, resetting queued will orphan files if a conversion
  ;; process is underway.
  ;; (overlay-put ovr 'queued nil)
  (preview-remove-urgentization ovr)
  (unless preview-leave-open-previews-visible
    (overlay-put ovr 'preview-image nil))
  (setcdr (overlay-get ovr 'strings) nil)
  (overlay-put ovr 'timestamp nil)
  (overlay-put ovr 'preview-state 'disabled)
  (preview-point-toggle ovr t))

(defun preview-point-check-changes ()
  "Check whether the contents under the overlay have changed.
Disable it if that is the case.  Ignores text properties.

Adapted from `preview-check-changes' to call
`preview-point-disable' instead of `preview-disable'."
  (dolist (ov preview-change-list)
    (condition-case nil
        (with-current-buffer (overlay-buffer ov)
          (let ((text (save-restriction
                        (widen)
                        (buffer-substring-no-properties
                         (overlay-start ov) (overlay-end ov)))))
            (if (zerop (length text))
                (preview-delete ov)
              (unless
                  (or (eq (overlay-get ov 'preview-state) 'disabled)
                      (preview-relaxed-string=
                       text (overlay-get ov 'preview-prechange)))
                (overlay-put ov 'insert-in-front-hooks nil)
                (overlay-put ov 'insert-behind-hooks nil)
                (preview-point-disable ov)))))
      (error nil))
    (overlay-put ov 'preview-prechange nil))
  (setq preview-change-list nil))

(defun preview-point-toggle (ov &optional arg event show-construct)
  "Toggle visibility of preview overlay OV.
ARG can be one of the following: t activate the preview, nil
deactivate the preview, `toggle' toggles. If the preview is
disabled, the disabled symbol is shown when activated. If EVENT
is given, it indicates the window where the event occurred,
either by being a mouse event or by directly being the window in
question. This may be used for cursor restoration purposes.
SHOW-CONSTRUCT forces showing under-construction previews."
  (let* ((old-urgent (preview-remove-urgentization ov))
         (pt (and
              (overlay-buffer ov)
              (with-current-buffer (overlay-buffer ov)
                (point))))
         (strings (overlay-get ov 'strings))
         (disabled-p (eq (overlay-get ov 'preview-state) 'disabled))
         (construct-p (eq (overlay-get ov 'preview-point-state)
                          'under-construction))
         (frame-p
          (or
           (eq preview-point-show-in 'buframe)
           (eq (car-safe preview-point-show-in) 'buframe)))
         (inside-p (preview-point-inside-overlay-p ov pt))
         (str (car strings)))
    ;; Update `strings' cdr, if needed
    (when (or (and construct-p show-construct) disabled-p)
      (unless (cdr (overlay-get ov 'strings))
        (setcdr (overlay-get ov 'strings)
                (cond
                 (disabled-p
                  (pcase preview-point-progress-indicators
                    ('icons (preview-disabled-string ov))
                    ('faces (propertize str
                                        'face
                                        'preview-point-disabled-face))))
                 (construct-p
                  (pcase preview-point-progress-indicators
                    ('icons
                     (propertize "x" 'display preview-nonready-icon))
                    ('faces (propertize str
                                        'face
                                        'preview-point-processing-face)))))))
      (setq str (cdr (overlay-get ov 'strings))))

    (unless disabled-p
      (overlay-put ov 'preview-state
                   (if
                       (and arg inside-p (or (not construct-p)
                                             show-construct))
                       'active
                     'inactive)))

    (when (or (not construct-p) show-construct)
      (if (and arg inside-p (or (not construct-p)
                                show-construct))
          (progn
            (overlay-put ov 'category 'preview-overlay)
            ;; (unless (or (null preview-point--frame-overlay)
            ;;             (equal preview-point--frame-overlay ov))
            ;;   (message "SHOWING OVERALY %S BEFORE HIDING %S"
            ;;            ov preview-point--frame-overlay))
            (setq preview-point--frame-overlay ov)
            (if frame-p
                (setq
                 preview-point--frame
                 (preview-point-popup-frame ov str))
              (overlay-put ov preview-point-show-in str)))
        (when (equal preview-point--frame-overlay ov)
          ;; (unless (equal preview-point--frame-overlay ov)
          ;;   (message "HIDING %S WHILE %S SHOWN"
          ;;            ov preview-point--frame-overlay))
          (if frame-p
              (when (and preview-point--frame
                         ;; Only hide the frame if this is the shown overlay.
                         (equal preview-point--frame-overlay ov))
                (buframe-disable preview-point--frame))
            (overlay-put ov preview-point-show-in nil))
          (setq preview-point--frame-overlay nil))))
    (when old-urgent
      (apply #'preview-add-urgentization old-urgent)))
  (if event
      (preview-restore-position
       ov
       (if (windowp event)
           event
         (posn-window (event-start event))))))

(defun preview-point-mark-point ()
  "Mark point for fake intangibility in preview overlays."
  (unless (preview-point-p)
    (preview-mark-point)))

(defun preview-point-move-point ()
  "Move point outside fake-intangible preview overlays.
Toggle previews as point enters or leaves overlays."
  (if (not (preview-point-p))
      (preview-move-point)
    (preview-point-check-changes)
    (let* ((pt (point))
           (lst (overlays-at pt)))
      ;; Hide any open overlays
      (when-let (ov preview-point--frame-overlay)
        (and (overlay-buffer ov)
             (overlay-get ov 'preview-state)
             (not (eq (overlay-get ov 'preview-state) 'inactive))
             (when (not (preview-point-inside-overlay-p
                         ov pt))
               (preview-point-toggle ov nil))))

      ;; Show all overlays under point
      (dolist (ovr lst)
        (let ((state (overlay-get ovr 'preview-state)))
          (when ;; (eq (overlay-get ovr 'preview-state) 'inactive)
              (and (and state (not (eq state 'active)))
                   (not (equal ovr preview-point--frame-overlay)))
            (preview-point-toggle ovr t)))))))

(defun preview-point-place (place-fn ov &rest args)
  "Place preview function for `preview-point'.
Calls PLACE-FN with arguments (OV ARGS) and marks the overlay OV
as under-construction."
  (let ((ovl (apply place-fn ov args)))
    (dolist (ov ovl)
      (overlay-put ov 'preview-point-state 'under-construction))
    ovl))

(defun preview-point-close (close-fn process closedata)
  "Close preview function for `preview-point'.
Calls CLOSE-FN with arguments (PROCESS CLOSEDATA) and shows
overlays in CLOSEDATA after."
  (prog1 (funcall close-fn process closedata)
    (cl-mapc
     (lambda (ov)
       ;; Show preview, will be ignored if point is not on it
       (preview-point-toggle ov t nil t))
     closedata)))

(defun preview-point-setup ()
  "Set up hooks for preview-point in the current buffer."
  (remove-hook 'pre-command-hook #'preview-mark-point t)
  (remove-hook 'post-command-hook #'preview-move-point t)
  (add-hook 'pre-command-hook #'preview-point-mark-point nil t)
  (add-hook 'post-command-hook #'preview-point-move-point nil t)
  (add-hook 'preview-dvi-after-place-hook 'preview-point-after-place))

(defun preview-point@around@preview-toggle (old-fn &rest args)
  "Advice around `preview-toggle' to conditionally use `preview-point-toggle'.
OLD-FN is the `preview-toggle' and ARGS are all of its arguments."
  (if (with-current-buffer TeX-command-buffer
        (preview-point-p))
      (apply 'preview-point-toggle args)
    (apply old-fn args)))

(defun preview-point-after-place (ovl)
  "Handle overlays OVL after placing previews."
  (when (with-current-buffer TeX-command-buffer
          (preview-point-p))
    (cl-loop for ov in ovl
             for filename = (caar (overlay-get ov 'filenames))
             when (and filename (file-exists-p filename))
             do
             ;; No longer under construction
             (overlay-put ov 'preview-point-state nil)
             ;; Show preview, will be ignored if point is not on it
             (preview-point-toggle ov t))))

(defun preview-point-install (&optional remove)
  "Install or REMOVE advices and setup for preview-point."
  (cl-flet ((adv (if remove
                     (lambda (fn _ ad) (advice-remove fn ad))
                   'advice-add)))
    (adv 'preview-toggle :around #'preview-point@around@preview-toggle)
    (adv 'preview-mode-setup :after #'preview-point-setup)))

(preview-point-install)

;;; Auto-preview
(defcustom preview-point-auto-delay 0.1
  "Delay in seconds for automatic preview timer."
  :type 'number)

(defun preview-point@around@write-region (orig-fun &rest args)
   "Advice around `write-region' to suppress messages.
ORIG-FUN is the original function.  ARGS are its arguments."
  (let ((noninteractive t)
        (inhibit-message t)
        message-log-max)
    (apply orig-fun args)))

(buframe--defun-debounced preview-point--preview-at-point
  (pt buffer &delay preview-point-auto-delay)
  "Debounced preview at point PT in BUFFER."
  (interactive (list (current-buffer)))
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (if-let ((cur-process
                (or (get-buffer-process (TeX-process-buffer-name
                                         (TeX-region-file)))
                    (get-buffer-process (TeX-process-buffer-name
                                         (TeX-master-file))))))
          (let ((preview-point-auto-delay
                 (if (> preview-point-auto-delay 0)
                     preview-point-auto-delay
                   0.5)))
            ;; Force de-bouncing
            (when (and preview-current-region
                       (not preview-abort-flag)
                       ;; (< beg (cdr preview-current-region))
                       )
              (progn
                (ignore-errors (TeX-kill-job))
                (setq preview-abort-flag t)))
            (with-local-quit (accept-process-output cur-process))
            (preview-point--preview-at-point pt buffer))
        (let ((TeX-suppress-compilation-message t)
              (save-silently t))
          (advice-add 'write-region :around
                      #'preview-point@around@write-region)
          (unwind-protect
              ;; If we are working in a file buffer that is not a tex file,
              ;; then we want preview-region to operate in "non-file" mode,
              ;; where it passes "<none>" to TeX-region-create.
              (progn ;; (message "Running previewing at %S for %S" (point)
                ;;          (buffer-substring-no-properties (preview-next-border t)
                ;;                                          (preview-next-border nil)))
                (save-excursion
                  (goto-char pt)
                  ;; (preview-at-point)
                  (preview-region (preview-next-border t)
                                  (preview-next-border nil))))
            (advice-remove 'write-region
                           #'preview-point@around@write-region)))))))

(defun preview-point-buf-change (&rest _)
  "Run preview at point when buffer changes."
  (when (or (cl-find-if
             (lambda (ov) (overlay-get ov 'preview-state))
             (overlays-at (point)))
            (funcall preview-point-auto-p))
    (preview-point--preview-at-point (point) (current-buffer))))

(cl-pushnew '(point-dvisvgm
              (open preview-gs-open preview-dvi-process-setup)
	      (place preview-point-place preview-dvi-place)
	      (close preview-point-close preview-dvi-close)
              (args ((image-type svg)
                     (process-name "Preview-DviSVGM")
                     (command preview-dvisvgm-command)
                     (ascent (lambda (&rest _) 'center)))))
	    preview-image-creators
	    :test #'equal)


(cl-pushnew '(point-dvipng
              (open preview-gs-open preview-dvi-process-setup)
	      (place preview-point-place preview-dvi-place)
	      (close preview-point-close preview-dvi-close)
              (args ((image-type png)
                     (process-name "Preview-DviPNG")
                     (command preview-dvipng-command)
                     (ascent (lambda (&rest _) 'center)))))
	    preview-image-creators
	    :test #'equal)

(provide 'preview-point)
;;; preview-point.el ends here
