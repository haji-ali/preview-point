;;; preview-dvi.el --- Output processor for LaTeX preview  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Abdul-Lateef Haji-Ali <abdo.haji.ali@gmail.com>
;; Keywords: tex, tools
;; Version: 0.0.1
;; URL: https://github.com/hajiali/preview-point
;; Package-Requires: ((emacs "27.1") (auctex "13.0.12"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package provides a generic backend for preview-latex that handles
;; DVI -> image conversion, abstracting away duplicated logic between
;; `preview-dvipng-*' and `preview-dvisvgm-*'.
;;
;; It does three main things:
;;
;; 1. Provides a configurable framework where only the converter-specific
;;    parts (`command', `image-type', etc.) need to be specified.
;; 2. Adds hooks and extension points (marked with NOTE) for customization.
;; 3. Improves file management and process handling over the original code
;;    (again marked with NOTE).
;;
;; The variable `preview-image-creators' is extended with entries for both
;; dvipng and dvisvgm. Each entry carries an `args' alist containing:
;;
;;  - `image-type'   Output image type (`png' or `svg').
;;  - `process-name' Process buffer name.
;;  - `command'      Function returning the shell command and temp dir.
;;  - `ascent'       Function to compute ascent from bounding box.
;;
;; This makes it possible to swap the converter while reusing the same
;; infrastructure.
;;
;;; Code:

(require 'preview)
(require 'cl-lib)
(require 'face-remap)

(defvar preview-dvi-after-place-hook nil
  "Hook run after preview overlays are placed.
Each function is called with one argument: the list of overlays
processed.")

(defvar preview-dvi-config-default
  '((image-type png)
    (process-name "Preview-DviPNG")
    (command preview-dvipng-command)
    (ascent preview-ascent-from-bb))
  "Default configuration for `preview-dvi'.
Values are overridden by entries in `preview-image-creators'.")

;;; DVI generic functions
(defun preview-dvi-process-setup ()
  "Set up dvi* process for conversion."
  (setq preview-gs-command-line (append
                                 preview-gs-command-line
                                 (list (preview-gs-resolution
                                        (preview-hook-enquiry preview-scale)
                                        (car preview-resolution)
                                        (cdr preview-resolution)))))
  (if preview-parsed-pdfoutput
      (if (preview-supports-image-type preview-gs-image-type)
          (preview-pdf2dsc-process-setup)
        (error "preview-image-type setting '%s unsupported by this Emacs"
               preview-gs-image-type))
    (let ((type (preview-dvi-config 'image-type)))
      (unless (preview-supports-image-type type)
        (error "Image type \"%s\" unsupported by this Emacs" type))
      ;; NOTE: `TeX-sentinel-function' was being set to something temporary
      ;; rather than being set correctly before `preview-dvi-start' is
      ;; called. Here I set it before calling the function.
      (setq TeX-sentinel-function 'preview-dvi-sentinel)
      (list (preview-dvi-start)
            (current-buffer) TeX-active-tempdir t
            type))))

(defun preview-dvi-start ()
  "Start a dvi conversion process process.
See the original `preview-start-dvipng'."
  (let* ((name (preview-dvi-config 'process-name))
         (dir-command (funcall (preview-dvi-config 'command)))
         (command (car dir-command))
         (tempdir (cdr dir-command)))
    (setq TeX-active-tempdir tempdir)
    (goto-char (point-max))
    (insert-before-markers "Running `" name "' with ``" command "''\n")
    (setq mode-name name)
    (if TeX-process-asynchronous
        (let ((process (start-process name (current-buffer) TeX-shell
                                      TeX-shell-command-option
                                      command)))
          (if TeX-after-start-process-function
              (funcall TeX-after-start-process-function process))
          (TeX-command-mode-line process)
          (set-process-filter process #'TeX-command-filter)
          (set-process-sentinel process #'TeX-command-sentinel)
          (set-marker (process-mark process) (point-max))
          (push process compilation-in-progress)
          (sit-for 0)
          process)
      (setq mode-line-process ": run")
      (force-mode-line-update)
      (call-process TeX-shell nil (current-buffer) nil
                    TeX-shell-command-option
                    command))))

(defun preview-dvi-place-all ()
  "Place all images dvipng has created, if any.
Deletes the dvi file when finished.

See the original `preview-dvipng-place-all'."
  (let ((type (preview-dvi-config 'image-type))
        filename queued oldfiles snippet processed)
    (dolist (ov (prog1 preview-gs-queue (setq preview-gs-queue nil)))
      (when (and (setq queued (overlay-get ov 'queued))
                 (setq snippet (aref (overlay-get ov 'queued) 2))
                 (setq filename (preview-make-filename
                                 (format "prev%03d.%s" snippet type)
                                 TeX-active-tempdir)))
        (if (file-exists-p (car filename))
            (progn
              ;; NOTE: Delete previous filenames here before overwriting the
              ;; property `'filenames', potentially orphaning files.
              (preview-dvi-delete-overlay-files ov)
              (overlay-put ov 'filenames (list filename))
              (preview-replace-active-icon
               ov
               (preview-create-icon (car filename)
                                    type
                                    (funcall
                                     ;; NOTE: Allow customization of the
                                     ;; ascent function.
                                     (preview-dvi-config 'ascent)
                                     (aref queued 0))
                                    (aref preview-colors 2)))
              (overlay-put ov 'queued nil)
              (push ov processed))
          (push filename oldfiles)
          ;; NOTE: Do note overwrite `filenames' if we are not replacing it.
          ;; to avoid orphaning files.
          ;; (overlay-put ov 'filenames nil)
          (push ov preview-gs-queue))))
    (if (setq preview-gs-queue (nreverse preview-gs-queue))
        (progn
          (setq TeX-sentinel-function (lambda (process command)
                                        (preview-dvi-sentinel
                                         process
                                         command
                                         t)))
          (preview-dvi-start)
          (dolist (ov preview-gs-queue)
            (setq snippet (aref (overlay-get ov 'queued) 2))
            (overlay-put ov 'filenames
                         (list
                          (preview-make-filename
                           (or preview-ps-file
                               (format "preview.%03d" snippet))
                           TeX-active-tempdir))))
          (while (setq filename (pop oldfiles))
            (condition-case nil
                (preview-delete-file filename)
              (file-error nil))))
      (condition-case nil
          (let ((gsfile preview-gs-file))
            (delete-file
             (with-current-buffer TeX-command-buffer
               (funcall (car gsfile) "dvi" t))))
        (file-error nil)))
    ;; NOTE: Call a custom hook to process overlays after they have been
    ;; replaced with an active icon.
    (when processed
      (run-hook-with-args 'preview-dvi-after-place-hook processed))))

(defun preview-dvi-abort ()
  "Abort a dvi conversion run.
See the original `preview-dvipng-abort'"
  (prog1 (preview-dvips-abort)
    ;; NOTE: When the command is aborted, there is a chance that this happens
    ;; before the previews are generated but after a temp directory is create,
    ;; in this case an empty folder is left behind. This makes sure that this
    ;; folder is deleted.
    (when TeX-active-tempdir
      (unless (> (nth 2 TeX-active-tempdir) 1)
        (delete-directory (nth 0 TeX-active-tempdir))))))

(defun preview-dvi-sentinel (process command &optional placeall)
  "Sentinel function for indirect rendering DviPNG process.
The usual PROCESS and COMMAND arguments for
`TeX-sentinel-function' apply.  Places all snippets if PLACEALL
is set.
See the original `preview-dvipng-sentinel'."
  (condition-case err
      (let ((status (process-status process)))
        (cond ((or (eq status 'signal)
                   (save-excursion
                     (goto-char (point-max))
                     (search-backward (format "%s exited abnormally"
                                              command)
                                      nil t)))
               (delete-process process)
               (preview-dvi-abort))
              ((eq status 'exit)
               (delete-process process)
               (setq TeX-sentinel-function nil)
               (when placeall (preview-dvi-place-all)))))
    (error (preview-log-error err "DVI sentinel" process)))
  (preview-reraise-error process))

(defun preview-dvi-close (process closedata)
  "Clean up after PROCESS and set up queue accumulated in CLOSEDATA.
See the original `preview-dvipng-abort'."
  (setq preview-gs-queue (nconc preview-gs-queue closedata))
  (if process
      (if preview-gs-queue
          (if TeX-process-asynchronous
              (if (and (eq (process-status process) 'exit)
                       (null TeX-sentinel-function))
                  ;; Process has already finished and run sentinel
                  (preview-dvi-place-all)
                (setq TeX-sentinel-function (lambda (process command)
                                              (preview-dvi-sentinel
                                               process
                                               command
                                               t))))
            (TeX-synchronous-sentinel
             (preview-dvi-config 'process-name)
             (cdr preview-gs-file) process))
        ;; pathological case: no previews although we sure thought so.
        (delete-process process)
        (unless (eq (process-status process) 'signal)
          (preview-dvi-abort)))))

(defun preview-dvi-config (key)
  "Return the value associated with KEY in the current preview config.
Values are taken from `preview-image-creators' or
`preview-dvi-config-default'."
  (or (car (alist-get key
                      (cadr (assq 'args
                                  (cdr (assq preview-image-type
                                             preview-image-creators))))))
      (car (alist-get key preview-dvi-config-default))))

(defun preview-dvi-delete-overlay-files (ovr)
  "Delete all files associated with overlay OVR.
Also clears its `filenames' property."
  (dolist (filename (overlay-get ovr 'filenames))
    (condition-case nil
        (preview-delete-file filename)
      (file-error nil))
    (overlay-put ovr 'filenames nil)))

(defun preview-dvi-place (ov &rest args)
  "Place preview images for overlay OV with ARGS.
Delete old files first, then call `preview-gs-place'. Ensure
filenames are preserved across overlays to avoid accidental
deletions. Return list of overlays placed."
  ;; NOTE `preview-gs-place' immediately overwrites `filenames' without
  ;; checking it first, so we remove files in listed in `filenames' here.
  (preview-dvi-delete-overlay-files ov)

  (let ((ovl (apply 'preview-gs-place ov args)))
    (dolist (ov ovl)
      ;; NOTE: Inside `preview-place-preview', after `place' is called,
      ;; `preview-clearout' will be called to delete overlays in within the
      ;; same overlay and which have a timestamp that is different from the
      ;; one in the overlay. We need to make sure that these overlays do not
      ;; have the filename of our preview image in their 'filename property to
      ;; avoid eager file deletion
      (when preview-leave-open-previews-visible
        (when-let* ((filename (cadr (overlay-get ov 'preview-image))))
        (let ((start (or (overlay-start ov) (point-min)))
              (end (or (overlay-end ov) (point-max)))
              (exception ov)
                (timestamp (overlay-get ov 'timestamp)))
          (dolist (oov (overlays-in start end)) ;; Old overlays
            (when (and (not (eq oov exception))
                       (overlay-get oov 'preview-state)
                       (not (and timestamp
                                   (equal timestamp
                                          (overlay-get oov 'timestamp)))))
              ;; The overlay is gonna be deleted with its files.
              ;; Make sure its `filenames' does not contain our image
              (let ((files-oov (overlay-get oov 'filenames))
                    (files-ov  (overlay-get ov  'filenames)))
                  (when-let* ((entry (assoc filename files-oov)))
                  (overlay-put oov 'filenames
                               (assq-delete-all filename files-oov))
                  ;; Add the filename to the current overlay instead
                  ;; if it's not already there
                  (unless (assoc filename files-ov)
                      (overlay-put ov 'filenames
                                   (cons entry files-ov)))))))))))
    ovl))

;;; DVIPNG
(defun preview-dvipng-command ()
  "Return a shell command for starting a DviPNG process.
The result is a cons cell (COMMAND . TEMPDIR)."
  (let* (;; (file preview-gs-file)
         (res (/ (* (car preview-resolution)
                    (preview-hook-enquiry preview-scale))
                 (preview-get-magnification)))
         (resolution  (format " -D%d " res))
         (colors (preview-dvipng-color-string preview-colors res)))
    (with-current-buffer TeX-command-buffer
      (let ((cmd (concat (TeX-command-expand preview-dvipng-command)
                         " " colors resolution)))
        (cons cmd TeX-active-tempdir)))))

;;; DVISVGM
(defcustom preview-dvisvgm-command
  "dvisvgm --no-fonts %d --page=- --output=\"%m/prev%%3p.svg\""
  "Command used for converting to separate svg images.

Note, most SVG renderes do not support <font>...</font>-elements
generated by dvisvgm.

You might specify options for converting to other image types,
but then you'll need to adapt `preview-dvi-config'."
  :group 'preview-latex
  :type 'string)

(defun preview-dvisvgm-command ()
  "Return a shell command for running dvisvgm.
Result is a cons cell (COMMAND . TEMPDIR).  Includes scaling based on
preview magnification and text-scale settings."
  (let* (;; (file preview-gs-file)
         (scale (* (/ (preview-hook-enquiry preview-scale)
                      (preview-get-magnification))
		   (with-current-buffer TeX-command-buffer
		     (if text-scale-mode
		         (expt text-scale-mode-step text-scale-mode-amount)
		       1.0)))))
    (with-current-buffer TeX-command-buffer
      (let ((cmd (concat (TeX-command-expand preview-dvisvgm-command)
                         (format " --scale=%g " scale))))
        (cons cmd TeX-active-tempdir)))))

;;; Updates to variables.
(defun preview-dvi-variable-standard-value (symbol)
  "Return the standard value of variable SYMBOL.
This looks up SYMBOL's `standard-value' property and evaluates it."
  (let ((container (get symbol 'standard-value)))
    (cl-assert (consp container) "%s does not have a standard value")
    (eval (car container))))

(defun preview-dvi-set-variable-standard-value (symbol value)
  "Set standard value of SYMBOL to VALUE.
If SYMBOL currently holds its standard value, also set it to VALUE.
Update SYMBOL's `standard-value' property accordingly."
  (let ((standard (preview-dvi-variable-standard-value symbol)))
    (when (equal (symbol-value symbol) standard)
      (set symbol value))
    (put symbol 'standard-value (list
                                 (list
                                  'quote
                                  value)))))

(gv-define-simple-setter preview-dvi-variable-standard-value
                         preview-dvi-set-variable-standard-value)

(setf (preview-dvi-variable-standard-value 'preview-image-creators)
      (assq-delete-all
       'dvipng
       (preview-dvi-variable-standard-value 'preview-image-creators)))

(cl-loop for (type . args) in '((dvipng
                                 (image-type png)
                                 (process-name "Preview-DviPNG")
                                 (command preview-dvipng-command))
                                (dvisvgm
                                 (image-type svg)
                                 (process-name "Preview-DviSVGM")
                                 (command preview-dvisvgm-command)))
         do
         (cl-pushnew `(,type (open preview-gs-open preview-dvi-process-setup)
		             (place preview-dvi-place)
		             (close preview-dvi-close)
                             (args ,args))
	             (preview-dvi-variable-standard-value
                      'preview-image-creators)
	             :test #'equal))

;; Update preview-image-type customization list
(put 'preview-image-type 'custom-type
     (append '(choice)
             (mapcar (lambda (symbol) (list 'const (car symbol)))
                     preview-image-creators)
             '((symbol :tag "Other"))))

(cl-pushnew '(dvisvgm png "-sDEVICE=png16m")
	    (preview-dvi-variable-standard-value 'preview-gs-image-type-alist)
	    :test #'equal)

(provide 'preview-dvi)
;;; preview-dvi.el ends here
