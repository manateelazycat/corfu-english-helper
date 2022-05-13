;;; capf-english-helper.el --- English helper with capf interface

;;; Commentary:
;;
;; English helper with corfu interface.
;;

;;; Require
(require 'cl)
(require 'cl-lib)
(require 'corfu-english-helper-data)

;;; Code:

(defvar corfu-english-helper-active-p nil
  "The status of corfu-english-helper plugins.
Default is disable.")

(defun corfu-english-helper-annotation (s)
  (let* ((translation (get-text-property 0 :initials s)))
    (format "  %s" translation)))

(defun corfu-english-helper-search (&optional interactive)
  (interactive (list t))
  (if interactive
      (let ((completion-at-point-functions (list 'corfu-english-helper-search)))
        (completion-at-point))
    (let* ((bds (bounds-of-thing-at-point 'symbol))
           (start (car bds))
           (end (cdr bds))
           (prefix (if (and start end)
                       (buffer-substring-no-properties start end)
                     "")))
      (list start end (corfu-english-helper-get-items prefix)
            :annotation-function 'corfu-english-helper-annotation))))

(defun corfu-english-helper-get-items (prefix)
  (let* ((prefix-match-candidates
          (cl-remove-if-not
           (lambda (c)  (string-prefix-p (downcase prefix) c))
           corfu-english-helper-completions)))
    (corfu-english-helper-convert-candidates prefix prefix-match-candidates)))

(defun corfu-english-helper-convert-candidates (input candidates)
  (cond ((corfu-english-helper-upcase-string-p input)
         (mapcar 'upcase candidates))
        ((corfu-english-helper-capitalize-string-p input)
         (mapcar 'capitalize candidates))
        (t candidates)))

(defun corfu-english-helper-upcase-string-p (str)
  (let ((case-fold-search nil))
    (and (> (length str) 1)
         (string-match-p "\\`[A-Z]*\\'" str))))

(defun corfu-english-helper-capitalize-string-p (str)
  (let ((case-fold-search nil))
    (string-match-p "\\`[A-Z][a-z]*\\'" str)))

(defun toggle-corfu-english-helper ()
  "Toggle company english helper."
  (interactive)
  (if corfu-english-helper-active-p
      (progn
        ;; Restore options.
        (setq-local corfu-auto corfu-english-helper--corfu-auto)
        (setq-local corfu-auto-prefix corfu-english-helper--corfu-auto-prefix)
        
        (setq completion-at-point-functions (remove 'corfu-english-helper-search completion-at-point-functions))
        (setq corfu-english-helper-active-p nil)
        (message "Corfu english helper has disable."))
    
    ;; Save options.
    (setq-local corfu-english-helper--corfu-auto corfu-auto)
    (setq-local corfu-english-helper--corfu-auto-prefix corfu-auto-prefix)
    
    ;; Turn on `corfu-auto' and adjust `corfu-auto-prefix' to 0.
    (setq-local corfu-auto t)
    (setq-local corfu-auto-prefix 0)
    
    ;; We need call `(setq-local corfu-auto t)' before corfu-mode turn on.
    (corfu-mode 1)
    
    (add-to-list 'completion-at-point-functions #'corfu-english-helper-search)
    (setq corfu-english-helper-active-p t)
    (message "Corfu english helper has enable.")))

(provide 'corfu-english-helper)

;;; corfu-english-helper.el ends here
