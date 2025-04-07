;;; pkg-exp.el --- Explore installed packages via a Transient menu -*- lexical-binding: t -*-

;; Copyright (C) 2025 theyamo <tta@kapsi.fi>

;; Author: theyamo <tta@kapsi.fi>
;; URL: https://github.com/theyamo/pkg-exp-el
;; Keywords: convenience
;; Package-Requires: ((emacs "29") (cl-lib "0.3"))

;; This file is NOT part of GNU Emacs.

;; pkg-exp is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; pkg-exp is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with weather-el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Explore installed packages via a Transient menu.

;;; Code:

(require 'transient)

(defvar pkg-exp--current-function nil)
(defvar pkg-exp--current-package nil)

(defun pkg-exp--get-package-commands (pkg)
  "Return a list of interactive functions grouped by package."
  (let ((result nil)
        (pkg-name (symbol-name (car pkg)))
        (functions nil))
    (mapatoms
     (lambda (sym)
       (when (and (commandp sym) ; Is the function interactive?
                  (let ((fn-name (symbol-name sym)))
                    ;; Check if function belongs to package's namespace
                    (string-prefix-p (concat pkg-name "-") fn-name)))
         (push sym functions))))
    functions))

(defun OLD-pkg-exp--make-short-doc (fn)
  (let ((doc (documentation fn))
        (name (symbol-name fn)))
    (if (and doc
             (not (string-prefix-p "\n" doc)))
        (car (split-string (or doc "") "\\." t))
      (propertize (concat "`" name "'") 'face 'font-lock-function-name-face))))

(defun pkg-exp--make-short-doc (fn)
  (let ((doc (documentation fn))
        (name (symbol-name fn))
        (selected (equal fn pkg-exp--current-function)))
    (if (and doc
             (not (string-prefix-p "\n" doc)))
        (if selected
            (propertize (car (split-string (or doc "") "\\." t)) 'face 'transient-argument)
          (propertize (car (split-string (or doc "") "\\." t))))
      (if selected
          (propertize (concat "`" name "'") 'face 'transient-argument)
        (propertize (concat "`" name "'") 'face 'font-lock-function-name-face)))))

(transient-define-suffix trape-execute ()
  "Execute command."
  :key "RET"
  :description "Execute command"
  (interactive)
  (let* ((current-command (transient-args transient-current-command))
         (do-debug (transient-arg-value "debug-on-entry" current-command)))
    (when do-debug
      (debug-on-entry pkg-exp--current-function))
    (call-interactively pkg-exp--current-function)))

(defun pkg-exp--actions-inapt ()
  (null pkg-exp--current-function))

(defconst pkg-exp--command-actions
  '[(trape-execute :inapt-if pkg-exp--actions-inapt)
    ("Xh" "Describe" (lambda () (interactive) (describe-command pkg-exp--current-function)) :inapt-if pkg-exp--actions-inapt :transient t)
    ("Xa" "Apropos" (lambda () (interactive) (apropos (symbol-name pkg-exp--current-function))) :inapt-if pkg-exp--actions-inapt)
    ("Xf" "Find definition" (lambda () (interactive) (find-function pkg-exp--current-function)) :inapt-if pkg-exp--actions-inapt)
    ("Xd" "Invoke debugger each time the function is visited" "debug-on-entry" :inapt-if pkg-exp--actions-inapt)])

(defconst pkg-exp--package-actions
  '[("Pd"
     (lambda () (concat "Describe package: " (propertize (format "%s" (car pkg-exp--current-package)) 'face 'transient-argument)))
     (lambda () (interactive) (describe-package (car pkg-exp--current-package))) :transient t)
    ("Pl" "Go to source" (lambda () (interactive) (find-library (symbol-name (car pkg-exp--current-package)))))
    ("Pi" "Display manual" (lambda () (interactive) (info-display-manual (symbol-name (car pkg-exp--current-package)))))
    ("Po" "Select another package" (lambda () (interactive) (call-interactively 'pkg-exp)))
    ("Pu" "Uninstall" (lambda () (interactive) (package-delete pkg-exp--current-package)))
    ("Pc" "Customize" (lambda () (interactive) (customize-group (car pkg-exp--current-package))))])

(defun pkg-exp--remove-package-name-prefix (fn)
  "Extract package name prefixes from FUNCTION-NAMES."
  (if (string-match "^\\([^:-]+\\)-" fn)
      (substring fn (1+ (length (match-string 1 fn))))
    fn))

(defun pkg-exp--generate-transient-hotkeys (strings reserved-chars)
  "Generate hotkeys for STRINGS avoiding RESERVED-CHARS.
If the number of STRINGS is less than or equal to 20, prefer single-character hotkeys."
  (let* ((available-chars (delete-if (lambda (ch) (member ch reserved-chars))
                                     (mapcar 'identity "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOQRSTUVWYZ")))
         (hotkeys))

    ;; Use single-character hotkeys if possible
    (if (<= (length strings) 50)
        (setq hotkeys (mapcar 'char-to-string (seq-take available-chars (length strings))))
      ;; Otherwise, generate two-character hotkeys
      (dolist (c1 available-chars)
        (dolist (c2 available-chars)
          (push (concat (char-to-string c1) (char-to-string c2)) hotkeys)
          (when (>= (length hotkeys) (length strings)) (return)))))
    (cl-mapcar (lambda (hk fn)
                 (cons hk fn))
               (cl-subseq (nreverse hotkeys) 0 (length strings)) strings)))

(defun pkg-exp--set-command (&optional fn)
  (setq pkg-exp--current-function fn)
  (transient-setup))

(defun pkg-exp--make-transient-menu (pkg)
  (interactive (list
                (completing-read "Package name: " package-alist)))
  (setq pkg-exp--current-package (assq (intern pkg) package-alist))
  (let* ((funcs (pkg-exp--get-package-commands pkg-exp--current-package))
         (items-per-column (/ (length funcs) 2))
         (need-two-keys (> (length funcs) 20))
         (bindings (pkg-exp--generate-transient-hotkeys
                    (mapcar (lambda (sym) (symbol-name sym)) funcs)
                    '(?X ?P)))
         (menu-name (symbol-name (car pkg-exp--current-package)))
         (menu (cl-map 'vector
                       (lambda (b)
                         (let* ((fn-no-prefix (pkg-exp--remove-package-name-prefix (cdr b)))
                                (binding (car b)))
                           (list binding
                                 `(lambda () (pkg-exp--make-short-doc ',(intern (cdr b))))
                                 `(lambda () (interactive) (pkg-exp--set-command ',(intern (cdr b))))
                                 :transient t)))
                       bindings)))
    `(transient-define-prefix pkg-exp-transient ()
       "My transient Menu"
       ["Command Actions" ,pkg-exp--command-actions]
       [(:info (lambda () (concat "Function: "
                                  (if pkg-exp--current-function
                                      (propertize (format "%s" pkg-exp--current-function) 'face 'transient-argument)
                                    (propertize "nothing selected" 'face 'transient-inactive-argument))
                                  )))]
       ["Package Actions" ,pkg-exp--package-actions]
       [,menu-name ,menu])))

(defun pkg-exp (pkg-name)
  "Explore package via a Transient menu."
  (interactive (list
                (completing-read "Package name: " package-alist)))
  (setq pkg-exp--current-function nil)
  (eval (pkg-exp--make-transient-menu pkg-name))
  (pkg-exp-transient))

;; (call-interactively 'pkg-exp)
