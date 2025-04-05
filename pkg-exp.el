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

(defun pkg-exp--interleave-uppercase (str)
  "Remove non-alphabetic characters from STR and append uppercase version to the lowercase string."
  (let* ((filtered (seq-filter (lambda (c)
                                 (string-match-p "[a-zA-Z]" (char-to-string c)))
                               str))
         (lower-str (apply 'string filtered))
         (upper-str (upcase lower-str)))
    (concat lower-str upper-str)))

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
  :key "X"
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
    ("H" "Describe symbol" (lambda () (interactive) (describe-symbol pkg-exp--current-function)) :inapt-if pkg-exp--actions-inapt :transient t)
    ("A" "Apropos" apropos :inapt-if pkg-exp--actions-inapt)
    ("F" "Find definition" (lambda () (interactive) (find-function pkg-exp--current-function)) :inapt-if pkg-exp--actions-inapt)
    ("D" "Invoke debugger each time the function is visited" "debug-on-entry" :inapt-if pkg-exp--actions-inapt)])

(defconst pkg-exp--package-actions
  '[("P"
     (lambda () (concat "Describe package: " (propertize (format "%s" (car pkg-exp--current-package)) 'face 'transient-argument)))
     (lambda () (interactive) (describe-package (car pkg-exp--current-package))) :transient t)
    ("C" "Customize" (lambda () (interactive) (customize-group (car pkg-exp--current-package))))])

(defun pkg-exp--remove-package-name-prefix (fn)
  "Extract package name prefixes from FUNCTION-NAMES."
  (if (string-match "^\\([^:-]+\\)-" fn)
      (substring fn (1+ (length (match-string 1 fn))))
    fn))

(defun pkg-exp--make-keybinding-alist (bindings fn-name)
  (let ((result)
        (str (pkg-exp--interleave-uppercase fn-name)))
    (dotimes (i (length str))
      (unless result
        (let* ((char (aref str i)))
          (when (not (assq char bindings))
            (setq result (cons char fn-name))))))
    (unless result
      (message (format "could not find a suitable keybinding: %s" str))
      (setq result (cons ?Z fn-name)))
    result))

(defun pkg-exp--set-command (&optional fn)
  (setq pkg-exp--current-function fn)
  (transient-setup))

(defun pkg-exp--make-transient-menu (pkg)
  (interactive (list
                (completing-read "Package name: " package-alist)))
  (setq pkg-exp--current-package (assq (intern pkg) package-alist))
  (let* ((funcs (pkg-exp--get-package-commands pkg-exp--current-package))
         (bindings)
         (menu-name (symbol-name (car pkg-exp--current-package)))
         (menu (cl-map 'vector
                       (lambda (fn)
                         (let* ((fn-no-prefix (pkg-exp--remove-package-name-prefix (symbol-name fn)))
                                (binding (pkg-exp--make-keybinding-alist bindings fn-no-prefix)))
                           (push binding bindings)
                           (when pkg-exp--current-function
                             (debug fn pkg-exp--current-function))
                           (list (char-to-string (caar bindings))
                                 `(lambda () (pkg-exp--make-short-doc ',fn))
                                 `(lambda () (interactive) (pkg-exp--set-command ',fn))
                                 :transient t)))
                       funcs)))
    `(transient-define-prefix pkg-exp-transient ()
       "My transient Menu"
       ["Command Actions" ,pkg-exp--command-actions]
       [ (:info (lambda () (concat "Function: "
                                   (if pkg-exp--current-function
                                       (propertize (format "%s" pkg-exp--current-function) 'face 'transient-argument)
                                     (propertize "nothing selected" 'face 'transient-inactive-argument))
                                   ))) ]
       ["Package Actions" ,pkg-exp--package-actions]
       [,menu-name ,menu]
       )))

(defun pkg-exp (pkg-name)
  "Explore package via a Transient menu."
  (interactive (list
                (completing-read "Package name: " package-alist)))
  (setq pkg-exp--current-function nil)
  (eval (pkg-exp--make-transient-menu pkg-name))
  (pkg-exp-transient))

;; (call-interactively 'pkg-exp)
