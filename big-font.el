;;; big-font.el --- Minor emacs mode for changing the font face temporarily -*- lexical-binding: t; -*-

;; Author: 8dcc <8dcc.git@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/8dcc/big-font.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package was inspired by doom Emacs' big-font-mode.  For an example of
;; how set-up this package, see my personal dotfiles:
;;   https://github.com/8dcc/emacs-dotfiles

;;; Code:

(defgroup big-font ()
  "Minor mode for using bigger fonts globally."
  :group 'faces
  :prefix "big-font-")

(defcustom big-font-faces '((default 120 nil))
  "List of elements (FACE HEIGHT FAMILY) used by `big-font-mode'.

Each entry will be used for overwriting the :height and :family properties of
the specified FACE using `set-face-attribute'.  The FACE should be a symbol, the
HEIGHT an integer, and the FAMILY a string.

Note that the FACE is mandatory, but the HEIGHT and FAMILY are both optional and
can be nil."
  :group 'big-font
  :type '(repeat
          (list (face :tag "Face")
                (choice :tag "Height"
                        (const :tag "Inherit" nil)
                        integer)
                (choice :tag "Family"
                        (const :tag "Inherit" nil)
                        string)))
  :risky t)

(defvar big-font--old-faces nil
  "The last user values before enabling `big-font-mode'.

This list has the same format as `big-font-faces'.")

(defun big-font--enable ()
  "Enable `big-font-mode'."
  (setq big-font--old-faces nil)
  (dolist (element big-font-faces)
    (let ((face   (car   element))
          (height (cadr  element))
          (family (caddr element)))
      (if (null face)
          (error "Invalid face in `big-font-faces'"))
      (add-to-list 'big-font--old-faces
                   (list face
                         (face-attribute face :height)
                         (face-attribute face :family))
                   'append)
      (if height
          (set-face-attribute face nil :height height))
      (if family
          (set-face-attribute face nil :family family)))))

(defun big-font--disable ()
  "Disable `big-font-mode'."
  (dolist (element big-font--old-faces)
    (let ((face   (car   element))
          (height (cadr  element))
          (family (caddr element)))
      (if (null face)
          (error "Invalid face in `big-font--old-faces'"))
      (if height
          (set-face-attribute face nil :height height))
      (if family
          (set-face-attribute face nil :family family))))
  (setq big-font--old-faces nil))

;;;###autoload
(define-minor-mode big-font-mode
  "Minor mode for using bigger fonts globally.

Overwrites the height and family on an arbitrary number of faces, globally.  See
the `big-font-faces' variable for more information.

Useful for business presentations."
  :init-value nil
  :global t
  (if big-font-mode
      (big-font--enable)
    (big-font--disable)))

(provide 'big-font)
;;; big-font.el ends here
