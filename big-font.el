;;; big-font.el --- Minor emacs mode for changing the font face temporarily -*- lexical-binding: t; -*-

;; Author: 8dcc <8dcc.git@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/8dcc/big-font.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package was inspired by doom emacs' big-font-mode. For an example of how
;; set-up this package, see my personal dotfiles:
;;   https://github.com/8dcc/emacs-dotfiles

;;; Code:

(defgroup big-font ()
  "Toggle to a big font face."
  :group 'font)

(defcustom big-font-height 150
  "The font height for the default face when `big-font-mode' is enabled."
  :group 'big-font
  :type 'integer
  :risky t)

(defcustom big-font-family-alist nil
  "Alist of (FACE . FAMILY) that will be overwritten when `big-font-mode' is
enabled. The family should be a string."
  :group 'big-font
  :type '(alist :key-type face :value-type string)
  :risky t)

(defvar big-font--normal-height 80
  "The last user height of the default face before enabling `big-font-mode'")

(defvar big-font--normal-families nil
  "The last user height of the default face before enabling `big-font-mode'")

(defun big-font--enable ()
  (if big-font-height
      (progn
        (setq big-font--normal-height (face-attribute 'default :height))
        (set-face-attribute 'default nil :height big-font-height)))
  (if big-font-family-alist
      (progn
        (setq big-font--normal-families nil)
        (dolist (target big-font-family-alist)
          (add-to-list 'big-font--normal-families
                       (cons (car target)
                             (face-attribute (car target) :family))
                       'append))
        (dolist (target big-font-family-alist)
          (set-face-attribute (car target) nil :family (cdr target))))))

(defun big-font--disable ()
  (set-face-attribute 'default nil :height big-font--normal-height)
  (setq big-font--normal-height nil)
  (dolist (target big-font--normal-families)
    (set-face-attribute (car target) nil :family (cdr target)))
  (setq big-font--normal-families nil))

;;;###autoload
(define-minor-mode big-font-mode
  "Globally resizes your fonts for business presentations.

Uses `big-font' if its set, otherwise scales `default' to `big-font-height'."
  :init-value nil
  :global t
  (if big-font-mode
      (big-font--enable)
    (big-font--disable)))

(provide 'big-font)
;;; big-font.el ends here
