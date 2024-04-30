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

;;;###autoload
(defvar big-font-face nil
  "The font to use for `big-font-mode'.

If nil, `default' will be used, and the height will be set to `big-font-height'.

See also `defface' and `set-frame-font'.")

;;;###autoload
(defvar big-font-height 200
  "The height for the `default' face if `big-font-mode' is enabled and
`big-font' is nil.")

;;;###autoload
(defun big-font--enable ()
  (if big-font-face
      (add-to-list 'face-remapping-alist '(default . big-font-face))
    (add-to-list 'face-remapping-alist `(default . (height ,big-font-height)))))

;;;###autoload
(defun big-font--disable ()
  (if big-font-face
      (setq face-remapping-alist
            (seq-remove (lambda (x)
                          (equal (cdr x) 'big-font-face))
                        face-remapping-alist))
    (setq face-remapping-alist
          (seq-remove (lambda (x)
                        (equal (caddr x) 'big-font-height))
                      face-remapping-alist))))

;;;###autoload
(define-minor-mode big-font-mode
  "Globally resizes your fonts for business presentations.

Uses `big-font' if its set, otherwise scales `default' to `big-font-height'."
  :init-value nil
  :lighter " Big"
  :global t
  (if big-font-mode
      (big-font--enable)
    (big-font--disable)))

(provide 'big-font)
;;; big-font.el ends here
