;;; selected-window-contrast-old.el --- Highlight by brightness of text and background   -*- lexical-binding: t -*-

;; Copyright (c) 2025 github.com/Anoncheg1,codeberg.org/Anoncheg
;; Author: <github.com/Anoncheg1,codeberg.org/Anoncheg>
;; Keywords:  color, windows, faces, buffer, background
;; URL: https://codeberg.org/Anoncheg/selected-window-contrast
;; Version: 0.1
;; Created: 11 dec 2024
;; Package-Requires: ((emacs "29.4"))
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; License
;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not,
;; see <https://www.gnu.org/licenses/agpl-3.0.en.html>.

;;; Commentary:
;; It Loop windows at frame, measure and adjust contrast.  Allow to
;;  set color (face) of background and text by comparing their
;;  brightness.
;; This is useful for changing themes during the daytime (circadian
;;  package) and for highlighting selected window.  Also this works
;;  for modeline.

;; Usage:

;;  (require 'swc)
;;  ;; - increase contrast for selected window (default = 1.0)
;;  (setopt swc-selected-magnitude-text 0.8)
;;  (setopt swc-selected-magnitude-background 0.9)
;;  ;; - decrease conrtrast for other windows (default = 1.0)
;;  (setopt swc-not-sel-magnitude-text 1.1)
;;  (setopt swc-not-sel-magnitude-background 1.1)
;;  (add-hook 'buffer-list-update-hook
;;           #'swc-highlight-selected-window)

;; To increase contrast of selected modeline:

;;  (swc-change-modeline 0.7 0.7)

;; How this works:
;;  1) We get color with `face-attribute' `selected-frame' for
;;  foreground and backgraound.
;;  2) Convert color to HSL
;;  3) adjust brightness in direction of foreground-background average
;;  4) convert color to RGB, then to HEX
;;  5) apply color

;; 1) ("swc--get-current-colors" ("WhiteSmoke" "black")
;; 2)
;; ("swc--hex-to-hsl rgb" (0.9607843137254902 0.9607843137254902 0.9607843137254902) "WhiteSmoke")
;; ("swc--hex-to-hsl rgb" (0.0 0.0 0.0) "black")
;; ("swc--adjust-brightness hsl" "WhiteSmoke": (0.0 0.0 1.46606288811397e-05) "black": (0.0 0.0 0.0))
;; ("swc--adjust-brightness av b" 7.33031444056985e-06)
;; 7.33031444056985e-06
;; ("swc--adjust-brightness newt newb" 1.0e+INF -6.597282996512864e-05)


;; ("swc--get-current-colors" ("#383a42" "#fafafa"))

;; ("swc--hex-to-hsl" (0.2196078431372549 0.22745098039215686 0.25882352941176473) "#383a42")

;; ("swc--hex-to-hsl" (0.9803921568627451 0.9803921568627451 0.9803921568627451) "#fafafa")

;; ("swc--adjust-brightness hsl" (0.6333333333333334 0.0819672131147542 3.650197394896007e-06) (0.0 0.0 1.495982538891806e-05))


;; Customize: M-x customize-group RET swc

;; Donate:
;; - BTC (Bitcoin) address: 1CcDWSQ2vgqv5LxZuWaHGW52B9fkT5io25
;; - USDT (Tether) address: TVoXfYMkVYLnQZV3mGZ6GvmumuBfGsZzsN
;; - TON (Telegram) address: UQC8rjJFCHQkfdp7KmCkTZCb5dGzLFYe2TzsiZpfsnyTFt9D

;; Other packages:
;; - Modern navigation in major modes https://github.com/Anoncheg1/firstly-search
;; - Search with Chinese	https://github.com/Anoncheg1/pinyin-isearch
;; - Ediff no 3-th window	https://github.com/Anoncheg1/ediffnw
;; - Dired history		https://github.com/Anoncheg1/dired-hist
;; - Copy link to clipboard	https://github.com/Anoncheg1/emacs-org-links
;; - Solution for "callback hell"	https://github.com/Anoncheg1/emacs-async1
;; - Restore buffer state	https://github.com/Anoncheg1/emacs-unmodified-buffer1
;; - outline.el usage		https://github.com/Anoncheg1/emacs-outline-it
;; - Call LLMs & AIfrom Org-mode block.  https://github.com/Anoncheg1/emacs-oai

;;; Code:
(require 'color)

;; - configurable:
(defcustom selected-window-contrast-old-selected-magnitude-text 1
  "Change in contrast of text for selected window.
Higher value decrease contrast between text and background.
This value change contrast of text regarding to background."
  :group 'swc
  :type 'number)
(defcustom selected-window-contrast-old-selected-magnitude-background 1
  "Selected window background."
  :group 'swc
  :type 'number)
(defcustom selected-window-contrast-old-not-sel-magnitude-text 1.2
  "Not Selected window text."
  :group 'swc
  :type 'number)
(defcustom selected-window-contrast-old-not-sel-magnitude-background 1.2
  "Not selected window background."
  :group 'swc
  :type 'number)


(defun selected-window-contrast-old--get-current-colors ()
  "Get current text and background color of default face.
Returns list: (text background), both strings."
  ;; (print (list "selected-window-contrast-old--get-current-colors" (list (face-attribute 'default :foreground (selected-frame))
  ;;                                              (face-attribute 'default :background (selected-frame)))))
  (list (face-attribute 'default :foreground (selected-frame))
        (face-attribute 'default :background (selected-frame))))

;; (defun selected-window-contrast-old--hex-to-rgb (hex-color &optional digits-per-component)
;;   "Return RGB values for the color specified by HEX-COLOR.
;; HEX-COLOR should be a string in the format #RRGGBB or #RRRRGGGGBBBB.
;; Optional argument DIGITS-PER-COMPONENT can be either 4 or 2 (the default);
;; use the latter if you need a 24-bit specification of a color."

;;   (let* ((hex-color (substring hex-color 1))
;;          (digits-per-component (or digits-per-component (if (= (length hex-color) 6) 2 4)))
;;          (maxval (if (= digits-per-component 2) 255 65535)))
;;     (if (= digits-per-component 2)
;;         (list (/ (float (string-to-number (substring hex-color 0 2) 16)) maxval)
;;               (/ (float (string-to-number (substring hex-color 2 4) 16)) maxval)
;;               (/ (float (string-to-number (substring hex-color 4 6) 16)) maxval))
;;       (list (/ (float (string-to-number (substring hex-color 0 4) 16)) maxval)
;;             (/ (float (string-to-number (substring hex-color 4 8) 16)) maxval)
;;             (/ (float (string-to-number (substring hex-color 8 12) 16)) maxval)))))


(defun selected-window-contrast-old--hex-to-rgb (hex-color &optional digits-per-component)
  "Return RGB values for the color specified by HEX-COLOR.
HEX-COLOR should be a string in the format #RRGGBB or #RRRRGGGGBBBB.
Optional argument DIGITS-PER-COMPONENT can be either 4 or 2 (the default)."
  (unless (and (stringp hex-color) (string-prefix-p "#" hex-color))
    (error "HEX-COLOR must be a string starting with '#'"))
  (let* ((hex-str (substring hex-color 1))
         (digits (or digits-per-component (if (= (length hex-str) 6) 2 4)))
         (expected-length (* 3 digits)))
    (unless (= (length hex-str) expected-length)
      (error "HEX-COLOR does not match expected length for %d digits/component" digits))
    (let ((maxval (if (= digits 2) 255 65535))
          (rgb-list nil))
      (dotimes (i 3)
        (push (/ (float (string-to-number (substring hex-str (* i digits) (* (+ i 1) digits)) 16))
                 maxval)
              rgb-list))
      (nreverse rgb-list))))

(defun selected-window-contrast-old--hex-to-hsl (hex-color)
  "Convert hex or name of color to HSL (all values in 0-1).
Argument HEX-COLOR ."
  (let ((rgb (if (and (stringp hex-color) (string-match "^#[0-9a-fA-F]+$" hex-color))
                 (selected-window-contrast-old--hex-to-rgb hex-color)
               ;; else
               (or (color-name-to-rgb hex-color)
                   (error "Color name '%s' not recognized" hex-color)))))
    ;; (print (list "selected-window-contrast-old--hex-to-hsl rgb" rgb hex-color))
    ;; (print rgb)
    (color-rgb-to-hsl (/ (nth 0 rgb) 65535.0)
                      (/ (nth 1 rgb) 65535.0)
                      (/ (nth 2 rgb) 65535.0))))

;; (apply 'color-rgb-to-hsl (color-name-to-rgb "WhiteSmoke"))
;; (nth 2  (selected-window-contrast-old--hex-to-hsl "WhiteSmoke"))
;; (apply #'color-rgb-to-hsl rgb)
;; (let ((rgb '(0.5 0 0)))
;;   (apply 'color-hsl-to-rgb (color-rgb-to-hsl (/ (nth 0 rgb) 65535.0)
;;                       (/ (nth 1 rgb) 65535.0)
;;                       (/ (nth 2 rgb) 65535.0))))
;;   (apply 'color-hsl-to-rgb (apply #'color-rgb-to-hsl '(1 0 0)))

(defun selected-window-contrast-old--adjust-brightness (text-color background-color magnitude-text magnitude-back)
  "Adjust the brightness of the text and background colors.
To be closer By the magnitude.  Return (foreground, background).
Argument TEXT-COLOR hex or name for color of text.  Argument
BACKGROUND-COLOR tex or name for color of background.  Argument
MAGNITUDE-TEXT value to increase or decrease contrast for text.
Argument MAGNITUDE-BACK value to increase or decrease contrast
for background."
  (let ((text-hsl (selected-window-contrast-old--hex-to-hsl text-color))
        (background-hsl (selected-window-contrast-old--hex-to-hsl background-color)))
    ;; (print (list "selected-window-contrast-old--adjust-brightness hsl" text-hsl background-hsl))
    (let ((text-brightness (nth 2 text-hsl))
          (background-brightness (nth 2 background-hsl)))
      ;; new-text-brightness = average + (average - background) / magnitude-text
      ;; new-back-brightness = average + (average - text) / magnitude-back
      (let ((average-brightness (/ (+ text-brightness background-brightness) 2)))
        ;; (print (list "selected-window-contrast-old--adjust-brightness av b" average-brightness))
        (let ((new-text-brightness (+ average-brightness (/ (- average-brightness background-brightness)
                                                            magnitude-text))) ;; (max 0 (min 1
              (new-background-brightness (- average-brightness (/ (- text-brightness average-brightness)
                                                                  magnitude-back)))) ;;  (max 0 (min 1
          ;; (print (list "selected-window-contrast-old--adjust-brightness newt newb" new-text-brightness new-background-brightness))
          (list (color-hsl-to-rgb (nth 0 text-hsl)
                                  (nth 1 text-hsl)
                                  new-text-brightness)
                (color-hsl-to-rgb (nth 0 background-hsl)
                                  (nth 1 background-hsl)
                                  new-background-brightness)))))))

(defun selected-window-contrast-old--rgb-to-hex (red green blue)
  "Convert RGB to hex.
0.7 0 0 -> #ffff00000000, color-rgb-to-hex give #b33200000000
Argument RED color.
Argument GREEN color.
Argument BLUE color."
  (color-rgb-to-hex (max 0 (min 1 (* red 65535)))
                    (max 0 (min 1 (* green 65535)))
                    (max 0 (min 1 (* blue 65535)))))

;; (selected-window-contrast-old--rgb-to-hex 0.7 0 0)
;; (color-rgb-to-hex 0.7 0 0)

(defun selected-window-contrast-old--apply-new-colors (text-color background-color)
  "Apply the new text and background colors.
Argument TEXT-COLOR rgb color.
Argument BACKGROUND-COLOR rgb color."
  ;; (print (list "selected-window-contrast-old--apply-new-colors text-color background-color" text-color background-color))
  (let ((fg (selected-window-contrast-old--rgb-to-hex (nth 0 text-color) (nth 1 text-color) (nth 2 text-color)))
        (bg (selected-window-contrast-old--rgb-to-hex (nth 0 background-color) (nth 1 background-color) (nth 2 background-color))))
    ;; (print (list "selected-window-contrast-old--apply-new-colors bg fg" bg fg))
    (buffer-face-set (list :background bg :foreground fg))))

(defun selected-window-contrast-old-change-window (magnitude-text magnitude-back)
  "Adjust the text and background colors to be closer in brightness.
Argument MAGNITUDE-TEXT float value to increase or decrease contrast.
Argument MAGNITUDE-BACK float value to increase or decrease contrast."
  (let* ((current-colors (selected-window-contrast-old--get-current-colors))
         (new-colors (selected-window-contrast-old--adjust-brightness (nth 0 current-colors)
                                             (nth 1 current-colors)
                                             magnitude-text
                                             magnitude-back)))
    ;; (print "selected-window-contrast-old-change-window" new-colors)
    (selected-window-contrast-old--apply-new-colors (nth 0 new-colors)
                                              (nth 1 new-colors))))

(defun selected-window-contrast-old-change-modeline (magnitude-text magnitude-back)
  "Adjust modeline brightness of text and background.
Argument MAGNITUDE-TEXT float value to increase or decrease contrast.
Argument MAGNITUDE-BACK float value to increase or decrease contrast."
  (let* ((back (face-attribute 'mode-line-active :background))
         (fore (face-attribute 'mode-line-active :foreground)))
    (when (or (eq back 'unspecified) (eq back 'unspecified-bg))
      (setq back (face-attribute 'default :background)))
    ;; (print (list fore back))
    (when (eq fore 'unspecified)
      (setq fore (face-attribute 'default :foreground)))

    (if (or (eq back 'unspecified)
            (eq back 'unspecified-bg)
            (eq fore 'unspecified))
        (message "backgound or foreground color is unspecified in active mode line.")
      ;; else
      (let* ((new-colors (selected-window-contrast-old--adjust-brightness fore
                                                                      back
                                                                      magnitude-text
                                                                      magnitude-back))
             (new-fore (apply #'selected-window-contrast-old--rgb-to-hex (nth 0 new-colors)))
             (new-back (apply #'selected-window-contrast-old--rgb-to-hex (nth 1 new-colors))))
        (set-face-attribute 'mode-line-active nil
                            :foreground new-fore
                            :background new-back)
        t))))

(defun selected-window-contrast-old-highlight-selected-window-timeout1 ()
  "Highlight not selected windows with a different background color.
Timeout 0.01 sec.
For for case when hook triggered from (reverse themes) before the
new theme is fully loaded, that cause breaking contrast."
  (selected-window-contrast-old-highlight-selected-window)
  ;; - fix for emacsclient -c --eval "(pop-to-buffer-same-window (dired-noselect ..."
  (run-with-idle-timer 1 nil #'selected-window-contrast-old-highlight-selected-window))

(defun selected-window-contrast-old-highlight-selected-window-timeout2 ()
  "Highlight not selected windows with a different background color.
Timeout 0.1 sec.
For case of opening new frame with new buffer by call:
$ emacsclient -c ~/file"
  (run-with-idle-timer 0.4 nil #'selected-window-contrast-old-highlight-selected-window))

(defun selected-window-contrast-old-highlight-selected-window ()
  "Highlight not selected windows with a different background color."
  (let ((cbn (buffer-name (current-buffer)))
        (sw (selected-window)))
    (when (/= (aref cbn 0) ?\s) ; ignore system buffers
      ;; - not selected:
      (walk-windows (lambda (w)
                      (unless (or (and (= 1 selected-window-contrast-old-not-sel-magnitude-text)
                                       (= 1 selected-window-contrast-old-not-sel-magnitude-background))
                                  (eq sw w)
                                  (eq cbn (buffer-name (window-buffer w))))
                        (with-selected-window w
                          (buffer-face-set 'default)
                          (selected-window-contrast-old-change-window
                           selected-window-contrast-old-not-sel-magnitude-text
                           selected-window-contrast-old-not-sel-magnitude-background))))
                    -1 ) ; -1 means to not include minimuber

      ;; - selected:
      (if (not (and (= 1 selected-window-contrast-old-selected-magnitude-text)
                    (= 1 selected-window-contrast-old-selected-magnitude-background)))
          (selected-window-contrast-old-change-window
           selected-window-contrast-old-selected-magnitude-text
           selected-window-contrast-old-selected-magnitude-background)
        ;; else
        (buffer-face-set 'default)))))

(provide 'selected-window-contrast-old)
;;; selected-window-contrast-old.el ends here
