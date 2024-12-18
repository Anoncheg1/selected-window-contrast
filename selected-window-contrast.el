;;; selected-window-contrast.el --- Set contrast of text and background for window and modeline   -*- lexical-binding: t -*-

;; Copyright (c) 2024 github.com/Anoncheg1,codeberg.org/Anoncheg

;; Author: github.com/Anoncheg1,codeberg.org/Anoncheg
;; Keywords:  color, contrast, selected, windows, faces, buffer
;; URL: https://github.com/Anoncheg1/selected-window-contrast
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.3"))

;; This file is not part of GNU Emacs.

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

;; Loop windows at frame, measure and adjust contrast.  Allow to set
;;  color (face) of background and text by comparing their
;;  brightness.  This is useful for changing themes during the daytime
;;  (circadian package) and for highlighting selected window.  Also
;;  this works for modeline.

;; Usage:

;;  (require 'selected-window-contrast)
;;  ;; - increase contrast for selected window
;;  (setopt selected-window-contrast-selected-magnitude-text 0.8)  ; default = 1.0
;;  (setopt selected-window-contrast-selected-magnitude-background 0.9)  ; default = 1.0
;;  ;; - decrease conrtrast for other windows
;;  (setopt selected-window-contrast-not-sel-magnitude-text 1.1)  ; default = 1.0
;;  (setopt selected-window-contrast-not-sel-magnitude-background 1.1)  ; default = 1.0
;;  (add-hook 'buffer-list-update-hook #'selected-window-contrast-highlight-selected-window)
;;  ;; - for case of call: $ emacsclient -c ~/file
;;  (add-hook 'server-after-make-frame-hook
;;            (lambda ()
;;              (run-with-idle-timer 0.1 nil #'selected-window-contrast-highlight-selected-window)))

;; To increase contrast of selected modeline:

;;  (selected-window-contrast-change-modeline 0.7 0.7) ; increase contrast

;; How this works:
;;  1) We get color with ```face-attribute (selected-frame)``` for foreground and backgraound.
;;  2) Convert color to HSL
;;  3) adjust brightness in direction of foreground-background average
;;  4) convert color to RGB, then to HEX
;;  5) apply color

;; Customize: M-x customize-group RET selected-window-contrast

;;; Code:

;; - configurable:
(defcustom selected-window-contrast-selected-magnitude-text 1
  "Change in contrast of text for selected window.
Higher value decrease contrast between text and background.
This value change contrast of text regarding to background."
  :group 'selected-window-contrast
  :type 'number)
(defcustom selected-window-contrast-selected-magnitude-background 1
  "Selected window background."
  :group 'selected-window-contrast
  :type 'number)
(defcustom selected-window-contrast-not-sel-magnitude-text 1.2
  "Not Selected window text."
  :group 'selected-window-contrast
  :type 'number)
(defcustom selected-window-contrast-not-sel-magnitude-background 1.2
  "Not selected window background."
  :group 'selected-window-contrast
  :type 'number)


(defun selected-window-contrast--get-current-colors ()
  "Get the current text and background colors."
  (let ((text-color (face-attribute 'default :foreground (selected-frame)))
        (background-color (face-attribute 'default :background (selected-frame))))
    (list text-color background-color)))

(defun selected-window-contrast--hex-to-rgb (hex-color &optional digits-per-component)
  "Return RGB values for the color specified by HEX-COLOR.
HEX-COLOR should be a string in the format #RRGGBB or #RRRRGGGGBBBB.
Optional argument DIGITS-PER-COMPONENT can be either 4 or 2 (the default);
use the latter if you need a 24-bit specification of a color."

  (let* ((hex-color (substring hex-color 1))
         (digits-per-component (or digits-per-component (if (= (length hex-color) 6) 2 4))) ; (if (= (length hex-color) 6) 2 4)
         (maxval (if (= digits-per-component 2) 255 65535)))
    (if (= digits-per-component 2)
        (list (/ (float (string-to-number (substring hex-color 0 2) 16)) maxval)
              (/ (float (string-to-number (substring hex-color 2 4) 16)) maxval)
              (/ (float (string-to-number (substring hex-color 4 6) 16)) maxval))
      (list (/ (float (string-to-number (substring hex-color 0 4) 16)) maxval)
            (/ (float (string-to-number (substring hex-color 4 8) 16)) maxval)
            (/ (float (string-to-number (substring hex-color 8 12) 16)) maxval)))))

(defun selected-window-contrast--hex-to-hsl (hex-color)
  "Convert hex or name of color to RGB.  Generated with AI.
Argument HEX-COLOR color."
  (let ((rgb (if (string-match "^#[0-9a-fA-F]+$" hex-color)
                 (selected-window-contrast--hex-to-rgb hex-color)
                 ;; else
                 (color-name-to-rgb hex-color))))
    (color-rgb-to-hsl (/ (nth 0 rgb) 65535.0)
                      (/ (nth 1 rgb) 65535.0)
                      (/ (nth 2 rgb) 65535.0))))

(defun selected-window-contrast--adjust-brightness (text-color background-color magnitude-text magnitude-back)
  "Adjust the brightness of the text and background colors.
To be closer By the magnitude.  Return (foreground , background).
Argument TEXT-COLOR hex or name for color of text.  Argument
BACKGROUND-COLOR tex or name for color of background.  Argument
MAGNITUDE-TEXT value to increase or decrease contrast for text.
Argument MAGNITUDE-BACK value to increase or decrease contrast
for background."
  (let ((text-hsl (selected-window-contrast--hex-to-hsl text-color))
        (background-hsl (selected-window-contrast--hex-to-hsl background-color)))
    (let ((text-brightness (nth 2 text-hsl))
          (background-brightness (nth 2 background-hsl)))
      ;; (print (list "debug" text-color background-color magnitude-text magnitude-back))
      ;; (print (list "debug2" text-hsl text-brightness))
      ;; new-text-brightness = average + (average - background) / magnitude-text
      ;; new-back-brightness = average + (average - text) / magnitude-back
      (let ((average-brightness (/ (+ text-brightness background-brightness) 2)))
        (let ((new-text-brightness (+ average-brightness (/ (- average-brightness background-brightness)
                                                            magnitude-text))) ;; (max 0 (min 1
              (new-background-brightness (- average-brightness (/ (- text-brightness average-brightness)
                                                                  magnitude-back)))) ;;  (max 0 (min 1
          ;; (print (list text-hsl new-text-brightness background-hsl new-background-brightness))
          (list (color-hsl-to-rgb (nth 0 text-hsl)
                                  (nth 1 text-hsl)
                                  new-text-brightness)
                (color-hsl-to-rgb (nth 0 background-hsl)
                                  (nth 1 background-hsl)
                                  new-background-brightness)))))))

(defun selected-window-contrast--rgb-to-hex (red green blue)
  "Convert RGB to hex.
Argument RED color.
Argument GREEN color.
Argument BLUE color."
  (color-rgb-to-hex (max 0 (min 1 (* red 65535)))
                    (max 0 (min 1 (* green 65535)))
                    (max 0 (min 1 (* blue 65535)))))

(defun selected-window-contrast--apply-new-colors (text-color background-color)
  "Apply the new text and background colors.
Argument TEXT-COLOR rgb color.
Argument BACKGROUND-COLOR rgb color."
  ;; (print (list "sss" text-color background-color))
  (let ((fg (selected-window-contrast--rgb-to-hex (nth 0 text-color) (nth 1 text-color) (nth 2 text-color)))
        (bg (selected-window-contrast--rgb-to-hex (nth 0 background-color) (nth 1 background-color) (nth 2 background-color))))
    ;; (print (list bg fg))
    (buffer-face-set (list :background bg :foreground fg))))

(defun selected-window-contrast-change-window (magnitude-text magnitude-back)
  "Adjust the text and background colors to be closer in brightness.
Argument MAGNITUDE-TEXT float value to increase or decrease contrast.
Argument MAGNITUDE-BACK float value to increase or decrease contrast."
  (let* ((current-colors (selected-window-contrast--get-current-colors))
         (new-colors (selected-window-contrast--adjust-brightness (nth 0 current-colors)
                                                                (nth 1 current-colors)
                                                                magnitude-text
                                                                magnitude-back)))
    (selected-window-contrast--apply-new-colors (nth 0 new-colors)
                                              (nth 1 new-colors))))

(defun selected-window-contrast-change-modeline (magnitude-text magnitude-back)
  "Adjust modeline brightness of text and background.
Argument MAGNITUDE-TEXT float value to increase or decrease contrast.
Argument MAGNITUDE-BACK float value to increase or decrease contrast."
  (let* ((back (face-attribute 'mode-line-active :background))
         (fore (face-attribute 'mode-line-active :foreground)))
    (when (eq back 'unspecified)
      (setq back (face-attribute 'default :background)))
    (when (eq fore 'unspecified)
      (setq fore (face-attribute 'default :foreground)))

         (if (or (eq back 'unspecified) (eq fore 'unspecified))
             (message "backgound or foreground color is unspecified in active mode line.")
           ;; else
           (let* ((new-colors (selected-window-contrast--adjust-brightness fore
                                                                         back
                                                                         magnitude-text
                                                                         magnitude-back))
                  (new-fore (apply 'selected-window-contrast--rgb-to-hex (nth 0 new-colors)))
                  (new-back (apply 'selected-window-contrast--rgb-to-hex (nth 1 new-colors))))
             (set-face-attribute 'mode-line-active nil
                          :foreground new-fore
                          :background new-back)))))


(defun selected-window-contrast-highlight-selected-window ()
  "Highlight not selected windows with a different background color.
For for case when hook triggered from (reverse themes) before the
new theme is fully loaded, that cause breaking contrast."
   (run-with-idle-timer 0.01 nil 'selected-window-contrast-highlight-selected-window2))

(defun selected-window-contrast-highlight-selected-window2 ()
  "Highlight not selected windows with a different background color."
  (let ((cbn (buffer-name (current-buffer)))
        (sw (selected-window)))
    (when (/= (aref cbn 0) ?\s) ; ignore system buffers
      ;; - not selected:
      (walk-windows (lambda (w)
                      (unless (or (and (= 1 selected-window-contrast-not-sel-magnitude-text)
                                        (= 1 selected-window-contrast-not-sel-magnitude-background))
                                  (eq sw w)
                                  (eq cbn (buffer-name (window-buffer w))))
                        (with-selected-window w
                          ;; (if (not (and (= 1 selected-window-contrast-not-sel-magnitude-text)
                          ;;               (= 1 selected-window-contrast-not-sel-magnitude-background)))
                              (selected-window-contrast-change-window
                               selected-window-contrast-not-sel-magnitude-text
                               selected-window-contrast-not-sel-magnitude-background)
                            ;; else
                            (buffer-face-set 'default)))))
      ;; )

      ;; - selected:
      (if (not (and (= 1 selected-window-contrast-selected-magnitude-text)
                    (= 1 selected-window-contrast-selected-magnitude-background)))
          (selected-window-contrast-change-window
           selected-window-contrast-selected-magnitude-text
           selected-window-contrast-selected-magnitude-background)
        ;; else
        (buffer-face-set 'default)))))

(provide 'selected-window-contrast)
;;; selected-window-contrast.el ends here
