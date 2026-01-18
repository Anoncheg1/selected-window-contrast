;;; selected-window-contrast.el --- Highlight by brightness of text and background   -*- lexical-binding: t -*-

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

;; Known issue: if you use several themes with different contrast you
;; should set both variables `selected-window-contrast-text-other' and
;; `selected-window-contrast-bg-selected'


;; Usage:

;; (add-to-list 'load-path "path_to/selected-window-contrast") ; optional
;; (when (require 'selected-window-contrast nil 'noerror)
;;   (setopt selected-window-contrast-bg-selected 0.95)
;;   (setopt selected-window-contrast-bg-others 0.7)
;;   (setopt selected-window-contrast-contrast-text-selected 0.9)
;;   (setopt selected-window-contrast-contrast-text-others 0.6)
;;   (add-hook 'buffer-list-update-hook
;;             #'selected-window-contrast-highlight-selected-window))


;; How this works:
;;  1) We get color with `face-attribute' `selected-frame' for
;;  foreground and backgraound.
;;  2) Convert color to HSL
;;  3) adjust brightness in direction of foreground-background average
;;  4) convert color to RGB, then to HEX
;;  5) apply color

;; Customize: M-x customize-group RET selected-window-contrast

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
;; Touch: Global variables bound deep is not good, it is a type of the inversion of control.
;; I am the best that is why I am the winner.
(require 'color)

;; - configurable:
(defcustom selected-window-contrast-bg-selected nil
  "Non-nil used to set selected window background contrast in [0-1] range.
Higher value increase contrast between text and background.
This value change contrast of text regarding to background."
  :group 'swc2
  :type '(choice (number :tag "contrast in [0-1] range")
                 (const :tag "Don't change default contrast of theme." nil)))

(defcustom selected-window-contrast-bg-others 0.8
  "Non-nil used to set not selected windows background contrast.
in [0-1] range."
  :group 'swc2
  :type '(choice (number :tag "contrast [0-1].")
                 (const :tag "Don't change default contrast of theme." nil)))

(defcustom selected-window-contrast-contrast-text-selected nil
  "Non-nil used to set not selected windows text contrast in [0-1] range."
  :group 'swc2
  :type '(choice (number :tag "Text contrast [0-1].")
                 (const :tag "Don't change default contrast of theme." nil)))

(defcustom selected-window-contrast-contrast-text-others nil
  "Non-nil used to set not selected windows text contrast in [0-1] range."
  :group 'swc2
  :type '(choice (number :tag "Text contrast [0-1].")
                 (const :tag "Don't change default contrast of theme." nil)))

;; (defcustom selected-window-contrast-contrast-ratio 0.5
;;   "Ratio selected vs other window background contrast (in [0-1]).
;; 0.5 means don't do anything."
;;   :group 'swc2
;;   :type '(number :tag "contrast ratio (selected/other) in [0-1]."))


(defun selected-window-contrast--get-current-colors ()
  "Get current text and background color of default face.
Returns list: (foreground background), both strings."
  (list (face-attribute 'default :foreground (selected-frame))
        (face-attribute 'default :background (selected-frame))))


;; (defun selected-window-contrast-hex-to-rgb (hex-color) ; new
;;   "Convert hex string #RRGGBB or #RRRRGGGGBBBB to normalized (0-1) RGB list."
;;   (unless (and (stringp hex-color) (string-prefix-p "#" hex-color))
;;     (error "HEX-COLOR must start with '#'"))
;;   (let* ((hex-str (substring hex-color 1))
;;          (digits-per (cond ((= (length hex-str) 6) 2)
;;                            ((= (length hex-str) 12) 4)
;;                            (t (error "HEX-COLOR format not recognized")))
;;          )
;;          (maxval (if (= digits-per 2) 255.0 65535.0)))
;;     (cl-loop for i from 0 below 3
;;              collect (/ (string-to-number (substring hex-str (* i digits-per) (* (+ i 1) digits-per)) 16)
;;                         maxval))))

;; (defun selected-window-contrast-hex-or-name-to-hsl (color) ; new
;;   "Convert hex string or color name to normalized HSL list."
;;   (let ((rgb (cond ((and (stringp color) (string-prefix-p "#" color))
;;                     (selected-window-contrast-hex-to-rgb color))
;;                    ((color-name-to-rgb color))   ;; Already [0,1]
;;                    (t (error "Color not recognized: %s" color)))))
;;     (apply #'color-rgb-to-hsl rgb)))

(defun selected-window-contrast--parse-color (color)
  "Return normalized RGB list for COLOR (hex or name).
COLOR: string, hex like '#rrggbb' or '#rrrrggggbbbb', or color name."
  ;; (print (list "selected-window-contrast--parse-color" color))
  (cond
   ((and (stringp color) (string-prefix-p "#" color))
    (let* ((hex (substring color 1))
           (len (length hex))
           (digits (/ len 3))
           (maxval (if (= digits 2) 255.0 65535.0)))
      (mapcar
       (lambda (i)
         (/ (string-to-number (substring hex (* i digits) (* (+ i 1) digits)) 16)
            maxval))
       '(0 1 2))))
   ((and (stringp color) (color-name-to-rgb color))
    (color-name-to-rgb color))
   (t (error "Invalid color: %s" color))))


(defun selected-window-contrast--color-to-hsl (color)
  "Convert COLOR (hex or name) to a normalized HSL list.
COLOR: string, hex or color name."
  ;; (print (list "selected-window-contrast--color-to-hsl" color (selected-window-contrast--parse-color color)))
  (apply #'color-rgb-to-hsl (selected-window-contrast--parse-color color)))


;; (defun selected-window-contrast-adjust-brightness (text-color background-color magnitude)
;;   "Given text/background colors, remap both to maximize contrast around midpoint."
;;   (let* ((text-hsl (selected-window-contrast--color-to-hsl text-color))
;;          (bg-hsl (selected-window-contrast--color-to-hsl background-color))
;;          (t-l (nth 2 text-hsl))
;;          (b-l (nth 2 bg-hsl))
;;          (mid 0.5)
;;          (new-t-l (if (> t-l mid)
;;                       (+ mid (* magnitude (- 1.0 mid))) ; push toward white
;;                     (- mid (* magnitude (- mid 0)))) ; push toward black
;;                     )
;;          (new-b-l (if (> b-l mid)
;;                       (+ mid (* magnitude (- 1.0 mid)))
;;                     (- mid (* magnitude (- mid 0)))))
;;          (new-text-rgb (apply #'color-hsl-to-rgb (list (nth 0 text-hsl) (nth 1 text-hsl) new-t-l)))
;;          (new-bg-rgb   (apply #'color-hsl-to-rgb (list (nth 0 bg-hsl) (nth 1 bg-hsl) new-b-l))))
;;     (list new-text-rgb new-bg-rgb)))

;; (defun selected-window-contrast-adjust-contrast (text-color background-color magnitude &optional _)
;;   "Maximize visual contrast between TEXT-COLOR and BACKGROUND-COLOR.
;; This function remaps the lightness component of both input colors so that:
;; - Colors above the midpoint (0.5) are pushed towards full white.
;; - Colors below the midpoint are pushed towards full black.

;; MAGNITUDE (float in [0,1]) controls the strength contrast, 1 is full
;; contrast.

;; Arguments:
;;  TEXT-COLOR        String, name or hex, e.g. \"WhiteSmoke\", \"#ff0000\".
;;  BACKGROUND-COLOR  String, name or hex.
;;  MAGNITUDE         Float in [0,1], degree of stretching.
;; Returns:
;;  List: (NEW-TEXT-RGB NEW-BACKGROUND-RGB), each a list of 3 float [0,1]."
;;   ;; Get HSL representation for each color
;;   (print (list "selected-window-contrast-adjust-contrast" text-color background-color magnitude))
;;   (let* ((text-hsl (selected-window-contrast--color-to-hsl text-color))
;;          (bg-hsl   (selected-window-contrast--color-to-hsl background-color))
;;          ;; Extract lightness components
;;          (t-l (nth 2 text-hsl))
;;          (b-l (nth 2 bg-hsl))
;;          (mid 0.5) ; Midpoint between black and white

;;          ;; Compute new lightness for text:
;;          ;; If above mid, push up toward white; else, push down toward black.
;;          (new-t-l
;;           (if (> t-l mid)
;;               ;; Above midpoint: move proportionally toward white
;;               (+ mid (* magnitude (- 1.0 mid)))
;;             ;; Below or at midpoint: move proportionally toward black
;;             (- mid (* magnitude (- mid 0)))))

;;          ;; Compute new lightness for background:
;;          ;; Same logic as for text
;;          (new-b-l
;;           (if (> b-l mid)
;;               (+ mid (* magnitude (- 1.0 mid)))
;;             (- mid (* magnitude (- mid 0)))))

;;          ;; Construct new RGB colors by keeping original hue/saturation but substituting new lightness
;;          (new-text-rgb (apply #'color-hsl-to-rgb (list (nth 0 text-hsl) (nth 1 text-hsl) new-t-l)))
;;          (new-bg-rgb   (apply #'color-hsl-to-rgb (list (nth 0 bg-hsl)   (nth 1 bg-hsl)   new-b-l))))
;;     (list new-text-rgb new-bg-rgb)))
;; (selected-window-contrast-adjust-contrast "#383a42" "#fafafa" 0.8) ; ((0.09180327868852456 0.09508196721311472 0.1081967213114754) (0.9 0.9 0.9))


;; (defun selected-window-contrast-adjust-contrast (text-color background-color magnitude &optional text-ratio)
;;   "Maximize visual contrast between TEXT-COLOR and BACKGROUND-COLOR.
;; Lightness of each color is stretched away from mid value (0.5).
;; MAGNITUDE controls total strength (float from 0 to 1).

;; Optional TEXT-RATIO controls proportion of contrast change given to text color:
;;  - 0: only background.
;;  - 0.5: split evenly (default).
;;  - 1: only text.

;; Arguments:
;;  TEXT-COLOR      String, color name or hex (e.g. \"#ff0000\").
;;  BACKGROUND-COLOR String, color name or hex.
;;  MAGNITUDE       Float in [0,1], overall stretch.
;;  TEXT-RATIO      Float in [0,1], proportion for text. Default 0.5.

;; Returns:
;;  List: (NEW-TEXT-RGB NEW-BACKGROUND-RGB), each a list [R G B] of floats [0,1]."
;;   ;; Set default text-ratio if not given
;;   (setq text-ratio (if (null text-ratio) 0.5 text-ratio))
;;   (let* ((text-hsl (selected-window-contrast--color-to-hsl text-color))
;;          (bg-hsl   (selected-window-contrast--color-to-hsl background-color))
;;          (t-l (nth 2 text-hsl))
;;          (b-l (nth 2 bg-hsl))
;;          (mid 0.5)
;;          ;; Per-color magnitude
;;          (t-mag (* magnitude text-ratio))
;;          (b-mag (* magnitude (- 1 text-ratio)))
;;          ;; New text lightness
;;          (new-t-l (if (> t-l mid)
;;                       (+ mid (* t-mag (- 1.0 mid)))
;;                     (- mid (* t-mag (- mid 0)))))
;;          ;; New background lightness
;;          (new-b-l (if (> b-l mid)
;;                       (+ mid (* b-mag (- 1.0 mid)))
;;                     (- mid (* b-mag (- mid 0)))))
;;          (new-text-rgb (apply #'color-hsl-to-rgb (list (nth 0 text-hsl) (nth 1 text-hsl) new-t-l)))
;;          (new-bg-rgb   (apply #'color-hsl-to-rgb (list (nth 0 bg-hsl)   (nth 1 bg-hsl)   new-b-l))))
;;     (list new-text-rgb new-bg-rgb)))

;; (defun selected-window-contrast-adjust-contrast (text-color background-color magnitude &optional foreground-share)
;;   "Maximize visual contrast between TEXT-COLOR and BACKGROUND-COLOR.
;; This function remaps the lightness component of both input colors so that:
;; - Colors above the midpoint (0.5) are pushed towards full white.
;; - Colors below the midpoint are pushed towards full black.
;; Arguments:
;;  TEXT-COLOR        String, name or hex.
;;  BACKGROUND-COLOR  String, name or hex.
;;  MAGNITUDE (float in [0,1]) controls stretching.
;;  FOREGROUND-SHARE (float in [0,1], optional): If NON-nil, splits contrast adjustment.
;;   - 1: only text changes
;;   - 0: only background changes
;;   - 0.5: both get full magnitude (default)
;; Returns:
;;  List: (NEW-TEXT-RGB NEW-BACKGROUND-RGB), each as (R G B) floats in [0,1]."
;;   (print (list "selected-window-contrast-adjust-contrast" text-color background-color magnitude foreground-share))
;;   (let* ((text-hsl (selected-window-contrast--color-to-hsl text-color))
;;          (bg-hsl   (selected-window-contrast--color-to-hsl background-color))
;;          (t-l (nth 2 text-hsl))
;;          (b-l (nth 2 bg-hsl))
;;          (mid 0.5)
;;          ;; Decide split logic:
;;          (text-mag (if (or (null foreground-share)
;;                            (= foreground-share 0.5))
;;                        magnitude            ;; original full effect
;;                      (* magnitude foreground-share)))
;;          (bg-mag   (if (or (null foreground-share)
;;                            (= foreground-share 0.5))
;;                        magnitude            ;; original full effect
;;                      (* magnitude (- 1 foreground-share))))
;;          ;; Compute new lightness for text:
;;          (new-t-l
;;           (if (> t-l mid)
;;               (+ mid (* text-mag (- 1.0 mid)))
;;             (- mid (* text-mag (- mid 0)))))
;;          ;; Compute new lightness for background:
;;          (new-b-l
;;           (if (> b-l mid)
;;               (+ mid (* bg-mag (- 1.0 mid)))
;;             (- mid (* bg-mag (- mid 0)))))
;;          ;; Pack new RGB:
;;          (new-text-rgb (apply #'color-hsl-to-rgb (list (nth 0 text-hsl) (nth 1 text-hsl) new-t-l)))
;;          (new-bg-rgb   (apply #'color-hsl-to-rgb (list (nth 0 bg-hsl) (nth 1 bg-hsl) new-b-l))))
;;     (list new-text-rgb new-bg-rgb)))

(defun selected-window-contrast-adjust-contrast (text-color background-color bg-mag text-mag)
  "Maximize visual contrast between TEXT-COLOR and BACKGROUND-COLOR.
This function remaps the lightness component of both input colors so that:
- Colors above the midpoint (0.5) are pushed towards full white.
- Colors below the midpoint are pushed towards full black.
Arguments:
 TEXT-COLOR        String, name or hex.
 BACKGROUND-COLOR  String, name or hex.
 BG-MAG (float in [0,1]) controls stretching of contrast for background.
 TEXT-MAG (float in [0,1], optional): stretching of contrast for text.
Returns:
 List: (NEW-TEXT-RGB NEW-BACKGROUND-RGB), each as (R G B) floats in [0,1]."
  ;; (print (list "AA1" text-mag bg-mag)) ; ("AA1" 1.4400000000000002 0.15999999999999998)
  (let ((text-hsl (selected-window-contrast--color-to-hsl text-color))
        (bg-hsl   (selected-window-contrast--color-to-hsl background-color))
        (mid 0.5))
    (let ((res-text-rgb
           (if (not text-mag)
               (apply #'color-hsl-to-rgb text-hsl)
             ;; else
             (let* ((t-l (nth 2 text-hsl))
                    (new-t-l (if (> t-l mid)
                                 (+ mid (* text-mag (- 1.0 mid)))
                               (- mid (* text-mag (- mid 0))))))
               (apply #'color-hsl-to-rgb (list (nth 0 text-hsl) (nth 1 text-hsl) new-t-l)))))
          (res-bg-rgb (if (not bg-mag)
                          (apply #'color-hsl-to-rgb bg-hsl)
                        ;; else
                        (let* ((b-l (nth 2 bg-hsl))
                               (new-b-l (if (> b-l mid)
                                            (+ mid (* bg-mag (- 1.0 mid)))
                                          (- mid (* bg-mag (- mid 0))))))
                          (apply #'color-hsl-to-rgb (list (nth 0 bg-hsl) (nth 1 bg-hsl) new-b-l))))))
      (list res-text-rgb res-bg-rgb))))

;;for foreground-share=0.6 not gives result that should be between foreground-share=0.5 and foreground-share=1

;; (defun selected-window-contrast-adjust-contrast (text-color background-color magnitude &optional foreground-share)
;;   "Maximize visual contrast between TEXT-COLOR and BACKGROUND-COLOR.
;; This function remaps the lightness of both colors, interpolating smoothly between maximum and minimum adjustment:
;; - MAGNITUDE (float in [0,1]): controls stretching.
;; - FOREGROUND-SHARE (float in [0,1]): how much adjustment goes to text (1=text only, 0=background only).
;; Returns: (NEW-TEXT-RGB NEW-BACKGROUND-RGB)."

;;   (let* ((share (if (null foreground-share) 0.5 foreground-share))
;;          (text-hsl (selected-window-contrast--color-to-hsl text-color))
;;          (bg-hsl   (selected-window-contrast--color-to-hsl background-color))
;;          (t-l (nth 2 text-hsl)) ;; lightness of text
;;          (b-l (nth 2 bg-hsl))   ;; lightness of background
;;          (mid 0.5)
;;          ;; Compute fully-adjusted and midpoint lightness for both colors
;;          (max-t-l (if (> t-l mid)
;;                       (+ mid (* magnitude (- 1.0 mid)))    ;; push toward white
;;                     (- mid (* magnitude mid))))            ;; push toward black
;;          (max-b-l (if (> b-l mid)
;;                       (+ mid (* magnitude (- 1.0 mid)))
;;                     (- mid (* magnitude mid))))
;;          ;; Interpolate between midpoint (no effect) and max adjustment
;;          (new-t-l (+ mid (* (- max-t-l mid) share)))          ;; text: 0 (mid), 1 (max-adjusted)
;;          (new-b-l (+ mid (* (- max-b-l mid) (- 1 share)))))  ;; background: 1 (mid), 0 (max-adjusted)
;;     (print (list text-hsl bg-hsl magnitude foreground-share))
;;     (print (list max-t-l max-b-l))
;;     (list
;;      (apply #'color-hsl-to-rgb (list (nth 0 text-hsl) (nth 1 text-hsl) new-t-l))
;;      (apply #'color-hsl-to-rgb (list (nth 0 bg-hsl) (nth 1 bg-hsl) new-b-l)))))

;; For foreground-share=1

;; Usage examples:
;; 50/50 split (default behavior)
;; (selected-window-contrast-adjust-contrast "#383a42" "#fafafa" 0.8)
;; (selected-window-contrast-adjust-contrast "#383a42" "#fafafa" 0.8 0.5)

;; Only text gets contrast change
;; (selected-window-contrast-adjust-contrast "#383a42" "#fafafa" 0.8 1)

;; Only background gets contrast change
;; (selected-window-contrast-adjust-contrast "#383a42" "#fafafa" 0.8 0)


(defun selected-window-contrast--rgb-to-hex (rgb &optional digits)
  "Convert normalized RGB list to hex string.
RGB: list of 3 floats in [0,1].  DIGITS: 2 or 4 digits/component."
  (apply #'color-rgb-to-hex (append rgb (list (or digits 2)))))

;; (defun selected-window-contrast-apply-new-colors-rgb (text-rgb background-rgb)
;;   "Set buffer face colors from normalized RGB lists.
;; TEXT-RGB, BACKGROUND-RGB: lists of 3 floats in [0,1]."
;;   (print (list "selected-window-contrast--apply-new-colors text-rgb background-rgb" text-rgb background-rgb))
;;   (let ((fg (selected-window-contrast--rgb-to-hex text-rgb))
;;         (bg (selected-window-contrast--rgb-to-hex background-rgb)))
;;     (message "selected-window-contrast-apply-new-colors-rgb fg=%s bg=%s" fg bg)
;;     (buffer-face-set (list :foreground fg :background bg))))

(defun selected-window-contrast-change-window (contrast-background contrast-text)
  "Increase contrast between text and background in buffer.
CONTRAST-BACKGROUND, CONTRAST-TEXT: float in [0,1]; of contrast 1 - is
full contrast, 0 - no contrast.
Works on both dark (light text/dark bg) and light (dark text/light bg) themes."
  (unless (or (when (and contrast-background
                         (or (not (numberp contrast-background))
                             (not (<= 0 contrast-background 1))))
                (message "Contrast-background must be floats in [0,1]"))
              (when (and contrast-text
                         (or (not (numberp contrast-text))
                             (not (<= 0 contrast-text 1))))
                (message "contrast-text must be floats in [0,1]")))
    (let* ((current-colors (selected-window-contrast--get-current-colors))
           (new-colors (selected-window-contrast-adjust-contrast (nth 0 current-colors)
                                                                 (nth 1 current-colors)
                                                                 contrast-background
                                                                 contrast-text)))
      ;; (print (list "selected-window-contrast-change-window current-colors new-colors" contrast-background contrast-text))
      (let ((background-rgb (nth 1 new-colors))
            (text-rgb (nth 0 new-colors)))
        (if (and contrast-background contrast-text)
            ;; :foreground (selected-window-contrast--rgb-to-hex text-rgb)
            (buffer-face-set (list :foreground (selected-window-contrast--rgb-to-hex text-rgb)
                                   :background (selected-window-contrast--rgb-to-hex background-rgb)))
          ;; else
          (when contrast-text
            (buffer-face-set (list :foreground (selected-window-contrast--rgb-to-hex text-rgb)
                                   :background (face-attribute 'default :background))))
          (when contrast-background
            (buffer-face-set (list :foreground (face-attribute 'default :foreground)
                                   :background (selected-window-contrast--rgb-to-hex background-rgb)))))))))

      ;; (selected-window-contrast-apply-new-colors-rgb (nth 0 new-colors) (nth 1 new-colors)))))

(defun selected-window-contrast-change-modeline (contrast-background contrast-text)
  "Adjust modeline brightness of text and background.
Arguments CONTRAST-BACKGROUND, CONTRAST-TEXT is float value to increase
or decrease contrast."
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
      (let* ((new-colors (selected-window-contrast-adjust-contrast fore
                                               back
                                               contrast-background
                                               contrast-text))
             (new-fore (apply #'selected-window-contrast--rgb-to-hex (nth 0 new-colors)))
             (new-back (apply #'selected-window-contrast--rgb-to-hex (nth 1 new-colors))))
        (set-face-attribute 'mode-line-active nil
                            :foreground new-fore
                            :background new-back)
        t))))

;; (defun selected-window-contrast-highlight-selected-window () ; (&optional theme)
;;   "Highlight not selected windows with a different background color.
;; Timeout 0.01 sec.
;; Optional THEME argument required for enable-theme-functions hook."
;;   (selected-window-contrast-highlight-selected-window))
;; (setq fore (face-attribute 'default :foreground))
;; (setq back (face-attribute 'default :background))
;; (selected-window-contrast-adjust-contrast fore
;;                                                back
;;                                                0.5)

(defun selected-window-contrast-highlight-selected-window-with-timeout ()
  "Highlight not selected windows with a different background color.
Timeout 0.1 sec.
For case of opening new frame with new buffer by call:
$ emacsclient -c ~/file"
  (run-with-idle-timer 0.4 nil #'selected-window-contrast-highlight-selected-window))

(defun selected-window-contrast-highlight-selected-window ()
  "Highlight not selected windows with a different background color."
  (let ((cbn (buffer-name (current-buffer)))
        (sw (selected-window)))
    (when (/= (aref cbn 0) ?\s) ; ignore system buffers
      ;; - not selected:
      (walk-windows (lambda (w)
                        (unless (or (eq sw w)
                                    (eq cbn (buffer-name (window-buffer w))))
                          (with-selected-window w
                            (buffer-face-set 'default)
                            (selected-window-contrast-change-window
                             selected-window-contrast-bg-others
                             selected-window-contrast-contrast-text-others))))
                      -1 ) ; -1 means to not include minimuber

      ;; - selected:
      (selected-window-contrast-change-window selected-window-contrast-bg-selected selected-window-contrast-contrast-text-selected)
        ;; else
        ;; (buffer-face-set 'default)
        )))

(provide 'selected-window-contrast)
;;; selected-window-contrast.el ends here
