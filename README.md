# selected-window-contrast
Configure contrast between text and background to mark selected window and mode-line.

Allow to adjust brightness of text and background for selected and not selected window.

Allow to increase and decrease constrast between text and background separately.

# Usage for selected window
```Elisp
(require 'selected-window-contrast)

;; - increase contrast for selected window
(setopt selected-window-contrast-selected-magnitude-text 0.8)  ; default = 1
(setopt selected-window-contrast-selected-magnitude-background 0.9)  ; default = 1
;; - decrease conrtrast for other windows
(setopt selected-window-contrast-not-sel-magnitude-text 1.1)  ; default = 1
(setopt selected-window-contrast-not-sel-magnitude-background 1.1)  ; default = 1

(add-hook 'buffer-list-update-hook 'selected-window-contrast-highlight-selected-window)
```

# Usage for modeline
```Elisp
(progn
  ;; - reset mode-line to default. (optional)
  (set-face-attribute 'mode-line-active nil
                      :background
                      (face-attribute 'mode-line :background)
                      :foreground
                      (face-attribute 'mode-line :foreground))
  ;; - set backgound color (optional)
  (set-face-attribute 'mode-line-active nil :background "cyan4")
  ;; - increate contrast
  (selected-window-contrast-change-modeline 2 0.7)
  )
```
# One time usage
Change contrast in current buffer ``` (selected-window-contrast-change-window 0.7 0.7) ```

# How this works
1) We get color with ```face-attribute (selected-frame)``` for foreground and backgraound.
2) Convert color to HSL
3) adjust brightness in direction of foreground-background average
4) convert color to RGB, then to HEX
5) apply color with ```(buffer-face-set (list :background bg :foreground fg)```. For modeline: ```(set-face-attribute 'mode-line-active```


# Original idea:
```Elisp
(defun highlight-selected-window ()
  "Highlight selected window with a different background color."
  (walk-windows (lambda (w)
                  (unless (eq w (selected-window))
                    (with-current-buffer (window-buffer w)
                      (buffer-face-set '(:background "#111"))))))
  (buffer-face-set 'default))
(add-hook 'buffer-list-update-hook 'highlight-selected-window)
```
from https://emacs.stackexchange.com/questions/24630/is-there-a-way-to-change-color-of-active-windows-fringe
