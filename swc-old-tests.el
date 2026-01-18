;;; swc-old-tests.el --- Accurate reduce contrast of non selected windows.   -*- lexical-binding: t -*-
;; emacs -Q --batch -l ert.el -l selected-window-contrast-old.el -l swc-tests.el -f ert-run-tests-batch-and-exit
;; OR
;; (eval-buffer)
;; (ert t)
(require 'ert)
(require 'selected-window-contrast-old)

(ert-deftest swc-tests--a1 ()
  (with-temp-buffer
    (should (listp (selected-window-contrast-old--get-current-colors)))
    (should (equal (length (selected-window-contrast-old--get-current-colors)) 2))
    )
)

(ert-deftest swc-tests--a2 ()
  (with-temp-buffer
    (let* ((text-color "white")
          (background-color "black")
          (magnitude-text 1.2)
          (magnitude-back 1.2)
          (text-hsl (selected-window-contrast-old--hex-to-hsl text-color))
          (background-hsl (selected-window-contrast-old--hex-to-hsl background-color))
          (text-brightness (nth 2 text-hsl))
          (background-brightness (nth 2 background-hsl))
          (average-brightness (/ (+ text-brightness background-brightness) 2))
          (new-text-brightness (+ average-brightness (/ (- average-brightness background-brightness)
                                                        magnitude-text)))
          (new-background-brightness (- average-brightness (/ (- text-brightness average-brightness)
                                                              magnitude-back))))
      (should (<= 0 new-text-brightness 1))
      (should (<= 0 new-background-brightness 1))
      (should (<= 0 (nth 0 (color-hsl-to-rgb (nth 0 text-hsl) (nth 1 text-hsl) new-text-brightness)) 1))
      (should (<= 0 (nth 1 (color-hsl-to-rgb (nth 0 text-hsl) (nth 1 text-hsl) new-text-brightness)) 1))
      (should (<= 0 (nth 2 (color-hsl-to-rgb (nth 0 text-hsl) (nth 1 text-hsl) new-text-brightness)) 1))
      (should (<= 0 (nth 0 (color-hsl-to-rgb (nth 0 background-hsl) (nth 1 background-hsl) new-background-brightness)) 1))
      (should (<= 0 (nth 1 (color-hsl-to-rgb (nth 0 background-hsl) (nth 1 background-hsl) new-background-brightness)) 1))
      (should (<= 0 (nth 2 (color-hsl-to-rgb (nth 0 background-hsl) (nth 1 background-hsl) new-background-brightness)) 1)))
    )
)

(ert-deftest swc-tests--a3 ()
  (with-temp-buffer
    (should (listp (selected-window-contrast-old--adjust-brightness "white" "black" 1.2 1.2)))
    (should (equal (length (selected-window-contrast-old--adjust-brightness "white" "black" 1.2 1.2)) 2))

    (should (stringp (selected-window-contrast-old--rgb-to-hex 1.0 1.0 1.0)))

    (let ((red 0.5)
          (green 0.5)
          (blue 0.5))
      (should (stringp (selected-window-contrast-old--rgb-to-hex red green blue))))
    ;; (setq fore "#7d8c820b93fb")
    ;; (setq back "#a587a587a636")
    ;; (should (equal t (swc-change-window 1.2 1.2)))
    ;; (swc-change-modeline 1.2 1.2)
    ;; (should (equal t (swc-change-modeline 1.2 1.2)))
    ))

(ert-deftest swc-tests--hex-to-rgb ()
  (should (equal (selected-window-contrast-old--hex-to-rgb "#ff0000")
                 (list 1.0 0.0 0.0)))

  ;; 24-bit, pure green
  (should (equal (selected-window-contrast-old--hex-to-rgb "#00ff00")
                 (list 0.0 1.0 0.0)))

  ;; 24-bit, black
  (should (equal (selected-window-contrast-old--hex-to-rgb "#000000")
                 (list 0.0 0.0 0.0)))

  ;; 24-bit, white
  (should (equal (selected-window-contrast-old--hex-to-rgb "#ffffff")
                 (list 1.0 1.0 1.0)))

  ;; 48-bit color, cyan (#0000ffff0000)
  (should (equal (selected-window-contrast-old--hex-to-rgb "#0000ffff0000" 4)
                 (list 0.0 1.0 0.0)))

  ;; 24-bit, with explicit digits-per-component = 2
  (should (equal (selected-window-contrast-old--hex-to-rgb "#ff0000" 2)
                 (list 1.0 0.0 0.0)))

  ;; Test edge: different uppercase/lowercase
  (should (equal (selected-window-contrast-old--hex-to-rgb "#Ff00FF")
                 (list 1.0 0.0 1.0))))

(ert-deftest swc-tests--hex-to-hsl ()
  ;; Typical hex: red
  (should (equal (selected-window-contrast-old--hex-to-hsl "#ff0000")
                 (color-rgb-to-hsl (/ 1.0 65535.0) 0.0 0.0)))

  ;; ;; 48-bit hex: green (#0000ffff0000)
  (let ((rgb (selected-window-contrast-old--hex-to-rgb "#0000ffff0000" 4)))
    (should (equal (selected-window-contrast-old--hex-to-hsl "#0000ffff0000")
                   (apply #'color-rgb-to-hsl (mapcar (lambda (x) (/ x 65535.0)) rgb)))))

  ;; Named color: red
  (should (equal (selected-window-contrast-old--hex-to-hsl "red")
                 (color-rgb-to-hsl (/ (nth 0 (color-name-to-rgb "red")) 65535.0)
                                   (/ (nth 1 (color-name-to-rgb "red")) 65535.0)
                                   (/ (nth 2 (color-name-to-rgb "red")) 65535.0))))

  ;; Named color: chartreuse
  (should (equal (selected-window-contrast-old--hex-to-hsl "chartreuse")
                 (color-rgb-to-hsl (/ (nth 0 (color-name-to-rgb "chartreuse")) 65535.0)
                                   (/ (nth 1 (color-name-to-rgb "chartreuse")) 65535.0)
                                   (/ (nth 2 (color-name-to-rgb "chartreuse")) 65535.0))))
  ;; ;; Typical hex: red
  ;; (should
  ;;  (equal (swc--hex-to-hsl "#ff0000")
  ;;         (color-rgb-to-hsl 1.0 0.0 0.0)))

  ;; ;; 48-bit hex: green (#0000ffff0000)
  ;; (let ((rgb (swc--hex-to-rgb "#0000ffff0000" 4)))
  ;; (should (equal (swc--hex-to-hsl "#0000ffff0000")
  ;;                (apply #'color-rgb-to-hsl rgb))))

  ;; ;; Named color: red
  ;; (let ((rgb (color-name-to-rgb "red")))
  ;; (should (equal (swc--hex-to-hsl "red")
  ;;                (apply #'color-rgb-to-hsl rgb))))

  ;; ;; Named color: chartreuse
  ;; (let ((rgb (color-name-to-rgb "chartreuse")))
  ;; (should (equal (swc--hex-to-hsl "chartreuse")
  ;;                (apply #'color-rgb-to-hsl rgb))))

  )
;; (ert-deftest swc-tests--a4 ()
  ;; (with-temp-buffer

  ;;   (let ((text-color "white")
  ;;         (background-color "black"))
  ;;     (print (swc--adjust-brightness text-color background-color))
  ;;     (print (swc--adjust-brightness-2 text-color background-color))
  ;;     ;; (should (equal (swc--adjust-brightness text-color background-color)
  ;;     ;;                (swc--adjust-brightness-2 text-color background-color))))
  ;;   )
;; )

(provide 'swc-old-tests)
;;; swc-old-tests.el ends here
