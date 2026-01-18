;;; swc2-tests.el --- Accurate reduce contrast of non selected windows.   -*- lexical-binding: t -*-
;; emacs -Q --batch -l ert.el -l selected-window-contrast.el -l swc2-tests.el -f ert-run-tests-batch-and-exit
;; OR
;; (eval-buffer)
;; (ert t)
(require 'ert)
(require 'selected-window-contrast)

(ert-deftest swc2-tests--a1 ()
  (with-temp-buffer
    (should (listp (selected-window-contrast--get-current-colors)))
    (should (equal (length (selected-window-contrast--get-current-colors)) 2))
    )
)

(ert-deftest swc2-tests--a2 ()
  (with-temp-buffer
    (let* ((text-color "white")
           (background-color "black")
           (text-hsl (selected-window-contrast--color-to-hsl text-color))
           (background-hsl (selected-window-contrast--color-to-hsl background-color))
           (text-brightness (nth 2 text-hsl))
           (background-brightness (nth 2 background-hsl))
           (res (selected-window-contrast-adjust-contrast text-color background-color 0.3 nil)))
      ;; res))
      ;; (= 1.0 text-brightness)))
      (should (= 1.0 text-brightness))
      (should (= 0.0 background-brightness))
      (should (equal res '((1.0 1.0 1.0) (0.35 0.35 0.35)))))))

(ert-deftest swc2-tests--adjust-contrast-1 ()
  (with-temp-buffer
    (should (equal (selected-window-contrast-adjust-contrast "white" "black" 0 nil) '((1.0 1.0 1.0) (0.5 0.5 0.5))))
    (should (equal (selected-window-contrast-adjust-contrast "white" "black" 0.5 nil) '((1.0 1.0 1.0) (0.25 0.25 0.25))))
    (should (equal (selected-window-contrast-adjust-contrast "white" "black" 1 nil) '((1.0 1.0 1.0) (0.0 0.0 0.0))))

    (should (equal (selected-window-contrast-adjust-contrast "white" "black" nil 0) '((0.5 0.5 0.5) (0.0 0.0 0.0))))
    (should (equal (selected-window-contrast-adjust-contrast "white" "black" nil 1) '((1.0 1.0 1.0) (0.0 0.0 0.0))))

    (should (equal (selected-window-contrast-adjust-contrast "black" "white" 0.5 nil) '((0.0 0.0 0.0) (0.75 0.75 0.75))))
    (should (equal (selected-window-contrast-adjust-contrast "black" "white" 1 nil) '((0.0 0.0 0.0) (1.0 1.0 1.0))))))


;; (should (stringp (selected-window-contrast--rgb-to-hex red green blue)))))
    ;; (setq fore "#7d8c820b93fb")
    ;; (setq back "#a587a587a636")
    ;; (should (equal t (selected-window-contrast-change-window 1.2 1.2)))
    ;; (selected-window-contrast-change-modeline 1.2 1.2)
    ;; (should (equal t (selected-window-contrast-change-modeline 1.2 1.2)))

(ert-deftest swc2-tests--rgb-to-hex ()
  (let ((red 0.5)
        (green 0.5)
        (blue 0.5))
    (should (string-equal (selected-window-contrast--rgb-to-hex (list red green blue)) "#7f7f7f"))))


(ert-deftest swc2-tests--parse-color1 ()
  (should (string-equal "#fafafa"
                (selected-window-contrast--rgb-to-hex (selected-window-contrast--parse-color "#fafafa")))))

(ert-deftest swc2-tests--parse-color2 ()
  (should (equal (selected-window-contrast--parse-color "#ff0000")
                 (list 1.0 0.0 0.0)))

  ;; 24-bit, pure green
  (should (equal (selected-window-contrast--parse-color "#00ff00")
                 (list 0.0 1.0 0.0)))

  ;; 24-bit, black
  (should (equal (selected-window-contrast--parse-color "#000000")
                 (list 0.0 0.0 0.0)))

  ;; 24-bit, white
  (should (equal (selected-window-contrast--parse-color "#ffffff")
                 (list 1.0 1.0 1.0)))

    ;; Test edge: different uppercase/lowercase
  (should (equal (selected-window-contrast--parse-color "#Ff00FF")
                 (list 1.0 0.0 1.0))))

(ert-deftest swc2-tests--hex-to-hsl ()
  ;; Typical hex: red
  (should (equal (selected-window-contrast--color-to-hsl "#ff0000")
                 '(0.0 1.0 0.5)))

  ;; 48-bit hex: green (#0000ffff0000)
  (should (equal (selected-window-contrast--color-to-hsl "#0000ffff0000")
                 '(0.3333333333333333 1.0 0.5)))

  ;; Named color: red
  (should (equal (selected-window-contrast--color-to-hsl "red")
                 '(0.0 1.0 0.5)))

  ;; (print "wtf222" (color-name-to-rgb "chartreuse"))
  ;; Named color: chartreuse
  (should (or
           (equal (selected-window-contrast--color-to-hsl "chartreuse")
                  '(0.3333333333333333 1.0 0.5))
           (equal (selected-window-contrast--color-to-hsl "chartreuse")
                 '(0.2503267973856209 1.0 0.5))))
  ;; ;; Typical hex: red
  (should
   (equal (selected-window-contrast--color-to-hsl "#ff0000")
          (color-rgb-to-hsl 1.0 0.0 0.0)))
  )
;; (ert-deftest swc2-tests--a4 ()
  ;; (with-temp-buffer

  ;;   (let ((text-color "white")
  ;;         (background-color "black"))
  ;;     (print (swc2--adjust-brightness text-color background-color))
  ;;     (print (swc2--adjust-brightness-2 text-color background-color))
  ;;     ;; (should (equal (swc2--adjust-brightness text-color background-color)
  ;;     ;;                (swc2--adjust-brightness-2 text-color background-color))))
  ;;   )
;; )

(provide 'swc2-tests)
;;; swc2-tests.el ends here
