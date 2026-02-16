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
    (should (equal (length (selected-window-contrast--get-current-colors)) 2))))

(ert-deftest swc2-tests--a2 ()
  (with-temp-buffer
    (let* ((text-color "white")
           (background-color "black")
           (res (selected-window-contrast-adjust-contrast text-color background-color 0.3 nil)))
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


(ert-deftest swc2-tests--rgb-to-hex ()
  (let ((red 0.5)
        (green 0.5)
        (blue 0.5))
    (should (string-equal (selected-window-contrast--rgb-to-hex (list red green blue)) "#7f7f7f"))))


(provide 'swc2-tests)
;;; swc2-tests.el ends here
