;;; brb-test.el --- Tests for brb.el -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Unit tests for pure functions in brb.el.

;;; Code:

(require 'ert)
(require 'brb)

;;; * Price formatting

(ert-deftest brb-test-price-format-positive ()
  "Test formatting positive amounts."
  (should (string= (brb-price-format 100) "100 UAH"))
  (should (string= (brb-price-format 1000) "1 000 UAH"))
  (should (string= (brb-price-format 12345) "12 345 UAH"))
  (should (string= (brb-price-format 1234567) "1 234 567 UAH")))

(ert-deftest brb-test-price-format-rounds-up ()
  "Test that price formatting rounds up."
  (should (string= (brb-price-format 99.1) "100 UAH"))
  (should (string= (brb-price-format 99.9) "100 UAH"))
  (should (string= (brb-price-format 100.01) "101 UAH")))

(ert-deftest brb-test-price-format-nil ()
  "Test formatting nil amount."
  (should (string= (brb-price-format nil) "– UAH")))

(ert-deftest brb-test-price-format-zero ()
  "Test formatting zero."
  (should (string= (brb-price-format 0) "0 UAH")))

(ert-deftest brb-test-price-to-number ()
  "Test converting price string to number."
  (should (= (brb-price-to-number "100 UAH") 100))
  (should (= (brb-price-to-number "1 000 UAH") 1000))
  (should (= (brb-price-to-number "12 345 UAH") 12345)))

(ert-deftest brb-test-price-to-number-nil ()
  "Test converting nil price representation."
  (should (null (brb-price-to-number "– UAH"))))

(ert-deftest brb-test-price-to-number-wrong-currency ()
  "Test that wrong currency returns nil."
  (should (null (brb-price-to-number "100 USD"))))

(ert-deftest brb-test-to-price ()
  "Test parsing price string to alist."
  (let ((result (brb-to-price "100 UAH")))
    (should (= (alist-get 'amount result) 100))
    (should (string= (alist-get 'currency result) "UAH"))))

(ert-deftest brb-test-to-price-invalid ()
  "Test that invalid price string signals error."
  (should-error (brb-to-price "invalid"))
  (should-error (brb-to-price "100"))
  (should-error (brb-to-price "-100 UAH")))

;;; * Number grouping

(ert-deftest brb-test-string-group-number-basic ()
  "Test basic number grouping."
  (should (string= (brb-string-group-number 1000) "1 000"))
  (should (string= (brb-string-group-number 1000000) "1 000 000"))
  (should (string= (brb-string-group-number 123456789) "123 456 789")))

(ert-deftest brb-test-string-group-number-small ()
  "Test that small numbers are not grouped."
  (should (string= (brb-string-group-number 1) "1"))
  (should (string= (brb-string-group-number 12) "12"))
  (should (string= (brb-string-group-number 123) "123"))
  (should (string= (brb-string-group-number 999) "999")))

(ert-deftest brb-test-string-group-number-negative ()
  "Test grouping negative numbers."
  (should (string= (brb-string-group-number -1000) "-1 000"))
  (should (string= (brb-string-group-number -1234567) "-1 234 567")))

(ert-deftest brb-test-string-group-number-custom-separator ()
  "Test custom separator."
  (should (string= (brb-string-group-number 1000000 3 ",") "1,000,000")))

(ert-deftest brb-test-string-group-number-custom-size ()
  "Test custom group size."
  (should (string= (brb-string-group-number 12345678 4 " ") "1234 5678")))

(ert-deftest brb-test-string-group-number-string-input ()
  "Test that string input works."
  (should (string= (brb-string-group-number "1000") "1 000")))

;;; * Price round-trip

(ert-deftest brb-test-price-round-trip ()
  "Test that formatting and parsing are inverse operations."
  (dolist (amount '(0 1 100 999 1000 12345 100000 1234567))
    (should (= (brb-price-to-number (brb-price-format amount)) amount))))

;;; * String table

(ert-deftest brb-test-string-table-basic ()
  "Test basic table formatting."
  (let ((result (brb-string-table
                 :data '(("a" "b") ("c" "d"))
                 :sep " | ")))
    (should (string= result "a | b\nc | d"))))

(ert-deftest brb-test-string-table-with-header ()
  "Test table with header."
  (let ((result (brb-string-table
                 :data '(("1" "2") ("3" "4"))
                 :header '("X" "Y")
                 :sep " ")))
    (should (string-match-p "X Y" result))
    (should (string-match-p "1 2" result))))

(ert-deftest brb-test-string-table-padding ()
  "Test that columns are padded to equal width."
  (let ((result (brb-string-table
                 :data '(("a" "long") ("short" "b"))
                 :sep "|")))
    ;; All rows should have the same length
    (let ((lines (split-string result "\n")))
      (should (= (length (nth 0 lines)) (length (nth 1 lines)))))))

;;; * QPR calculation

(ert-deftest brb-test-qpr-returns-nil-for-invalid-inputs ()
  "Test that QPR returns nil for invalid inputs."
  (should (null (brb-qpr 0 3.5 nil)))      ; zero price
  (should (null (brb-qpr 100 0 nil)))      ; zero score
  (should (null (brb-qpr 100 nil nil)))    ; nil score
  (should (null (brb-qpr nil 3.5 nil))))   ; nil price

(ert-deftest brb-test-qpr-returns-number ()
  "Test that QPR returns a number for valid inputs."
  ;; Create a mock wine note
  (cl-letf (((symbol-function 'vulpea-note-meta-get)
             (lambda (_note key &optional _type)
               (pcase key
                 ("volume" 750)
                 ("carbonation method" nil)))))
    (let ((result (brb-qpr 1000 4.0 'mock-wine)))
      (should (numberp result))
      (should (> result 0)))))

(ert-deftest brb-test-qpr-higher-score-means-higher-qpr ()
  "Test that higher score results in higher QPR."
  (cl-letf (((symbol-function 'vulpea-note-meta-get)
             (lambda (_note key &optional _type)
               (pcase key
                 ("volume" 750)
                 ("carbonation method" nil)))))
    (let ((qpr-low (brb-qpr 1000 2.0 'mock-wine))
          (qpr-high (brb-qpr 1000 4.0 'mock-wine)))
      (should (> qpr-high qpr-low)))))

(ert-deftest brb-test-qpr-higher-price-means-lower-qpr ()
  "Test that higher price results in lower QPR."
  (cl-letf (((symbol-function 'vulpea-note-meta-get)
             (lambda (_note key &optional _type)
               (pcase key
                 ("volume" 750)
                 ("carbonation method" nil)))))
    (let ((qpr-cheap (brb-qpr 500 3.5 'mock-wine))
          (qpr-expensive (brb-qpr 2000 3.5 'mock-wine)))
      (should (> qpr-cheap qpr-expensive)))))

(ert-deftest brb-test-qpr-traditional-method-multiplier ()
  "Test that traditional method sparkling wines get higher QPR."
  (let ((qpr-still nil)
        (qpr-sparkling nil))
    (cl-letf (((symbol-function 'vulpea-note-meta-get)
               (lambda (_note key &optional _type)
                 (pcase key
                   ("volume" 750)
                   ("carbonation method" nil)))))
      (setq qpr-still (brb-qpr 1000 3.5 'mock-wine)))
    (cl-letf (((symbol-function 'vulpea-note-meta-get)
               (lambda (_note key &optional _type)
                 (pcase key
                   ("volume" 750)
                   ("carbonation method" "traditional")))))
      (setq qpr-sparkling (brb-qpr 1000 3.5 'mock-wine)))
    (should (> qpr-sparkling qpr-still))))

(ert-deftest brb-test-qpr-volume-normalization ()
  "Test that QPR normalizes to 750ml equivalent."
  (let ((qpr-750 nil)
        (qpr-375 nil))
    (cl-letf (((symbol-function 'vulpea-note-meta-get)
               (lambda (_note key &optional _type)
                 (pcase key
                   ("volume" 750)
                   ("carbonation method" nil)))))
      (setq qpr-750 (brb-qpr 1000 3.5 'mock-wine)))
    ;; Half bottle at half price should have same QPR
    (cl-letf (((symbol-function 'vulpea-note-meta-get)
               (lambda (_note key &optional _type)
                 (pcase key
                   ("volume" 375)
                   ("carbonation method" nil)))))
      (setq qpr-375 (brb-qpr 500 3.5 'mock-wine)))
    ;; Should be equal (or very close due to floating point)
    (should (< (abs (- qpr-750 qpr-375)) 0.01))))

(provide 'brb-test)
;;; brb-test.el ends here
