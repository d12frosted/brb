;;; brb-widget.el --- Custom widgets for brb UIs -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026  Boris Buliga

;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; URL: https://github.com/d12frosted/brb

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Custom Emacs widgets for brb user interfaces.
;;
;; Provides specialized label widgets for displaying:
;; - `money-label' - monetary values formatted with `brb-currency'
;; - `balance-label' - balances with positive/negative coloring
;; - `balance-reversed-label' - balances with inverted coloring (for debts)
;; - `note-label' - vulpea note titles

;;; Code:

(require 'brb)
(require 'widget-extra)

(define-widget 'money-label 'label
  "A label for monetary value using `brb-currency'."
  :format-value (lambda (_widget value) (brb-price-format value)))

(define-widget 'balance-label 'money-label
  "A label for balance with positive/negative coloring.

Positive values are shown as success, negative as warning."
  :face (lambda (_widget value)
          (cond
           ((> value 0) 'success)
           ((< value 0) 'warning)
           (t 'success))))

(define-widget 'balance-reversed-label 'money-label
  "A label for balance with inverted coloring.

Positive values are shown as warning (debt owed), negative as success."
  :face (lambda (_widget value)
          (cond
           ((> value 0) 'warning)
           ((< value 0) 'success)
           (t 'warning))))

(define-widget 'note-label 'label
  "A label for `vulpea-note'."
  :format-value (lambda (_widget value) (vulpea-note-title value)))

(provide 'brb-widget)
;;; brb-widget.el ends here
