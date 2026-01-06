;;; brb-event.el --- Wine tasting event management -*- lexical-binding: t; -*-

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
;; Event management for Barberry Garden wine tastings.
;;
;; This module provides functions to:
;; - Query and select events (`brb-event-select', `brb-events-from-range')
;; - Create new events (`brb-event-create')
;; - Access event metadata (wines, participants, dates)
;; - Calculate event statements and summaries with scoring statistics
;;
;; Events are org-mode notes (via vulpea) tagged with `brb-event-tags'.
;; Each event can have associated data stored in a companion .data.el file
;; for complex structures that don't fit in org metadata.
;;

;;; Code:

(require 'vulpea)
(require 'brb)

;;; * Configurations

(defconst brb-event-narrator-id "bc8aa837-3348-45e6-8468-85510966527a"
  "ID of Barberry Garden narrator.

This must point to a valid `vulpea' note tagged as _people_.")

(defconst brb-event-tags '("wine" "event" "barberry/public")
  "List of tags that denote event note.")

;;; * Event selection

(defun brb-event-select ()
  "Interactively select an event note.

If visiting an event buffer, uses this event as initial prompt."
  (let* ((event (when (and (eq major-mode 'org-mode)
                           (buffer-file-name))
                  (when-let* ((id (save-excursion
                                    (goto-char (point-min))
                                    (org-id-get)))
                              (note (vulpea-db-get-by-id id)))
                    (when (--every-p (-contains-p (vulpea-note-tags note) it) brb-event-tags)
                      note)))))
    (vulpea-select-from
     "Event"
     (--filter
      (= 0 (vulpea-note-level it))
      (vulpea-db-query-by-tags-every brb-event-tags))
     :require-match t
     :initial-prompt (when event (vulpea-note-title event)))))

(defun brb-events-from-range (from to)
  "Return list of events in a time range denoted by FROM and TO."
  (->> (vulpea-db-query-by-tags-every brb-event-tags)
       (--filter (= 0 (vulpea-note-level it)))
       (--filter (let ((date (brb-event-date-string it)))
                   (and (org-time>= date from)
                        (org-time< date to))))
       (--sort (org-time< (brb-event-date-string it)
                          (brb-event-date-string other)))))

(defun brb-events-without-date ()
  "Return list of events without any date set."
  (->> (vulpea-db-query-by-tags-every brb-event-tags)
       (--filter (= 0 (vulpea-note-level it)))
       (-remove #'brb-event-date-string)))

;;; * Metadata

(defun brb-event-date-string (event)
  "Return date of EVENT as ISO date string (YYYY-MM-DD)."
  (vulpea-utils-with-note event
    (when-let ((str (vulpea-buffer-prop-get "date")))
      (org-read-date nil nil str))))

;;; * Event creation

;;;###autoload
(defun brb-event-create ()
  "Create a new wine tasting event interactively.

Prompts for event name, URL slug, and date, then creates a vulpea note
with predefined structure for event management."
  (interactive)
  (let* ((name (read-string "Name: "))
         (slug (read-string "Slug: "))
         (date-str
          (with-temp-buffer
            (let ((date (org-read-date nil t nil "Date: ")))
              (org-insert-timestamp date nil t)
              (buffer-substring (point-min) (point-max))))))
    (vulpea-create
     name
     "wine/event/%<%Y%m%d%H%M%S>-${slug}.org"
     :tags (-concat brb-event-tags '("barberry/public" "barberry/post"))
     :head (->> (list (cons "date" date-str)
                      (cons "slug" slug)
                      (cons "tags" "report")
                      (cons "language" "EN")
                      (cons "author" "Boris")
                      (cons "description" "")
                      (cons "image" ""))
                (--map (format "#+%s: %s" (car it) (cdr it)))
                (s-join "\n"))
     :body (string-join
            '("- publish :: false"
              "- time to book :: N/A"
              ""
              "#+begin_src event_summary"
              "skip:"
              "- outs"
              "hideExtraWines: false"
              "#+end_src"
              ""
              "* Raw scores"
              ""
              "#+begin_src event_personal_scores"
              "hideExtraWines: false"
              "#+end_src"
              ""
              "* Notes                                                            :noexport:")
            "\n")
     :body "- publish :: false\n- time to book :: N/A\n"
     :immediate-finish t)))

;;; * Wines

(defun brb-event-wines (event)
  "Return list of wines from EVENT."
  (vulpea-note-meta-get-list event "wines" 'note))

;;; * Participants

(defun brb-event-participants (event)
  "Return list of participants from EVENT."
  (vulpea-note-meta-get-list event "participants" 'note))

;;; * Data
;;
;; There is a lot of information that is stored alongside with event which can
;; not be stored as vulpea metadata due to non-trivial structure.
;;
;; Hence each event comes with an associated el file that contains all the extra
;; data.
;;
;; This data is used in various modules, but modifications are done inside `brb-event-plan'.

(defun brb-event--data-file (event)
  "Return path to data file of EVENT."
  (file-name-with-extension (vulpea-note-path event) "data.el"))

(defun brb-event-data-read (event)
  "Read data for EVENT.

The result contains all the extra data for EVENT that can't be
stored as metadata in vulpea-note. It has the following
structure:

  ((planned-participants . num)
   (shared . (((item . str)
               (amount . num)
               (price . num))))
   (personal . (((item . str)
                 (price . num)
                 (orders . (((participant . id)
                             (amount . num)))))))
   (participants . ((id . id)
                    (fee . num)))
   (wines . (((id . id)
             (price-public . num)
             (price-real . num)
             (price-asking . num)
             (participants . (id))
             (type . str)
             (ignore-scores . bool)
             (scores . (((participant . id)
                         (score . num)
                         (sentiment . str))))))))"
  (let ((file (brb-event--data-file event)))
    (when (file-exists-p file)
      (with-temp-buffer
        (condition-case nil
	          (progn
	            (insert-file-contents file)
              (read (current-buffer)))
	        (error
	         (message "Could not read data from %s" file)))))))

(defun brb-event-data-write (event data)
  "Write DATA for EVENT."
  (let ((file (brb-event--data-file event)))
    (with-temp-file file
      (let ((print-level nil)
	          (print-length nil))
	      (pp data (current-buffer))))))

;;; ** Statement

(cl-defun brb-event-statement (event &key data participants wines host balances)
  "Prepare statement for EVENT.

DATA is loaded unless provided.
HOST is loaded unless provided.
PARTICIPANTS are loaded unless provided.
WINES are loaded unless provided.
BALANCES is a hash table."
  (declare (indent 1))
  (let* ((data (or data (brb-event-data-read event)))
         (participants (or participants (brb-event-participants event)))
         (wines (or wines (brb-event-wines event)))
         (host (or host (vulpea-note-meta-get event "host" 'note)))
         (balances (or balances (make-hash-table :test 'equal)))

         ;; calculations
         ;; (price (vulpea-note-meta-get event "price" 'number))
         (wines-normal (->> data
                            (assoc-default 'wines)
                            (--filter (-contains-p '("welcome" "normal") (assoc-default 'type it)))))
         (wines-extra (->> data
                           (assoc-default 'wines)
                           (--filter (string-equal "extra" (assoc-default 'type it)))))

         (spending-shared (->> data
                               (assoc-default 'shared)
                               (--map (ceiling
                                       (* (assoc-default 'amount it)
                                          (assoc-default 'price it))))
                               (-sum)))
         (spending-order (->> data
                              (alist-get 'personal)
                              (--map
                               (->> (alist-get 'orders it)
                                    (-map (lambda (od)
                                            (let ((amount (if (string-equal brb-event-narrator-id
                                                                            (alist-get 'participant od))
                                                              0
                                                            (alist-get 'amount od))))
                                              (* amount (alist-get 'price it)))))
                                    (-sum)))
                              (-sum)))
         (spending-wines-public (->> wines-normal
                                     (--map (assoc-default 'price-public it))
                                     (--filter it)
                                     (-sum)))
         (spending-wines-real (->> wines-normal
                                   (--map (assoc-default 'price-real it))
                                   (--filter it)
                                   (-sum)))
         (spending-extra-public (->> wines-extra
                                     (--map (assoc-default 'price-public it))
                                     (--filter it)
                                     (-sum)))
         (spending-extra-real (->> wines-extra
                                   (--map (assoc-default 'price-real it))
                                   (--filter it)
                                   (-sum)))
         (spending-expense-wines (->> data
                                      (assoc-default 'expense-wines)
                                      (--map (ceiling
                                              (* (assoc-default 'amount it)
                                                 (assoc-default 'price it))))
                                      (-sum)))

         (credit-public (+ spending-wines-public spending-extra-public spending-shared spending-expense-wines))
         (credit-real (+ spending-wines-real spending-extra-real spending-shared spending-expense-wines))
         (debit-base (->> participants
                          (--map (alist-get 'fee (brb-event-statement-for event it
                                                                          :data data
                                                                          :host host
                                                                          :wines wines
                                                                          :balances balances
                                                                          :participants participants)))
                          (-sum)))
         (debit-extra (->> wines-extra
                           (--map
                            (let* ((ps (-remove-item brb-event-narrator-id (assoc-default 'participants it)))
                                   (glass-price (brb-event--glass-price it)))
                              (* glass-price (length ps))))
                           (-sum)))
         (debit (+ debit-base debit-extra))
         (balance-public (- debit credit-public))
         (balance-real (- debit credit-real)))
    `((spending-shared . ,spending-shared)
      (spending-order . ,spending-order)
      (spending-wines-public . ,spending-wines-public)
      (spending-wines-real . ,spending-wines-real)
      (spending-extra-public . ,spending-extra-public)
      (spending-extra-real . ,spending-extra-real)
      (spending-expense-wines . ,spending-expense-wines)
      (credit-public . ,credit-public)
      (credit-real . ,credit-real)
      (debit-base . ,debit-base)
      (debit-extra . ,debit-extra)
      (debit . ,debit)
      (balance-public . ,balance-public)
      (balance-real . ,balance-real))))

;;;###autoload
(cl-defun brb-event-empty-statement-for (event participant balances)
  "Prepare statement for PARTICIPANT of EVENT.

DATA is loaded unless provided.
WINES is a list of `vulpea-note'. Loaded unless provided.
BALANCES is a hash table."
  (let* ((use-balance (pcase (or (vulpea-note-meta-get event "use balance") "true")
                        ("true" t)
                        (_ nil)))
         (pid (vulpea-note-id participant))
         (fee 0)
         (mode "normal")
         (order nil)
         (extra nil)
         (balance (if use-balance
                      (or (gethash pid balances) 0)
                    0))
         (total 0)
         (due 0)
         (balance-final balance))
    `((balance . ,balance)
      (balance-final . ,balance-final)
      (mode . ,mode)
      (fee . ,fee)
      (order . ,order)
      (extra . ,extra)
      (total . ,total)
      (due . ,due))))

(cl-defun brb-event-statement-for (event participant &key data host wines balances participants)
  "Prepare financial statement for PARTICIPANT of EVENT.

Returns an alist with fee, orders, extra wines, and balance calculations.

DATA is event data, loaded unless provided.
HOST is a `vulpea-note', loaded unless provided.
WINES is a list of `vulpea-note', loaded unless provided.
BALANCES is a hash table mapping participant IDs to their balance.
PARTICIPANTS is list of `vulpea-note' for all participants."
  (let* ((data (or data (brb-event-data-read event)))
         (balances (or balances (make-hash-table :test 'equal)))
         (use-balance (pcase (or (vulpea-note-meta-get event "use balance") "true")
                        ("true" t)
                        (_ nil)))
         (host (or host (vulpea-note-meta-get event "host" 'note)))
         (wines (or wines (brb-event-wines event)))
         (participants (or participants (brb-event-participants event)))
         (host-id (when host (vulpea-note-id host)))
         (pid (vulpea-note-id participant))
         (price (or (vulpea-note-meta-get event "price" 'number) 0))
         (host-p (string-equal pid host-id))
         ;; pays-for is stored inside each participant entry: ((id . "X") (pays-for . ("Y" "Z")))
         (participants-data (alist-get 'participants data))
         ;; Check if someone else is paying for this participant
         (paid-by (->> participants-data
                       (--find (-contains-p (alist-get 'pays-for it) pid))
                       (alist-get 'id)))
         ;; Get list of people this participant pays for
         (paying-for (->> participants-data
                          (--find (string-equal pid (alist-get 'id it)))
                          (alist-get 'pays-for)))
         ;; Calculate base fee (0 if host or if someone else is paying)
         (base-fee (cond
                    (host-p 0)
                    (paid-by 0)  ; Someone else is paying for this person
                    (t (or (->> data
                                (alist-get 'participants)
                                (--find (string-equal pid (alist-get 'id it)))
                                (alist-get 'fee))
                           price))))
         ;; Add fees for people we're paying for
         (paying-for-fees
          (if paying-for
              (-sum (--map
                     (let* ((payee-id it)
                            (payee-fee (or (->> data
                                                (alist-get 'participants)
                                                (--find (string-equal payee-id (alist-get 'id it)))
                                                (alist-get 'fee))
                                           price)))
                       payee-fee)
                     paying-for))
            0))
         (fee (+ base-fee paying-for-fees))
         (mode (cond
                (host-p "host")
                ((/= fee price) "custom")
                (t "normal")))
         ;; Helper to get orders for a participant id
         (get-orders-for
          (lambda (target-pid)
            (->> data
                 (alist-get 'personal)
                 (--map
                  (let* ((od (->> (alist-get 'orders it)
                                  (--find (string-equal target-pid (alist-get 'participant it)))))
                         (amount (or (when od (alist-get 'amount od))
                                     0)))
                    `((item . ,(alist-get 'item it))
                      (price . ,(alist-get 'price it))
                      (amount . ,amount)
                      (total . ,(* amount (alist-get 'price it))))))
                 (--filter (> (alist-get 'amount it) 0)))))
         ;; Helper to get extras for a participant id
         (get-extras-for
          (lambda (target-pid)
            (->> data
                 (alist-get 'wines)
                 (--filter (string-equal "extra" (assoc-default 'type it)))
                 (--filter (-contains-p (assoc-default 'participants it) target-pid))
                 (--map
                  (let* ((ps (alist-get 'participants it))
                         (wid (alist-get 'id it))
                         (price (or (alist-get 'price-asking it)
                                    (alist-get 'price-public it)))
                         (glass-price (ceiling (/ price (float (length ps)))))
                         (wine (--find (string-equal wid (vulpea-note-id it)) wines)))
                    `((glass-price . ,glass-price)
                      (amount . 1)
                      (total . ,glass-price)
                      (wine . ,wine))))
                 (--filter (alist-get 'wine it)))))
         ;; If someone is paying for this participant, they get 0 orders/extras
         ;; Otherwise get their own orders/extras
         (order (if paid-by nil (funcall get-orders-for pid)))
         (extra (if paid-by nil (funcall get-extras-for pid)))
         ;; Add orders for people we're paying for
         (paying-for-orders
          (if paying-for
              (-flatten-n 1 (--map (funcall get-orders-for it) paying-for))
            nil))
         ;; Add extras for people we're paying for
         (paying-for-extras
          (if paying-for
              (-flatten-n 1 (--map (funcall get-extras-for it) paying-for))
            nil))
         ;; Sum of balances for people we're paying for
         (paying-for-balances
          (if (and paying-for use-balance)
              (-sum (--map (or (gethash it balances) 0) paying-for))
            0))
         ;; Own balance (0 if someone else is paying for us - our balance goes to them)
         (own-balance (if use-balance
                          (if paid-by 0 (or (gethash pid balances) 0))
                        0))
         ;; Total effective balance = own + those we're paying for
         (balance (+ own-balance paying-for-balances))
         (total (+ fee
                   (-sum (--map (alist-get 'total it) order))
                   (-sum (--map (alist-get 'glass-price it) extra))
                   (-sum (--map (alist-get 'total it) paying-for-orders))
                   (-sum (--map (alist-get 'glass-price it) paying-for-extras))))
         (due (max 0 (- total balance)))
         (balance-final (- balance total)))
    `((balance . ,balance)
      (own-balance . ,own-balance)
      (paying-for-balances . ,paying-for-balances)
      (balance-final . ,balance-final)
      (mode . ,mode)
      (fee . ,fee)
      (base-fee . ,base-fee)
      (paying-for . ,paying-for)
      (paying-for-fees . ,paying-for-fees)
      (paying-for-orders . ,paying-for-orders)
      (paying-for-extras . ,paying-for-extras)
      (paid-by . ,paid-by)
      (order . ,order)
      (extra . ,extra)
      (total . ,total)
      (due . ,due))))

(defun brb-event-statement-add (s1 s2)
  "Add two statements S1 and S2."
  (let* ((balance (+ (alist-get 'balance s1)
                     (alist-get 'balance s2)))
         (balance-final (+ (alist-get 'balance-final s1)
                           (alist-get 'balance-final s2)))
         (mode (alist-get 'mode s1))
         (fee (+ (alist-get 'fee s1)
                 (alist-get 'fee s2)))
         (order (let* ((o1 (alist-get 'order s1))
                       (o2 (alist-get 'order s2))
                       (items (-uniq (append
                                      (--map (alist-get 'item it) o1)
                                      (--map (alist-get 'item it) o2)))))
                  (--map
                   (let* ((item it)
                          (i1 (--find (string-equal item (alist-get 'item it)) o1))
                          (i2 (--find (string-equal item (alist-get 'item it)) o2))
                          (price (alist-get 'price (or i1 i2)))
                          (amount (+ (or (alist-get 'amount i1) 0)
                                     (or (alist-get 'amount i2) 0)))
                          (total (+ (or (alist-get 'total i1) 0)
                                    (or (alist-get 'total i2) 0))))
                     `((item . ,item)
                       (price . ,price)
                       (amount . ,amount)
                       (total . ,total)))
                   items)))
         (extra (--reduce-from
                 (let* ((extra it)
                        (data (--find
                               (string-equal (vulpea-note-id (alist-get 'wine it))
                                             (vulpea-note-id (alist-get 'wine extra)))
                               acc))
                        (glass-price (alist-get 'glass-price extra))
                        (amount (+ (alist-get 'amount extra) (or (alist-get 'amount data) 0)))
                        (total (+ (alist-get 'total extra) (or (alist-get 'total data) 0)))
                        (wine (alist-get 'wine extra)))
                   (-snoc
                    (--remove (string-equal (vulpea-note-id (alist-get 'wine it))
                                            (vulpea-note-id (alist-get 'wine extra)))
                              acc)
                    `((glass-price . ,glass-price)
                      (amount . ,amount)
                      (total . ,total)
                      (wine . ,wine))))
                 nil
                 (append (alist-get 'extra s1)
                         (alist-get 'extra s2))))
         (total (+ (alist-get 'total s1)
                   (alist-get 'total s2)))
         (due (max 0 (- total balance))))
    `((balance . ,balance)
      (balance-final . ,balance-final)
      (mode . ,mode)
      (fee . ,fee)
      (order . ,order)
      (extra . ,extra)
      (total . ,total)
      (due . ,due))))

(cl-defun brb-event--glass-price (data)
  "Calculate glass price of an extra wine.

DATA is an alist with the following fields:

- price-asking - optional number
- price-public - number
- participants - amount of participating that pay for wine

Result is always a number."
  (let* ((price (or (assoc-default 'price-asking data)
                    (assoc-default 'price-public data)))
         (ps (assoc-default 'participants data)))
    (if ps (ceiling (/ price (float (length ps)))) 0)))

;;; ** Summary

(cl-defun brb-event-summary (event &key data wines participants)
  "Return score summary of EVENT.
Optional DATA, WINES, PARTICIPANTS override reading from event."
  (let* ((data (or data (brb-event-data-read event)))
         (wines (or wines (brb-event-wines event)))
         (participants (or participants (brb-event-participants event)))
         (weight-def 2)
         (weights (->> participants
                       (--map
                        (let ((weight (or (vulpea-note-meta-get
                                           it
                                           "tasting level"
                                           'number)
                                          weight-def)))
                          `((participant . ,(vulpea-note-id it))
                            (weight . ,(calc-from-number (* weight weight))))))))
         (wines-data (->>
                      wines
                      (-map
                       (lambda (wine)
                         (let ((data (--find (string-equal (vulpea-note-id wine) (alist-get 'id it))
                                             (alist-get 'wines data))))
                           (unless data
                             (error "Could not find scores data for %s" (vulpea-note-id wine)))
                           (let* ((scores (->> data
                                               (alist-get 'scores)
                                               (--map (alist-get 'score it))
                                               (--filter it)
                                               (-map #'calc-from-number)))
                                  (amean (when scores
                                           (calc-to-number (apply #'calcFunc-vmean scores))))
                                  (rms (when scores
                                         (calc-to-number (calcFunc-rms (apply #'calcFunc-vec scores)))))
                                  (use-weights (pcase (vulpea-note-meta-get event "weights")
                                                 (`"false" nil)
                                                 (_ t)))
                                  (weights-sum (->> weights
                                                    (--filter (-contains-p
                                                               (->> data
                                                                    (alist-get 'scores)
                                                                    (--filter (alist-get 'score it))
                                                                    (--map (alist-get 'participant it)))
                                                               (alist-get 'participant it)))
                                                    (--map (if use-weights (alist-get 'weight it) 1))
                                                    (apply #'calcFunc-vec)
                                                    (calcFunc-vsum)
                                                    (calc-to-number)))
                                  (wtotal (->> data
                                               (alist-get 'scores)
                                               (--filter (alist-get 'score it))
                                               (--map
                                                (let* ((pid (alist-get 'participant it))
                                                       (weight-data (--find
                                                                     (string-equal pid (alist-get 'participant it))
                                                                     weights)))
                                                  (unless weight-data
                                                    (error "Could not find weight data for participant %s for '%s' event" pid (vulpea-note-title event)))
                                                  (calcFunc-mul (if use-weights (assoc-default 'weight weight-data) 1)
                                                                (assoc-default 'score it))))
                                               (apply #'calcFunc-vec)
                                               (calcFunc-vsum)))
                                  (wavg (when (and scores (> weights-sum 0)) (/ wtotal weights-sum)))
                                  (price (alist-get 'price-public data))
                                  (qpr (brb-qpr price wavg wine))
                                  (sdev (when scores
                                          (calc-to-number (apply #'calcFunc-vpvar scores))))
                                  (fav (->> data
                                            (alist-get 'scores)
                                            (--map (alist-get 'sentiment it))
                                            (--count (string-equal "favourite" it))))
                                  (out (->> data
                                            (alist-get 'scores)
                                            (--map (alist-get 'sentiment it))
                                            (--count (string-equal "outcast" it))))
                                  (pscores (->>
                                            participants
                                            (--map
                                             (let* ((pid (vulpea-note-id it))
                                                    (sd (--find
                                                         (string-equal pid (alist-get 'participant it))
                                                         (alist-get 'scores data))))
                                               `((participant . ,it)
                                                 (score . ,(alist-get 'score sd))
                                                 (sentiment . ,(alist-get 'sentiment sd))))))))
                             `((wine . ,wine)
                               (ignore-scores . ,(alist-get 'ignore-scores data))
                               (type . ,(alist-get 'type data))
                               (amean . ,amean)
                               (rms . ,rms)
                               (wavg . ,wavg)
                               (sdev . ,sdev)
                               (fav . ,fav)
                               (out . ,out)
                               (price . ((amount . ,price)
                                         (currency . ,brb-currency)))
                               (qpr . ,qpr)
                               (scores . ,pscores))))))))
         (prices (->> data
                      (alist-get 'wines)
                      (--remove (alist-get 'ignore-scores it))
                      (--map (alist-get 'price-public it))
                      (--filter it)
                      (--remove (= 0 it))))
         (wines-price-total (when prices (-sum prices)))
         (wines-price-harmonic (when prices
                                 (->> prices
                                      (-map #'calc-from-number)
                                      (apply #'calcFunc-vec)
                                      (calcFunc-vhmean)
                                      (calc-to-number))))
         (wines-price-median (when prices
                               (->> prices
                                    (-map #'calc-from-number)
                                    (apply #'calcFunc-vec)
                                    (calcFunc-vmedian)
                                    (calc-to-number))))
         (rms-scores (->> wines-data
                          (--remove (alist-get 'ignore-scores it))
                          (--map (alist-get 'rms it))
                          (--filter it)
                          (-map #'calc-from-number)))
         (wavg-scores (->> wines-data
                           (--remove (alist-get 'ignore-scores it))
                           (--map (alist-get 'wavg it))
                           (--filter it)
                           (-map #'calc-from-number)))
         (event-rms (when rms-scores
                      (->> rms-scores
                           (apply #'calcFunc-vec)
                           (calcFunc-rms)
                           (calc-to-number))))
         (event-wavg (when wavg-scores
                       (->> wavg-scores
                            (apply #'calcFunc-vec)
                            (calcFunc-rms)
                            (calc-to-number))))
         (event-qpr (when (and event-wavg prices)
                      (brb-qpr wines-price-harmonic event-wavg event))))
    `((wines . ,wines-data)
      (wines-price-total . ,wines-price-total)
      (wines-price-harmonic . ,wines-price-harmonic)
      (wines-price-median . ,wines-price-median)
      (rms . ,event-rms)
      (wavg . ,event-wavg)
      (qpr . ,event-qpr))))

(provide 'brb-event)
;;; brb-event.el ends here
