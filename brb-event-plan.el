;;; brb-event-plan.el --- Event planning UI -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Boris Buliga

;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>

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
;; Widget-based UI for planning and managing wine tasting events.
;;
;; Main entry point is `brb-event-plan' which opens an interactive buffer
;; for the selected event with tabs for different aspects:
;; - plan: budget, wines, participants, shared spending
;; - scores: live scoring during event
;; - order: personal food/drink orders
;; - extra: extra wine sales (by-the-glass)
;; - invoices: invoice generation and ledger integration
;;
;; Architecture:
;; - Central state in `brb-plan-state' struct
;; - Zone-based rendering for efficient partial updates
;; - State changes trigger redraw of affected zones only
;;
;; Known Issues (widget.el limitations):
;;
;; 1. Cursor jumps on edit in Order tab - when editing quantity, the
;;    entire orders-personal zone redraws and cursor position is lost.
;;    Line+column restoration helps but isn't perfect. This is fundamental
;;    to widget.el - no concept of widget identity across redraws.
;;
;; 2. No fine-grained updates - can't update single table cell, must
;;    redraw entire zone. Zone granularity is coarse (e.g., all
;;    participants' orders in one zone).
;;
;; 3. Lambda capture in loops - must use backquoted lambdas with `,var`
;;    to properly capture loop variables in widget callbacks.
;;
;; 4. Nested anaphoric macros - dash.el's `--map`, `--filter`, `--each`
;;    all use `it`, causing shadowing bugs when nested.
;;
;; 5. Compound widgets need special handling - menu-choice in tables
;;    requires `widget-convert` and manual `:value-get`.
;;

;;; Code:

(require 'cl-lib)
(require 'dash)

(require 'vulpea)
(require 'vino)

(require 'widget)
(require 'wid-edit)
(require 'widget-extra)

(require 'brb)
(require 'brb-event)
(require 'brb-ledger)

;;; * State management

(cl-defstruct (brb-plan-state (:constructor brb-plan-state--create))
  "State for event planning UI."
  buffer        ; display buffer
  event         ; vulpea-note for the event
  data          ; event data (from .data.el file)
  tab           ; current tab string
  host          ; vulpea-note for host
  participants  ; list of vulpea-note
  waiting       ; waiting list (vulpea-notes)
  wines         ; list of vulpea-note
  balances)     ; hash-table pid->balance

(defvar-local brb-plan--state nil
  "Buffer-local state for event planning UI.")

(defun brb-plan-state-create (event)
  "Create a new state for EVENT."
  (let ((data (brb-event-data-read event)))
    (brb-plan-state--create
     :event event
     :data data
     :tab "plan"
     :host (vulpea-note-meta-get event "host" 'note)
     :participants (brb-event-participants event)
     :waiting (vulpea-note-meta-get-list event "waiting" 'note)
     :wines (brb-event-wines event)
     :balances (make-hash-table :test 'equal))))

(defun brb-plan--save-data (state)
  "Save data from STATE to file."
  (brb-event-data-write
   (brb-plan-state-event state)
   (brb-plan-state-data state)))

(defun brb-plan--set-event-meta (state key value)
  "Set event metadata KEY to VALUE in STATE.
Updates the org file and refreshes the in-memory event struct."
  (let* ((event (brb-plan-state-event state))
         (meta (vulpea-note-meta event)))
    ;; Update org file
    (vulpea-utils-with-note event
      (vulpea-buffer-meta-set key value 'append)
      (save-buffer))
    ;; Update in-memory struct's meta
    (setf (vulpea-note-meta event)
          (cons (list key (format "%s" value))
                (--remove (string-equal key (car it)) meta)))))

;;; * Zone infrastructure

(defvar brb-plan--zone-deps
  '((planned-participants . (forecast finances))
    (price . (forecast finances participants))
    (host . (general))
    (wines . (wines finances scores-wines scores-personal))
    (participants . (participants finances scores-personal))
    (waiting . (waiting))
    (shared . (shared finances))
    (personal . (orders-summary orders-personal finances))
    (extra . (extra-wines finances))
    (scores . (scores-summary scores-wines scores-personal finances)))
  "Mapping from data keys to zones that depend on them.")

(defun brb-plan--zone-start (name)
  "Insert zone start marker for zone NAME."
  (insert (propertize "\u200B" 'brb-zone-start name 'invisible t)))

(defun brb-plan--zone-end (name)
  "Insert zone end marker for zone NAME."
  (insert (propertize "\u200B" 'brb-zone-end name 'invisible t)))

(defun brb-plan--find-zone (name)
  "Find zone NAME in current buffer.

Returns (START . END) or nil if not found."
  (save-excursion
    (goto-char (point-min))
    (let ((start nil)
          (end nil))
      ;; Find start
      (while (and (not start) (< (point) (point-max)))
        (when (eq name (get-text-property (point) 'brb-zone-start))
          (setq start (point)))
        (forward-char 1))
      ;; Find end
      (when start
        (while (and (not end) (< (point) (point-max)))
          (when (eq name (get-text-property (point) 'brb-zone-end))
            (setq end (1+ (point))))
          (forward-char 1)))
      (when (and start end)
        (cons start end)))))

(defun brb-plan--redraw-zone (state zone &optional skip-cursor-restore)
  "Redraw ZONE in buffer for STATE.
If SKIP-CURSOR-RESTORE is non-nil, don't restore cursor position.
Returns (ZONE . DELTA) if point was in this zone, nil otherwise."
  (when-let ((bounds (brb-plan--find-zone zone)))
    (let* ((inhibit-read-only t)
           (zone-start (car bounds))
           (zone-end (cdr bounds))
           (point-in-zone (and (>= (point) zone-start)
                               (<= (point) zone-end)))
           ;; Save relative position within zone
           (delta (when point-in-zone (- (point) zone-start))))
      (save-excursion
        (delete-region zone-start zone-end)
        (goto-char zone-start)
        (brb-plan--render-zone state zone))
      ;; Restore cursor position within zone (unless skipped)
      (when (and point-in-zone (not skip-cursor-restore))
        (let ((new-bounds (brb-plan--find-zone zone)))
          (when new-bounds
            (goto-char (min (+ (car new-bounds) delta)
                            (cdr new-bounds))))))
      (widget-setup)
      ;; Return info for caller to handle cursor restoration
      (when point-in-zone
        (cons zone delta)))))

(defun brb-plan--redraw-zones (state zones)
  "Redraw multiple ZONES in buffer for STATE."
  (let ((cursor-info nil))
    ;; First pass: redraw all zones, find which one had cursor
    (dolist (zone zones)
      (when-let ((info (brb-plan--redraw-zone state zone t)))
        (setq cursor-info info)))
    ;; Restore cursor in the zone that had it
    (when cursor-info
      (let* ((zone (car cursor-info))
             (delta (cdr cursor-info))
             (new-bounds (brb-plan--find-zone zone)))
        (when new-bounds
          (goto-char (min (+ (car new-bounds) delta)
                          (cdr new-bounds))))))))

(defun brb-plan--update (state key value)
  "Update KEY to VALUE in STATE data, redraw affected zones."
  (setf (alist-get key (brb-plan-state-data state)) value)
  (brb-plan--save-data state)
  (brb-plan--redraw-zones state (alist-get key brb-plan--zone-deps)))

;;; * Zone renderers

(defun brb-plan--render-zone (state zone)
  "Render ZONE for STATE at point."
  (brb-plan--zone-start zone)
  (pcase zone
    ('header (brb-plan--render-header state))
    ('general (brb-plan--render-general state))
    ('forecast (brb-plan--render-forecast state))
    ('finances (brb-plan--render-finances state))
    ('wines (brb-plan--render-wines state))
    ('shared (brb-plan--render-shared state))
    ('expense (brb-plan--render-expense state))
    ('participants (brb-plan--render-participants state))
    ('waiting (brb-plan--render-waiting state))
    ('scores-summary (brb-plan--render-scores-summary state))
    ('scores-wines (brb-plan--render-scores-wines state))
    ('scores-personal (brb-plan--render-scores-personal state))
    ('orders-summary (brb-plan--render-orders-summary state))
    ('orders-personal (brb-plan--render-orders-personal state))
    ('extra-wines (brb-plan--render-extra-wines state))
    ('invoices-actions (brb-plan--render-invoices-actions state))
    ('invoices-settings (brb-plan--render-invoices-settings state))
    ('invoices-personal (brb-plan--render-invoices-personal state))
    (_ (widget-insert (format "[Zone %s not implemented]\n" zone))))
  (brb-plan--zone-end zone))

(defun brb-plan--render-header (state)
  "Render header zone for STATE."
  (let ((event (brb-plan-state-event state))
        (tab (brb-plan-state-tab state)))
    (widget-create 'title (vulpea-note-title event))
    (widget-create 'horizontal-choice
                   :value tab
                   :values '("plan" "scores" "order" "extra" "invoices")
                   :notify (lambda (widget &rest _)
                             (setf (brb-plan-state-tab state)
                                   (widget-value widget))
                             (brb-plan--display state)))
    (widget-insert "\n")
    (widget-create 'action-button
                   :value "Open note"
                   :notify (lambda (&rest _)
                             (vulpea-visit (brb-plan-state-event state))))
    (widget-insert " | ")
    (widget-create 'action-button
                   :value "Refresh"
                   :notify (lambda (&rest _)
                             (brb-plan--reload state)))
    (widget-insert "\n\n")))

(defun brb-plan--render-general (state)
  "Render general settings zone for STATE."
  (let ((event (brb-plan-state-event state)))
    (widget-create 'heading-2 "General")
    (widget-create
     'fields-group
     (list
      'person-field
      :tag "Host:"
      :value (brb-plan-state-host state)
      :notify (lambda (widget &rest _)
                (let ((host (widget-value widget)))
                  (setf (brb-plan-state-host state) host)
                  (vulpea-utils-with-note (brb-plan-state-event state)
                    (vulpea-buffer-meta-set "host" host 'append)
                    (save-buffer)))))
     (list
      'bounded-int-field
      :tag "Price:"
      :min-value 0
      :value (or (vulpea-note-meta-get event "price" 'number) 0)
      :format-value (lambda (_widget val) (brb-price-format val))
      :notify (lambda (widget &rest _)
                (brb-plan--set-event-meta state "price" (widget-value widget))
                (brb-plan--redraw-zones state '(forecast finances participants)))))
    (widget-insert "\n")))

(defun brb-plan--render-forecast (state)
  "Render forecast zone for STATE."
  (let* ((event (brb-plan-state-event state))
         (data (brb-plan-state-data state))
         (price (or (vulpea-note-meta-get event "price" 'number) 0))
         (planned (or (alist-get 'planned-participants data) 0))
         (debit (* planned price)))
    (widget-create 'heading-2 "Forecast")
    (widget-create
     'fields-group
     (list
      'bounded-int-field
      :tag "Participants:"
      :min-value 0
      :value planned
      :notify (lambda (widget &rest _)
                (brb-plan--update state
                                  'planned-participants
                                  (widget-value widget))))
     (list
      'numeric-label
      :tag "Debit:"
      :value debit
      :format-value (lambda (_widget val) (brb-price-format val))))
    (widget-insert "\n")))

(defun brb-plan--render-finances (state)
  "Render finances zone for STATE."
  (let* ((event (brb-plan-state-event state))
         (data (brb-plan-state-data state))
         (participants (brb-plan-state-participants state))
         (wines (brb-plan-state-wines state))
         (host (brb-plan-state-host state))
         (balances (brb-plan-state-balances state))
         (statement (brb-event-statement event
                                         :data data
                                         :participants participants
                                         :wines wines
                                         :host host
                                         :balances balances))
         (spending-shared (alist-get 'spending-shared statement))
         (spending-wines-public (alist-get 'spending-wines-public statement))
         (spending-wines-real (alist-get 'spending-wines-real statement))
         (spending-extra-public (alist-get 'spending-extra-public statement))
         (spending-extra-real (alist-get 'spending-extra-real statement))
         (spending-expense (alist-get 'spending-expense-wines statement))
         (credit-public (alist-get 'credit-public statement))
         (credit-real (alist-get 'credit-real statement))
         (debit-base (alist-get 'debit-base statement))
         (debit-extra (alist-get 'debit-extra statement))
         (debit (alist-get 'debit statement))
         (balance-public (alist-get 'balance-public statement))
         (balance-real (alist-get 'balance-real statement)))
    (widget-create 'heading-2 "Finances")
    (widget-create
     'table
     :padding-type '((1 . left) (2 . left))
     '(row (label :value "")
       (label :value "Public")
       (label :value "Real"))
     '(hline)
     `(row (label :value "Spending (shared)")
       (label :value ,(brb-price-format spending-shared))
       (label :value ""))
     `(row (label :value "Spending (wines)")
       (label :value ,(brb-price-format spending-wines-public))
       (label :value ,(brb-price-format spending-wines-real)))
     `(row (label :value "Spending (extra)")
       (label :value ,(brb-price-format spending-extra-public))
       (label :value ,(brb-price-format spending-extra-real)))
     `(row (label :value "Spending (expense)")
       (label :value ,(brb-price-format spending-expense))
       (label :value ""))
     `(row (label :value "Spending (total)")
       (label :value ,(brb-price-format credit-public))
       (label :value ,(brb-price-format credit-real)))
     '(hline)
     `(row (label :value "Debit (base)")
       (label :value ,(brb-price-format debit-base))
       (label :value ""))
     `(row (label :value "Debit (extra)")
       (label :value ,(brb-price-format debit-extra))
       (label :value ""))
     `(row (label :value "Debit (total)")
       (label :value ,(brb-price-format debit))
       (label :value ""))
     '(hline)
     `(row (label :value "Gain")
       (label :value ,(brb-price-format balance-public)
        :face ,(if (>= balance-public 0) 'success 'error))
       (label :value ,(brb-price-format balance-real)
        :face ,(if (>= balance-real 0) 'success 'error))))
    (widget-insert "\n")))

(defun brb-plan--wine-data (state wine-id)
  "Get wine data for WINE-ID from STATE."
  (--find (string-equal wine-id (alist-get 'id it))
          (alist-get 'wines (brb-plan-state-data state))))

(defun brb-plan--wine-set-data (state wine-id key value)
  "Set KEY to VALUE in wine data for WINE-ID in STATE."
  (let* ((data (brb-plan-state-data state))
         (wines-data (alist-get 'wines data))
         (wine-data (--find (string-equal wine-id (alist-get 'id it)) wines-data)))
    (if wine-data
        (setf (alist-get key wine-data) value)
      ;; Create new entry
      (push `((id . ,wine-id) (,key . ,value)) wines-data)
      (setf (alist-get 'wines data) wines-data))
    (brb-plan--save-data state)
    ;; Only redraw finances - table handles its own update
    (brb-plan--redraw-zones state '(finances))))

(defun brb-plan--wine-remove (state wine-id)
  "Remove wine with WINE-ID from STATE."
  (let* ((event (brb-plan-state-event state))
         (wines (->> (brb-plan-state-wines state)
                     (--remove (string-equal wine-id (vulpea-note-id it))))))
    ;; Update state
    (setf (brb-plan-state-wines state) wines)
    ;; Update event metadata
    (vulpea-utils-with-note event
      (vulpea-buffer-meta-set "wines" wines 'append)
      (save-buffer))
    ;; Remove from data
    (let ((data (brb-plan-state-data state)))
      (setf (alist-get 'wines data)
            (--remove (string-equal wine-id (alist-get 'id it))
                      (alist-get 'wines data)))
      (brb-plan--save-data state))
    (brb-plan--redraw-zones state '(wines finances))))

(defun brb-plan--wine-add (state)
  "Add a wine to STATE via selection."
  (when-let ((wine (vulpea-select-from
                    "Wine"
                    (vulpea-db-query-by-tags-every '("wine" "cellar")))))
    (let* ((event (brb-plan-state-event state))
           (wines (append (brb-plan-state-wines state) (list wine))))
      ;; Update state
      (setf (brb-plan-state-wines state) wines)
      ;; Update event metadata
      (vulpea-utils-with-note event
        (vulpea-buffer-meta-set "wines" wines 'append)
        (save-buffer))
      ;; Add to data with defaults
      (let* ((data (brb-plan-state-data state))
             (wines-data (alist-get 'wines data))
             (entry `((id . ,(vulpea-note-id wine))
                      (type . "normal")
                      (price-public . 0)
                      (price-real . 0))))
        (push entry wines-data)
        (setf (alist-get 'wines data) wines-data)
        (brb-plan--save-data state))
      (brb-plan--redraw-zones state '(wines finances)))))

(defun brb-plan--render-wines (state)
  "Render wines zone for STATE."
  (let ((wines (brb-plan-state-wines state)))
    (widget-create 'heading-2 "Wines")
    (apply
     #'widget-create
     'table
     :truncate '((1 . 20) (2 . 48))
     '(hline)
     '(row (label :value "")
       (label :value "Producer")
       (label :value "Wine")
       (label :value "Year")
       (label :value "P.Pub")
       (label :value "P.Real")
       (label :value "Type"))
     '(hline)
     (append
      ;; Wine rows
      (--map
       (let* ((wine it)
              (wine-id (vulpea-note-id wine))
              (wine-data (brb-plan--wine-data state wine-id))
              (producer (or (vulpea-note-title
                             (vulpea-note-meta-get wine "producer" 'note))
                            ""))
              (name (or (vulpea-note-meta-get wine "name") ""))
              (vintage (or (vulpea-note-meta-get wine "vintage" 'number) "NV"))
              (price-public (or (alist-get 'price-public wine-data) 0))
              (price-real (or (alist-get 'price-real wine-data) 0))
              (wine-type (or (alist-get 'type wine-data) "normal")))
         `(row
           (action-button
            :value "x"
            :action-data ,wine-id
            :notify (lambda (w &rest _)
                      (brb-plan--wine-remove ,state (widget-get w :action-data))))
           (label :value ,producer)
           (link-button :value ,name :target ,wine)
           (label :value ,(if (numberp vintage) (number-to-string vintage) vintage))
           (bounded-int-field
            :value ,price-public
            :min-value 0
            :action-data ,wine-id
            :notify (lambda (w &rest _)
                      (brb-plan--wine-set-data
                       ,state (widget-get w :action-data)
                       'price-public (widget-value w))))
           (bounded-int-field
            :value ,price-real
            :min-value 0
            :action-data ,wine-id
            :notify (lambda (w &rest _)
                      (brb-plan--wine-set-data
                       ,state (widget-get w :action-data)
                       'price-real (widget-value w))))
           (menu-choice
            :value ,wine-type
            :tag ,wine-type
            :format "%[%t%]"
            ;; Custom :value-get since we don't use %v in format
            ;; (default widget-child-value-get expects children)
            :value-get (lambda (w) (widget-get w :value))
            :action-data ,wine-id
            :notify (lambda (w &rest _)
                      (brb-plan--wine-set-data
                       ,state (widget-get w :action-data)
                       'type (widget-value w))
                      ;; Notify parent table to trigger redraw
                      (widget-default-action w)
                      ;; Redraw finances since wine type affects calculations
                      (brb-plan--redraw-zones ,state '(finances)))
            (choice-item :tag "normal" :value "normal")
            (choice-item :tag "welcome" :value "welcome")
            (choice-item :tag "bonus" :value "bonus")
            (choice-item :tag "extra" :value "extra"))))
       wines)
      ;; Footer row
      `((hline)
        (row
         (action-button
          :value "+"
          :notify (lambda (&rest _)
                    (brb-plan--wine-add ,state)))
         (label :value "")
         (label :value "")
         (label :value "")
         (label :value "")
         (label :value "")
         (label :value "")))))
    (widget-insert "\n")))

(defun brb-plan--shared-update (state index key value)
  "Update shared spending at INDEX, set KEY to VALUE in STATE."
  (let* ((data (brb-plan-state-data state))
         (shared (alist-get 'shared data))
         (item (nth index shared)))
    (when item
      (setf (alist-get key item) value)
      (brb-plan--save-data state)
      ;; Only redraw finances - table handles its own update
      (brb-plan--redraw-zones state '(finances)))))

(defun brb-plan--shared-remove (state index)
  "Remove shared spending at INDEX from STATE."
  (let* ((data (brb-plan-state-data state))
         (shared (alist-get 'shared data)))
    (setf (alist-get 'shared data)
          (-remove-at index shared))
    (brb-plan--save-data state)
    (brb-plan--redraw-zones state '(shared finances))))

(defun brb-plan--shared-add (state)
  "Add a new shared spending item to STATE."
  (let* ((item-name (read-string "Item name: "))
         (data (brb-plan-state-data state))
         (shared (alist-get 'shared data))
         (entry `((item . ,item-name) (amount . 1) (price . 0))))
    (setf (alist-get 'shared data)
          (append shared (list entry)))
    (brb-plan--save-data state)
    (brb-plan--redraw-zones state '(shared finances))))

(defun brb-plan--render-shared (state)
  "Render shared spending zone for STATE."
  (let* ((data (brb-plan-state-data state))
         (shared (alist-get 'shared data)))
    (widget-create 'heading-2 "Shared Spending")
    (apply
     #'widget-create
     'table
     :truncate '((1 . 24))
     '(hline)
     '(row (label :value "")
       (label :value "Item")
       (label :value "Amount")
       (label :value "Price")
       (label :value "Total"))
     '(hline)
     (append
      ;; Shared items rows
      (--map-indexed
       (let* ((item it)
              (idx it-index)
              (item-name (or (alist-get 'item item) ""))
              (amount (or (alist-get 'amount item) 1))
              (price (or (alist-get 'price item) 0))
              (total (ceiling (* amount price))))
         `(row
           (action-button
            :value "x"
            :action-data ,idx
            :notify (lambda (w &rest _)
                      (brb-plan--shared-remove ,state (widget-get w :action-data))))
           (label :value ,item-name)
           (bounded-int-field
            :value ,amount
            :min-value 0
            :action-data ,idx
            :notify (lambda (w &rest _)
                      (brb-plan--shared-update ,state (widget-get w :action-data)
                       'amount (widget-value w))))
           (bounded-int-field
            :value ,price
            :min-value 0
            :action-data ,idx
            :notify (lambda (w &rest _)
                      (brb-plan--shared-update ,state (widget-get w :action-data)
                       'price (widget-value w))))
           (label :value ,(brb-price-format total))))
       shared)
      ;; Footer row
      `((hline)
        (row
         (action-button
          :value "+"
          :notify (lambda (&rest _)
                    (brb-plan--shared-add ,state)))
         (label :value "")
         (label :value "")
         (label :value "")
         (label :value "")))))
    (widget-insert "\n")))

(defun brb-plan--expense-update (state index key value)
  "Update expense wine at INDEX, set KEY to VALUE in STATE."
  (let* ((data (brb-plan-state-data state))
         (expense (alist-get 'expense-wines data))
         (item (nth index expense)))
    (when item
      (setf (alist-get key item) value)
      (brb-plan--save-data state)
      ;; Only redraw finances - table handles its own update
      (brb-plan--redraw-zones state '(finances)))))

(defun brb-plan--expense-remove (state index)
  "Remove expense wine at INDEX from STATE."
  (let* ((data (brb-plan-state-data state))
         (expense (alist-get 'expense-wines data)))
    (setf (alist-get 'expense-wines data)
          (-remove-at index expense))
    (brb-plan--save-data state)
    (brb-plan--redraw-zones state '(expense finances))))

(defun brb-plan--expense-add (state)
  "Add a new expense wine item to STATE."
  (let* ((item-name (read-string "Item name: "))
         (data (brb-plan-state-data state))
         (expense (alist-get 'expense-wines data))
         (entry `((item . ,item-name) (amount . 1) (price . 0))))
    (setf (alist-get 'expense-wines data)
          (append expense (list entry)))
    (brb-plan--save-data state)
    (brb-plan--redraw-zones state '(expense finances))))

(defun brb-plan--render-expense (state)
  "Render expense wines zone for STATE."
  (let* ((data (brb-plan-state-data state))
         (expense (alist-get 'expense-wines data)))
    (widget-create 'heading-2 "Expense Wines")
    (apply
     #'widget-create
     'table
     :truncate '((1 . 24))
     '(hline)
     '(row (label :value "")
       (label :value "Item")
       (label :value "Amount")
       (label :value "Price")
       (label :value "Total"))
     '(hline)
     (append
      ;; Expense items rows
      (--map-indexed
       (let* ((item it)
              (idx it-index)
              (item-name (or (alist-get 'item item) ""))
              (amount (or (alist-get 'amount item) 1))
              (price (or (alist-get 'price item) 0))
              (total (ceiling (* amount price))))
         `(row
           (action-button
            :value "x"
            :action-data ,idx
            :notify (lambda (w &rest _)
                      (brb-plan--expense-remove ,state (widget-get w :action-data))))
           (label :value ,item-name)
           (bounded-int-field
            :value ,amount
            :min-value 0
            :action-data ,idx
            :notify (lambda (w &rest _)
                      (brb-plan--expense-update ,state (widget-get w :action-data)
                       'amount (widget-value w))))
           (bounded-int-field
            :value ,price
            :min-value 0
            :action-data ,idx
            :notify (lambda (w &rest _)
                      (brb-plan--expense-update ,state (widget-get w :action-data)
                       'price (widget-value w))))
           (label :value ,(brb-price-format total))))
       expense)
      ;; Footer row
      `((hline)
        (row
         (action-button
          :value "+"
          :notify (lambda (&rest _)
                    (brb-plan--expense-add ,state)))
         (label :value "")
         (label :value "")
         (label :value "")
         (label :value "")))))
    (widget-insert "\n")))

(defun brb-plan--participant-data (state pid)
  "Get participant data for PID from STATE."
  (--find (string-equal pid (alist-get 'id it))
          (alist-get 'participants (brb-plan-state-data state))))

(defun brb-plan--participant-set-fee (state pid fee)
  "Set FEE for participant PID in STATE."
  (let* ((data (brb-plan-state-data state))
         (participants-data (alist-get 'participants data))
         (p-data (--find (string-equal pid (alist-get 'id it)) participants-data)))
    (if p-data
        (setf (alist-get 'fee p-data) fee)
      ;; Create new entry
      (push `((id . ,pid) (fee . ,fee)) participants-data)
      (setf (alist-get 'participants data) participants-data))
    (brb-plan--save-data state)
    ;; Only redraw finances - table handles its own update
    (brb-plan--redraw-zones state '(finances))))

(defun brb-plan--participant-remove (state pid)
  "Remove participant with PID from STATE."
  (let* ((event (brb-plan-state-event state))
         (participants (->> (brb-plan-state-participants state)
                            (--remove (string-equal pid (vulpea-note-id it))))))
    ;; Update state
    (setf (brb-plan-state-participants state) participants)
    ;; Update event metadata
    (vulpea-utils-with-note event
      (vulpea-buffer-meta-set "participants" participants 'append)
      (save-buffer))
    ;; Remove from data
    (let ((data (brb-plan-state-data state)))
      (setf (alist-get 'participants data)
            (--remove (string-equal pid (alist-get 'id it))
                      (alist-get 'participants data)))
      (brb-plan--save-data state))
    (brb-plan--redraw-zones state '(participants finances))))

(defun brb-plan--participant-add (state)
  "Add a participant to STATE via selection."
  (when-let ((person (vulpea-select-from
                      "Participant"
                      (vulpea-db-query-by-tags-every '("people")))))
    (let* ((event (brb-plan-state-event state))
           (participants (append (brb-plan-state-participants state) (list person))))
      ;; Update state
      (setf (brb-plan-state-participants state) participants)
      ;; Update event metadata
      (vulpea-utils-with-note event
        (vulpea-buffer-meta-set "participants" participants 'append)
        (save-buffer))
      (brb-plan--redraw-zones state '(participants finances)))))

(defun brb-plan--render-participants (state)
  "Render participants zone for STATE."
  (let* ((participants (brb-plan-state-participants state))
         (event (brb-plan-state-event state))
         (price (or (vulpea-note-meta-get event "price" 'number) 0))
         (host (brb-plan-state-host state))
         (host-id (when host (vulpea-note-id host))))
    (widget-create 'heading-2 "Participants")
    (apply
     #'widget-create
     'table
     :truncate '((1 . 24))
     '(hline)
     '(row (label :value "")
       (label :value "Participant")
       (label :value "Mode")
       (label :value "Fee"))
     '(hline)
     (append
      ;; Participant rows
      (--map
       (let* ((person it)
              (pid (vulpea-note-id person))
              (name (vulpea-note-title person))
              (p-data (brb-plan--participant-data state pid))
              (host-p (string-equal pid host-id))
              (custom-fee (alist-get 'fee p-data))
              (fee (cond (host-p 0)
                         (custom-fee custom-fee)
                         (t price)))
              (mode (cond (host-p "host")
                          (custom-fee "custom")
                          (t "normal"))))
         `(row
           (action-button
            :value "x"
            :action-data ,pid
            :notify (lambda (w &rest _)
                      (brb-plan--participant-remove ,state (widget-get w :action-data))))
           (label :value ,name)
           (label :value ,mode)
           ,(if host-p
                `(label :value "0")
              `(bounded-int-field
                :value ,fee
                :min-value 0
                :action-data ,pid
                :notify (lambda (w &rest _)
                          (brb-plan--participant-set-fee
                           ,state (widget-get w :action-data)
                           (widget-value w)))))))
       participants)
      ;; Footer row
      `((hline)
        (row
         (action-button
          :value "+"
          :notify (lambda (&rest _)
                    (brb-plan--participant-add ,state)))
         (label :value "")
         (label :value "")
         (label :value "")))))
    (widget-insert "\n")))

(defun brb-plan--waiting-remove (state pid)
  "Remove person with PID from waiting list in STATE."
  (let* ((event (brb-plan-state-event state))
         (waiting (--remove (string-equal pid (vulpea-note-id it))
                            (brb-plan-state-waiting state))))
    (setf (brb-plan-state-waiting state) waiting)
    ;; Persist to note metadata
    (vulpea-utils-with-note event
      (vulpea-buffer-meta-set "waiting" waiting 'append)
      (save-buffer))
    (brb-plan--redraw-zone state 'waiting)))

(defun brb-plan--waiting-promote (state pid)
  "Promote person with PID from waiting to participants in STATE."
  (let* ((event (brb-plan-state-event state))
         (current-waiting (brb-plan-state-waiting state))
         (person (--find (string-equal pid (vulpea-note-id it)) current-waiting)))
    (when person
      ;; Remove from waiting
      (let ((new-waiting (--remove (string-equal pid (vulpea-note-id it)) current-waiting)))
        (setf (brb-plan-state-waiting state) new-waiting)
        ;; Add to participants
        (let ((participants (append (brb-plan-state-participants state) (list person))))
          (setf (brb-plan-state-participants state) participants)
          ;; Persist both to note metadata
          (vulpea-utils-with-note event
            (vulpea-buffer-meta-set "waiting" new-waiting 'append)
            (vulpea-buffer-meta-set "participants" participants 'append)
            (save-buffer))))
      (brb-plan--redraw-zones state '(waiting participants finances)))))

(defun brb-plan--waiting-add (state)
  "Add a person to waiting list in STATE."
  (when-let ((person (vulpea-select-from
                      "Person"
                      (vulpea-db-query-by-tags-every '("people")))))
    (let* ((event (brb-plan-state-event state))
           (waiting (append (brb-plan-state-waiting state) (list person))))
      (setf (brb-plan-state-waiting state) waiting)
      ;; Persist to note metadata
      (vulpea-utils-with-note event
        (vulpea-buffer-meta-set "waiting" waiting 'append)
        (save-buffer))
      (brb-plan--redraw-zone state 'waiting))))

(defun brb-plan--render-waiting (state)
  "Render waiting list zone for STATE."
  (let ((waiting (brb-plan-state-waiting state)))
    (widget-create 'heading-2 "Waiting List")
    (if (null waiting)
        (widget-insert "(empty)\n")
      (dolist (person waiting)
        (let ((pid (vulpea-note-id person))
              (name (vulpea-note-title person)))
          (widget-insert "  ")
          (widget-create 'label :value name)
          (widget-insert " ")
          (widget-create 'action-button
                         :value "[x]"
                         :action-data pid
                         :notify `(lambda (w &rest _)
                                    (brb-plan--waiting-remove ,state (widget-get w :action-data))))
          (widget-insert " ")
          (widget-create 'action-button
                         :value "[↑]"
                         :action-data pid
                         :notify `(lambda (w &rest _)
                                    (brb-plan--waiting-promote ,state (widget-get w :action-data))))
          (widget-insert "\n"))))
    (widget-create 'action-button
                   :value "[+] Add"
                   :notify `(lambda (&rest _)
                              (brb-plan--waiting-add ,state)))
    (widget-insert "\n")))

;;; * Scores zone renderers

(defun brb-plan--render-scores-summary (state)
  "Render scores summary zone for STATE."
  (let* ((event (brb-plan-state-event state))
         (summary (brb-event-summary event))
         (event-rms (alist-get 'rms summary))
         (event-wavg (alist-get 'wavg summary))
         (event-qpr (alist-get 'qpr summary))
         (price-harmonic (alist-get 'wines-price-harmonic summary))
         (price-median (alist-get 'wines-price-median summary)))
    (widget-create 'heading-2 "Summary")
    (widget-create
     'table
     '(hline)
     `(row (label :value "RMS")
       (label :value ,(if event-rms (format "%.4f" event-rms) "—")))
     `(row (label :value "WAVG")
       (label :value ,(if event-wavg (format "%.4f" event-wavg) "—")))
     `(row (label :value "QPR")
       (label :value ,(if event-qpr (format "%.4f" event-qpr) "—")))
     '(hline)
     `(row (label :value "Price (harmonic)")
       (label :value ,(if price-harmonic (brb-price-format price-harmonic) "—")))
     `(row (label :value "Price (median)")
       (label :value ,(if price-median (brb-price-format price-median) "—")))
     '(hline))
    (widget-insert "\n")))

(defun brb-plan--compute-places (wines-data)
  "Compute places for WINES-DATA based on WAVG with ties.
Wines with same WAVG get same place. Wines without WAVG get no place."
  (let* ((places (make-hash-table :test 'equal))
         ;; Sort by wavg descending for ranking
         (wines-sorted (->> wines-data
                            (--filter (alist-get 'wavg it))
                            (--sort (> (alist-get 'wavg it)
                                       (alist-get 'wavg other)))))
         (current-place 1)
         (prev-wavg nil))
    (--each-indexed wines-sorted
      (let* ((wine (alist-get 'wine it))
             (wavg (alist-get 'wavg it)))
        (unless (and prev-wavg (= wavg prev-wavg))
          (setq current-place (1+ it-index)))
        (puthash (vulpea-note-id wine) current-place places)
        (setq prev-wavg wavg)))
    places))

(defun brb-plan--render-scores-wines (state)
  "Render wines scores zone for STATE."
  (let* ((event (brb-plan-state-event state))
         (summary (brb-event-summary event))
         (wines-data (alist-get 'wines summary))
         ;; Keep original order, filter out ignored wines
         (wines-display (--remove (alist-get 'ignore-scores it) wines-data))
         (places (brb-plan--compute-places wines-display)))
    (widget-create 'heading-2 "Wines")
    (apply
     #'widget-create
     'table
     :truncate '((2 . 16) (3 . 20))
     :padding-type '((0 . left) (5 . left) (6 . left) (7 . left) (8 . left) (9 . left))
     '(hline)
     '(row (label :value "#")
       (label :value "##")
       (label :value "Producer")
       (label :value "Wine")
       (label :value "Year")
       (label :value "WAVG")
       (label :value "Sdev")
       (label :value "QPR")
       (label :value "Fav")
       (label :value "Out"))
     '(hline)
     (append
      (--map-indexed
       (let* ((wine-data it)
              (order (1+ it-index))
              (wine (alist-get 'wine wine-data))
              (wine-id (vulpea-note-id wine))
              (place (gethash wine-id places))
              (producer (or (vulpea-note-title
                             (vulpea-note-meta-get wine "producer" 'note))
                            ""))
              (name (or (vulpea-note-meta-get wine "name") ""))
              (vintage (or (vulpea-note-meta-get wine "vintage" 'number) "NV"))
              (wavg (alist-get 'wavg wine-data))
              (sdev (alist-get 'sdev wine-data))
              (qpr (alist-get 'qpr wine-data))
              (fav (alist-get 'fav wine-data))
              (out (alist-get 'out wine-data)))
         `(row
           (label :value ,(number-to-string order))
           (label :value ,(if place (number-to-string place) ""))
           (label :value ,producer)
           (link-button :value ,name :target ,wine)
           (label :value ,(if (numberp vintage) (number-to-string vintage) vintage))
           (label :value ,(if wavg (format "%.2f" wavg) "—"))
           (label :value ,(if sdev (format "%.2f" sdev) "—"))
           (label :value ,(if qpr (format "%.2f" qpr) "—"))
           (label :value ,(number-to-string (or fav 0)))
           (label :value ,(number-to-string (or out 0)))))
       wines-display)
      '((hline))))
    (widget-insert "\n")))

(defun brb-plan--score-set (state wine-id participant-id score)
  "Set SCORE for WINE-ID and PARTICIPANT-ID in STATE."
  (let* ((data (brb-plan-state-data state))
         (wines-data (alist-get 'wines data))
         (wine-data (--find (string-equal wine-id (alist-get 'id it)) wines-data)))
    (if wine-data
        (let ((scores (alist-get 'scores wine-data))
              (score-data (--find (string-equal participant-id (alist-get 'participant it))
                                  (alist-get 'scores wine-data))))
          (if score-data
              (setf (alist-get 'score score-data) score)
            ;; Create new score entry
            (push `((participant . ,participant-id)
                    (score . ,score)
                    (sentiment . nil))
                  scores)
            (setf (alist-get 'scores wine-data) scores)))
      ;; Create wine entry with score
      (push `((id . ,wine-id)
              (scores . (((participant . ,participant-id)
                          (score . ,score)
                          (sentiment . nil)))))
            wines-data)
      (setf (alist-get 'wines data) wines-data))
    (brb-plan--save-data state)
    ;; Only redraw summary zones - table handles its own update
    (brb-plan--redraw-zones state '(scores-summary scores-wines))))

(defun brb-plan--sentiment-set (state wine-id participant-id sentiment)
  "Set SENTIMENT for WINE-ID and PARTICIPANT-ID in STATE."
  (let* ((data (brb-plan-state-data state))
         (wines-data (alist-get 'wines data))
         (wine-data (--find (string-equal wine-id (alist-get 'id it)) wines-data)))
    (if wine-data
        (let ((scores (alist-get 'scores wine-data))
              (score-data (--find (string-equal participant-id (alist-get 'participant it))
                                  (alist-get 'scores wine-data))))
          (if score-data
              (setf (alist-get 'sentiment score-data) sentiment)
            ;; Create new score entry with sentiment
            (push `((participant . ,participant-id)
                    (score . nil)
                    (sentiment . ,sentiment))
                  scores)
            (setf (alist-get 'scores wine-data) scores)))
      ;; Create wine entry with sentiment
      (push `((id . ,wine-id)
              (scores . (((participant . ,participant-id)
                          (score . nil)
                          (sentiment . ,sentiment)))))
            wines-data)
      (setf (alist-get 'wines data) wines-data))
    (brb-plan--save-data state)
    ;; Only redraw summary zones - table handles its own update
    (brb-plan--redraw-zones state '(scores-summary scores-wines))))

(defun brb-plan--sentiment-cycle (state wine-id participant-id current)
  "Cycle sentiment for WINE-ID and PARTICIPANT-ID in STATE.
CURRENT is the current sentiment value."
  (let ((next (pcase current
                ("favourite" "outcast")
                ("outcast" nil)
                (_ "favourite"))))
    (brb-plan--sentiment-set state wine-id participant-id next)))

(defun brb-plan--score-edit (state wine-id participant-id current-score current-sentiment)
  "Edit score for WINE-ID and PARTICIPANT-ID in STATE.
CURRENT-SCORE and CURRENT-SENTIMENT are the current values.
Prompts for new score, or f=fav, o=outcast, c=clear sentiment."
  (let* ((prompt (format "Score (0-5, f=♥, o=✗, c=clear, current: %s%s): "
                         (if current-score (format "%.1f" current-score) "—")
                         (pcase current-sentiment
                           ("favourite" "♥")
                           ("outcast" "✗")
                           (_ ""))))
         (input (read-string prompt))
         ;; Save line and column for stable restoration
         (line (line-number-at-pos))
         (col (current-column)))
    (cond
     ((string-equal "f" input)
      (brb-plan--sentiment-set state wine-id participant-id "favourite")
      (brb-plan--redraw-zones state '(scores-personal))
      (goto-char (point-min))
      (forward-line (1- line))
      (move-to-column col))
     ((string-equal "o" input)
      (brb-plan--sentiment-set state wine-id participant-id "outcast")
      (brb-plan--redraw-zones state '(scores-personal))
      (goto-char (point-min))
      (forward-line (1- line))
      (move-to-column col))
     ((string-equal "c" input)
      (brb-plan--sentiment-set state wine-id participant-id nil)
      (brb-plan--redraw-zones state '(scores-personal))
      (goto-char (point-min))
      (forward-line (1- line))
      (move-to-column col))
     ((string-empty-p input)
      ;; Do nothing on empty input
      nil)
     (t
      (let ((score (string-to-number input)))
        (when (and (>= score 0) (<= score 5))
          (brb-plan--score-set state wine-id participant-id score)
          (brb-plan--redraw-zones state '(scores-personal))
          (goto-char (point-min))
          (forward-line (1- line))
          (move-to-column col)))))))

(defun brb-plan--render-scores-personal (state)
  "Render personal scores zone for STATE."
  (let* ((data (brb-plan-state-data state))
         (participants (brb-plan-state-participants state))
         (wines (brb-plan-state-wines state))
         (wines-data (alist-get 'wines data)))
    (widget-create 'heading-2 "Personal Scores")
    (when (and participants wines)
      (apply
       #'widget-create
       'table
       :truncate (cons '(0 . 16)
                       (--map-indexed `(,(1+ it-index) . 6) wines))
       '(hline)
       ;; Header row with wine numbers
       `(row (label :value "Participant")
         ,@(--map-indexed
            `(label :value ,(format "#%d" (1+ it-index)))
            wines))
       '(hline)
       (append
        ;; One row per participant
        (--map
         (let* ((person it)
                (pid (vulpea-note-id person))
                (name (vulpea-note-title person)))
           `(row
             (link-button :value ,name :target ,person)
             ,@(--map
                (let* ((wine it)
                       (wid (vulpea-note-id wine))
                       (wine-data (--find (string-equal wid (alist-get 'id it)) wines-data))
                       (score-data (when wine-data
                                    (--find (string-equal pid (alist-get 'participant it))
                                     (alist-get 'scores wine-data))))
                       (score (when score-data (alist-get 'score score-data)))
                       (sentiment (when score-data (alist-get 'sentiment score-data)))
                       (sentiment-char (pcase sentiment
                                        ("favourite" "♥")
                                        ("outcast" "✗")
                                        (_ ""))))
                 ;; Combined score + sentiment display
                 `(label
                   :value ,(concat (if score (format "%.1f" score) "—")
                            sentiment-char)
                   :action-data (,wid ,pid ,score ,sentiment)
                   :format "%[%v%]"
                   :notify (lambda (w &rest _)
                             (let ((data (widget-get w :action-data)))
                              (brb-plan--score-edit
                               ,state
                               (nth 0 data) (nth 1 data)
                               (nth 2 data) (nth 3 data))))))
                wines)))
         participants)
        '((hline)))))
    (widget-insert "\n")))

;;; * Order tab

(defun brb-plan--render-orders-summary (state)
  "Render orders summary zone for STATE.
Shows all order items with total quantities across participants."
  (let* ((data (brb-plan-state-data state))
         (personal (alist-get 'personal data))
         (participants (brb-plan-state-participants state)))
    (widget-create 'heading-2 "All Orders")
    (if (null personal)
        (widget-insert "No orders yet.\n")
      (let* ((grand-total-qty
              (-sum (--map
                     (-sum (--map (alist-get 'amount it) (alist-get 'orders it)))
                     personal)))
             (grand-total-price
              (-sum (--map
                     (let ((price (alist-get 'price it))
                           (qty (-sum (--map (alist-get 'amount it) (alist-get 'orders it)))))
                       (ceiling (* price qty)))
                     personal))))
        (apply
         #'widget-create
         'table
         :truncate '((1 . 24))
         :padding-type '((2 . left) (3 . left) (4 . left))
         '(hline)
         '(row (label :value "")
           (label :value "Item")
           (label :value "Price")
           (label :value "Qty")
           (label :value "Total")
           (label :value "Participants"))
         '(hline)
         (append
          (--map-indexed
           (let* ((item it)
                  (idx it-index)
                  (item-name (or (alist-get 'item item) ""))
                  (price (or (alist-get 'price item) 0))
                  (orders (alist-get 'orders item))
                  (total-qty (-sum (--map (alist-get 'amount it) orders)))
                  (total-price (ceiling (* price total-qty)))
                  (participant-names
                   (--map
                    (let ((pid (alist-get 'participant it)))
                      (vulpea-note-title
                       (--find (string= (vulpea-note-id it) pid) participants)))
                    orders)))
             `(row
               (action-button
                :value "x"
                :action-data ,idx
                :notify (lambda (w &rest _)
                          (brb-plan--order-remove ,state (widget-get w :action-data))))
               (label :value ,item-name)
               (label :value ,(brb-price-format price))
               (label :value ,(number-to-string total-qty))
               (label :value ,(brb-price-format total-price))
               (label :value ,(string-join participant-names ", ") :truncate 30)))
           personal)
          `((hline)
            (row
             (label :value "")
             (label :value "")
             (label :value "")
             (label :value ,(number-to-string grand-total-qty))
             (label :value ,(brb-price-format grand-total-price))
             (label :value "")))))))
    (widget-insert "\n")))

(defun brb-plan--render-orders-personal (state)
  "Render per-participant order zones for STATE."
  (let* ((data (brb-plan-state-data state))
         (personal (alist-get 'personal data))
         (participants (brb-plan-state-participants state)))
    (widget-create 'heading-2 "Orders by Participant")
    (--each participants
      (let* ((participant it)
             (pid (vulpea-note-id participant))
             (name (vulpea-note-title participant))
             ;; Build participant's order list from personal items
             (participant-orders
              (->> personal
                   (--map
                    (let* ((item-orders (alist-get 'orders it))
                           (order-entry (--find (string= pid (alist-get 'participant it))
                                                item-orders))
                           (amount (when order-entry (alist-get 'amount order-entry))))
                      (when (and amount (> amount 0))
                        `((item . ,(alist-get 'item it))
                          (price . ,(alist-get 'price it))
                          (amount . ,amount)
                          (total . ,(ceiling (* amount (alist-get 'price it))))))))
                   (-non-nil)))
             (participant-total
              (-sum (--map (alist-get 'total it) participant-orders))))
        (widget-insert "\n")
        (widget-create 'heading-3 name)
        (if (null participant-orders)
            (progn
              (widget-insert "No orders. ")
              (widget-create
               'action-button
               :value "[+ Add]"
               :notify `(lambda (&rest _)
                          (brb-plan--order-add-for-participant
                           ,state ,pid)))
              (widget-insert "\n"))
          (apply
           #'widget-create
           'table
           :truncate '((1 . 20))
           :padding-type '((2 . left) (3 . left) (4 . left))
           '(row (label :value "")
             (label :value "Item")
             (label :value "Price")
             (label :value "Qty")
             (label :value "Total"))
           '(hline)
           (append
            (--map
             (let* ((order it)
                    (item-name (alist-get 'item order))
                    (price (alist-get 'price order))
                    (amount (alist-get 'amount order))
                    (total (alist-get 'total order)))
               `(row
                 (action-button
                  :value "x"
                  :action-data (,item-name . ,pid)
                  :notify (lambda (w &rest _)
                            (let ((data (widget-get w :action-data)))
                             (brb-plan--order-remove-participant
                              ,state (car data) (cdr data)))))
                 (label :value ,item-name)
                 (label :value ,(brb-price-format price))
                 (bounded-int-field
                  :value ,amount
                  :min-value 1
                  :max-value 99
                  :action-data (,item-name . ,pid)
                  :notify (lambda (w &rest _)
                            (let ((data (widget-get w :action-data)))
                             (brb-plan--order-update-qty
                              ,state (car data) (cdr data) (widget-value w)))))
                 (label :value ,(brb-price-format total))))
             participant-orders)
            `((hline)
              (row
               (action-button
                :value "+"
                :action-data ,pid
                :notify (lambda (w &rest _)
                          (brb-plan--order-add-for-participant
                           ,state (widget-get w :action-data))))
               (label :value "")
               (label :value "")
               (label :value "Total:")
               (label :value ,(brb-price-format participant-total)))))))))))

(defun brb-plan--order-remove (state index)
  "Remove item at INDEX from personal orders in STATE."
  (let* ((data (brb-plan-state-data state))
         (personal (alist-get 'personal data)))
    (setf (alist-get 'personal data)
          (-remove-at index personal))
    (brb-plan--save-data state)
    (brb-plan--redraw-zones state '(orders-summary orders-personal finances))))

(defun brb-plan--order-remove-participant (state item-name participant-id)
  "Remove PARTICIPANT-ID from item ITEM-NAME in STATE."
  (let* ((data (brb-plan-state-data state))
         (personal (alist-get 'personal data))
         (item (--find (string= item-name (alist-get 'item it)) personal)))
    (when item
      (setf (alist-get 'orders item)
            (--remove (string= participant-id (alist-get 'participant it))
                      (alist-get 'orders item)))
      ;; If no participants left, remove the entire item
      (when (null (alist-get 'orders item))
        (setf (alist-get 'personal data)
              (--remove (string= item-name (alist-get 'item it)) personal))))
    (brb-plan--save-data state)
    (brb-plan--redraw-zones state '(orders-summary orders-personal finances))))

(defun brb-plan--order-update-qty (state item-name participant-id amount)
  "Update AMOUNT for PARTICIPANT-ID in item ITEM-NAME in STATE."
  (let* ((data (brb-plan-state-data state))
         (personal (alist-get 'personal data))
         (item (--find (string= item-name (alist-get 'item it)) personal))
         (order-entry (when item
                        (--find (string= participant-id (alist-get 'participant it))
                                (alist-get 'orders item)))))
    (when order-entry
      (setf (alist-get 'amount order-entry) amount)
      (brb-plan--save-data state)
      ;; Save cursor position by line/column for stable restoration
      (let ((line (line-number-at-pos))
            (col (current-column)))
        (brb-plan--redraw-zones state '(orders-summary orders-personal finances))
        (goto-char (point-min))
        (forward-line (1- line))
        (move-to-column col)))))

(defun brb-plan--order-add-for-participant (state participant-id)
  "Add order item for PARTICIPANT-ID in STATE."
  (let* ((data (brb-plan-state-data state))
         (personal (or (alist-get 'personal data) '()))
         (item-name (completing-read "Item: " (--map (alist-get 'item it) personal)))
         (existing (--find (string= item-name (alist-get 'item it)) personal))
         (price (or (when existing (alist-get 'price existing))
                    (read-number "Price: " 0)))
         (amount (read-number "Amount: " 1))
         (new-order `((participant . ,participant-id)
                      (amount . ,amount))))
    (if existing
        ;; Add/update participant in existing item
        (let* ((orders (alist-get 'orders existing))
               (order-entry (--find (string= participant-id (alist-get 'participant it))
                                    orders)))
          (if order-entry
              ;; Update existing amount
              (setf (alist-get 'amount order-entry) amount)
            ;; Add new participant entry
            (setf (alist-get 'orders existing)
                  (append orders (list new-order)))))
      ;; Create new item
      (let ((new-item `((item . ,item-name)
                        (price . ,price)
                        (orders . (,new-order)))))
        (setf (alist-get 'personal data)
              (append personal (list new-item)))))
    (brb-plan--save-data state)
    (brb-plan--redraw-zones state '(orders-summary orders-personal finances))))

;;; * Extra tab

(defun brb-plan--extra-glass-price (wine-data)
  "Calculate glass price for extra wine from WINE-DATA."
  (let ((asking (or (alist-get 'price-asking wine-data)
                    (alist-get 'price-public wine-data)
                    0))
        (participants (alist-get 'participants wine-data)))
    (if (and participants (> (length participants) 0))
        (ceiling (/ (float asking) (length participants)))
      0)))

(defun brb-plan--extra-set-asking-price (state wine-id)
  "Set asking price for extra wine WINE-ID in STATE."
  (let* ((price (read-number "Asking price: "))
         (data (brb-plan-state-data state))
         (wines-data (alist-get 'wines data))
         (wine-data (--find (string= wine-id (alist-get 'id it)) wines-data)))
    (when wine-data
      (setf (alist-get 'price-asking wine-data) price)
      (brb-plan--save-data state)
      (brb-plan--redraw-zones state '(extra-wines finances)))))

(defun brb-plan--extra-toggle-participant (state wine-id participant-id)
  "Toggle PARTICIPANT-ID for extra wine WINE-ID in STATE."
  (let* ((data (brb-plan-state-data state))
         (wines-data (alist-get 'wines data))
         (wine-data (--find (string= wine-id (alist-get 'id it)) wines-data)))
    (when wine-data
      (let ((participants (alist-get 'participants wine-data)))
        (if (member participant-id participants)
            (setf (alist-get 'participants wine-data)
                  (--remove (string= it participant-id) participants))
          (setf (alist-get 'participants wine-data)
                (cons participant-id participants))))
      (brb-plan--save-data state)
      (brb-plan--redraw-zones state '(extra-wines finances)))))

(defun brb-plan--render-extra-wines (state)
  "Render extra wines zone for STATE."
  (let* ((data (brb-plan-state-data state))
         (wines-data (alist-get 'wines data))
         (wines (brb-plan-state-wines state))
         (participants (brb-plan-state-participants state))
         ;; Filter to only extra type wines
         (extra-wines
          (--filter
           (let* ((wid (vulpea-note-id it))
                  (wd (--find (string= wid (alist-get 'id it)) wines-data)))
             (string= "extra" (alist-get 'type wd)))
           wines)))
    (widget-create 'heading-2 "Extra Wines")
    (if (null extra-wines)
        (widget-insert "No extra wines. Mark a wine as type \"extra\" in the Plan tab.\n")
      (--each extra-wines
        (let* ((wine it)
               (wine-id (vulpea-note-id wine))
               (wine-data (--find (string= wine-id (alist-get 'id it)) wines-data))
               (price-public (or (alist-get 'price-public wine-data) 0))
               (price-real (or (alist-get 'price-real wine-data) 0))
               (price-asking (or (alist-get 'price-asking wine-data) price-public))
               (wine-participants (alist-get 'participants wine-data))
               (participant-count (length wine-participants))
               (glass-price (brb-plan--extra-glass-price wine-data)))
          (widget-insert "\n")
          (widget-create 'heading-3 (vulpea-note-title wine))
          (widget-create
           'table
           :padding-type '((1 . left))
           `(row (label :value "Price public:")
             (label :value ,(brb-price-format price-public)))
           `(row (label :value "Price real:")
             (label :value ,(brb-price-format price-real)))
           `(row (label :value "Asking price:")
             (action-button
              :value ,(brb-price-format price-asking)
              :action-data ,wine-id
              :notify (lambda (w &rest _)
                        (brb-plan--extra-set-asking-price
                         ,state (widget-get w :action-data)))))
           `(row (label :value "Participants:")
             (label :value ,(number-to-string participant-count)))
           `(row (label :value "Glass price:")
             (label :value ,(brb-price-format glass-price))))
          (widget-insert "\n")
          ;; Participant toggles
          (--each participants
            (let* ((person it)
                   (pid (vulpea-note-id person))
                   (name (vulpea-note-title person))
                   (included (member pid wine-participants)))
              (widget-insert "  ")
              (widget-create
               'action-button
               :value (if included "[+]" "[-]")
               :action-data (cons wine-id pid)
               :notify `(lambda (w &rest _)
                          (let ((data (widget-get w :action-data)))
                           (brb-plan--extra-toggle-participant
                            ,state (car data) (cdr data)))))
              (widget-insert " ")
              (widget-create 'label :value name)
              (widget-insert "\n")))
          (widget-insert "\n"))))
    (widget-insert "\n")))

;;; * Invoices tab

(defun brb-plan--refresh-balances (state)
  "Refresh participant balances from ledger for STATE."
  (let ((balances (brb-plan-state-balances state)))
    (clrhash balances)
    (--each (brb-plan-state-participants state)
      (let* ((pid (vulpea-note-id it))
             (balance (brb-ledger-balance-of it)))
        (puthash pid balance balances)))
    (brb-plan--redraw-zones state '(invoices-personal finances))))

(defun brb-plan--charge-all (state)
  "Record charges for all participants to ledger for STATE."
  (let* ((event (brb-plan-state-event state))
         (data (brb-plan-state-data state))
         (participants (brb-plan-state-participants state))
         (wines (brb-plan-state-wines state))
         (host (brb-plan-state-host state))
         (balances (brb-plan-state-balances state))
         (date (vulpea-utils-with-note event
                 (vulpea-buffer-prop-get "date"))))
    (--each participants
      (unless (and host (string= (vulpea-note-id it) (vulpea-note-id host)))
        (let ((st (brb-event-statement-for
                   event it
                   :data data
                   :host host
                   :wines wines
                   :balances balances)))
          (brb-ledger-charge
           :convive it
           :code (concat (vulpea-note-id event) ":" (vulpea-note-id it))
           :amount (alist-get 'total st)
           :date (date-to-time date)
           :comment (vulpea-note-title event)))))
    (message "Charged all participants.")))

(defun brb-plan--record-spendings (state)
  "Record event spendings to ledger for STATE."
  (let* ((event (brb-plan-state-event state))
         (data (brb-plan-state-data state))
         (participants (brb-plan-state-participants state))
         (wines (brb-plan-state-wines state))
         (host (brb-plan-state-host state))
         (balances (brb-plan-state-balances state))
         (statement (brb-event-statement event
                                         :data data
                                         :participants participants
                                         :wines wines
                                         :host host
                                         :balances balances))
         (date (vulpea-utils-with-note event
                 (vulpea-buffer-prop-get "date"))))
    (brb-ledger-record-txn
     :amount (alist-get 'spending-wines-real statement)
     :date (date-to-time date)
     :comment (format "%s: wines" (vulpea-note-title event))
     :code (concat (vulpea-note-id event) ":wines")
     :account-to "personal:account"
     :account-from "balance:assets")
    (brb-ledger-record-txn
     :amount (alist-get 'spending-extra-real statement)
     :date (date-to-time date)
     :comment (format "%s: wines extra" (vulpea-note-title event))
     :code (concat (vulpea-note-id event) ":wines-extra")
     :account-to "personal:account"
     :account-from "balance:assets")
    (brb-ledger-spend
     :amount (alist-get 'spending-shared statement)
     :date (date-to-time date)
     :comment (format "%s: shared" (vulpea-note-title event))
     :code (concat (vulpea-note-id event) ":shared"))
    (when (> (alist-get 'spending-expense-wines statement) 0)
      (brb-ledger-record-txn
       :amount (alist-get 'spending-expense-wines statement)
       :date (date-to-time date)
       :comment (format "%s: expense wines" (vulpea-note-title event))
       :code (concat (vulpea-note-id event) ":expense-wines")
       :account-to "personal:account"
       :account-from "balance:assets"))
    (brb-ledger-spend
     :amount (alist-get 'spending-order statement)
     :date (date-to-time date)
     :comment (format "%s: delivery" (vulpea-note-title event))
     :code (concat (vulpea-note-id event) ":delivery"))
    (message "Recorded spendings.")))

(defun brb-plan--render-invoices-actions (state)
  "Render invoices actions zone for STATE."
  (widget-create 'heading-2 "Actions")
  (widget-create 'action-button
                 :value "[Refresh Balances]"
                 :notify `(lambda (&rest _)
                            (brb-plan--refresh-balances ,state)))
  (widget-insert " ")
  (widget-create 'action-button
                 :value "[Charge All]"
                 :notify `(lambda (&rest _)
                            (brb-plan--charge-all ,state)))
  (widget-insert " ")
  (widget-create 'action-button
                 :value "[Record Spendings]"
                 :notify `(lambda (&rest _)
                            (brb-plan--record-spendings ,state)))
  (widget-insert "\n\n"))

(defun brb-plan--render-invoices-settings (state)
  "Render invoices settings zone for STATE."
  (let* ((event (brb-plan-state-event state))
         (use-balance (or (vulpea-note-meta-get event "use balance") "true"))
         (pay-url (vulpea-note-meta-get event "pay url" 'link)))
    (widget-create 'heading-2 "Settings")
    (widget-create
     'table
     :padding-type '((1 . left))
     `(row (label :value "Use balance:")
       (action-button
        :value ,(format "[%s]" (if (string= use-balance "true") "X" " "))
        :notify (lambda (&rest _)
                  (let ((new-val (if (string= ,use-balance "true") "false" "true")))
                   (vulpea-utils-with-note (brb-plan-state-event ,state)
                    (vulpea-buffer-meta-set "use balance" new-val 'append)
                    (save-buffer))
                   (brb-plan--reload ,state)))))
     `(row (label :value "Pay URL:")
       (action-button
        :value ,(format "[%s]" (or pay-url "not set"))
        :notify (lambda (&rest _)
                  (let ((url (read-string "Pay URL: ")))
                   (vulpea-utils-with-note (brb-plan-state-event ,state)
                    (vulpea-buffer-meta-set "pay url" url 'append)
                    (save-buffer))
                   (brb-plan--reload ,state))))))
    (widget-insert "\n")))

(defun brb-plan--render-invoices-personal (state)
  "Render per-participant invoice zones for STATE."
  (let* ((event (brb-plan-state-event state))
         (data (brb-plan-state-data state))
         (participants (brb-plan-state-participants state))
         (wines (brb-plan-state-wines state))
         (host (brb-plan-state-host state))
         (balances (brb-plan-state-balances state))
         (use-balance (string= "true" (or (vulpea-note-meta-get event "use balance") "true"))))
    (widget-create 'heading-2 "Invoices")
    (--each participants
      (let* ((participant it)
             (pid (vulpea-note-id participant))
             (name (vulpea-note-title participant))
             (balance (gethash pid balances 0))
             (st (brb-event-statement-for
                  event participant
                  :data data
                  :host host
                  :wines wines
                  :balances balances))
             (fee (alist-get 'fee st))
             (order (alist-get 'order st))
             (extra (alist-get 'extra st))
             (total (alist-get 'total st))
             (balance-final (alist-get 'balance-final st))
             (due (alist-get 'due st)))
        (widget-insert "\n")
        (widget-create 'heading-3 name)
        ;; Invoice details
        (apply
         #'widget-create
         'table
         :padding-type '((1 . left))
         (append
          (when (and use-balance (not (= balance 0)))
            `((row (label :value "Starting balance:")
               (label :value ,(brb-price-format balance)))))
          `((row (label :value "Event fee:")
             (label :value ,(brb-price-format fee))))
          (--map
           `(row (label :value ,(format "Order: %s" (alist-get 'item it)))
             (label :value ,(brb-price-format (alist-get 'total it))))
           order)
          (--map
           `(row (label :value ,(format "Extra: %s" (vulpea-note-title (alist-get 'wine it))))
             (label :value ,(brb-price-format (alist-get 'total it))))
           extra)
          '((hline))
          `((row (label :value "Total:")
             (label :value ,(brb-price-format total))))
          (when (and use-balance (not (= balance 0)))
            `((row (label :value "Final balance:")
               (label :value ,(brb-price-format balance-final)))
              (row (label :value "Due:")
               (label :value ,(brb-price-format due)
                :face ,(if (> due 0) 'error 'success)))))))
        (widget-insert "\n")))))

;;; * Tab rendering

(defun brb-plan--render-tab (state)
  "Render current tab content for STATE."
  (pcase (brb-plan-state-tab state)
    ("plan" (brb-plan--render-tab-plan state))
    ("scores" (brb-plan--render-tab-scores state))
    ("order" (brb-plan--render-tab-order state))
    ("extra" (brb-plan--render-tab-extra state))
    ("invoices" (brb-plan--render-tab-invoices state))
    (tab (widget-insert (format "Unknown tab: %s\n" tab)))))

(defun brb-plan--render-tab-plan (state)
  "Render plan tab for STATE."
  (brb-plan--render-zone state 'general)
  (brb-plan--render-zone state 'forecast)
  (brb-plan--render-zone state 'finances)
  (brb-plan--render-zone state 'wines)
  (brb-plan--render-zone state 'shared)
  (brb-plan--render-zone state 'expense)
  (brb-plan--render-zone state 'participants)
  (brb-plan--render-zone state 'waiting))

(defun brb-plan--render-tab-scores (state)
  "Render scores tab for STATE."
  (brb-plan--render-zone state 'scores-summary)
  (brb-plan--render-zone state 'scores-wines)
  (brb-plan--render-zone state 'scores-personal))

(defun brb-plan--render-tab-order (state)
  "Render order tab for STATE."
  (brb-plan--render-zone state 'orders-summary)
  (brb-plan--render-zone state 'orders-personal))

(defun brb-plan--render-tab-extra (state)
  "Render extra tab for STATE."
  (brb-plan--render-zone state 'extra-wines))

(defun brb-plan--render-tab-invoices (state)
  "Render invoices tab for STATE."
  (brb-plan--render-zone state 'invoices-actions)
  (brb-plan--render-zone state 'invoices-settings)
  (brb-plan--render-zone state 'invoices-personal))

;;; * Display and navigation

(defun brb-plan--display (state)
  "Display UI for STATE."
  (let ((buffer (get-buffer-create
                 (format "*Event: %s*"
                         (vulpea-note-title (brb-plan-state-event state))))))
    (setf (brb-plan-state-buffer state) buffer)
    (switch-to-buffer buffer)
    (kill-all-local-variables)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)
    (setq-local brb-plan--state state)

    ;; Render header (not in a zone since it's always present)
    (brb-plan--render-zone state 'header)

    ;; Render tab content
    (brb-plan--render-tab state)

    (use-local-map widget-keymap)
    (widget-setup)
    (goto-char (point-min))))

(defun brb-plan--reload (state)
  "Reload event data and refresh display for STATE."
  (let ((event (vulpea-db-get-by-id
                (vulpea-note-id (brb-plan-state-event state)))))
    (setf (brb-plan-state-event state) event)
    (setf (brb-plan-state-data state) (brb-event-data-read event))
    (setf (brb-plan-state-host state) (vulpea-note-meta-get event "host" 'note))
    (setf (brb-plan-state-participants state) (brb-event-participants event))
    (setf (brb-plan-state-wines state) (brb-event-wines event))
    (brb-plan--display state)))

;;; * Entry point

;;;###autoload
(defun brb-event-plan (&optional event)
  "Open planning UI for EVENT.

If EVENT is nil, interactively select one."
  (interactive)
  (let* ((event (or event (brb-event-select)))
         (state (brb-plan-state-create event)))
    (brb-plan--display state)))

;;; * Custom widgets

(define-widget 'note-field 'field
  "A field for selecting vulpea notes."
  :format-value (lambda (_widget value)
                  (if value (vulpea-note-title value) "__"))
  :match (lambda (_widget value)
           (or (null value) (vulpea-note-p value)))
  :match-error-message (lambda (_widget _value)
                         "Value must be a vulpea note")
  :prompt-value (lambda (_widget prompt _value _unbound)
                  (vulpea-select prompt)))

(define-widget 'person-field 'note-field
  "A field for selecting person notes."
  :match (lambda (_widget value)
           (or (null value)
               (and (vulpea-note-p value)
                    (seq-contains-p (vulpea-note-tags value) "people"))))
  :match-error-message (lambda (_widget _value)
                         "Value must be a person note")
  :prompt-value (lambda (_widget prompt _value _unbound)
                  (vulpea-select-from
                   prompt
                   (vulpea-db-query-by-tags-every '("people")))))

(define-widget 'wine-field 'note-field
  "A field for selecting wine notes."
  :match (lambda (_widget value)
           (or (null value)
               (and (vulpea-note-p value)
                    (seq-contains-p (vulpea-note-tags value) "wine"))))
  :match-error-message (lambda (_widget _value)
                         "Value must be a wine note")
  :prompt-value (lambda (_widget prompt _value _unbound)
                  (vulpea-select-from
                   prompt
                   (vulpea-db-query-by-tags-every '("wine")))))

(provide 'brb-event-plan)
;;; brb-event-plan.el ends here
