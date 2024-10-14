;;; brb.el --- Barberry Garden utilities and helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Boris Buliga

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
;; In addition to its focus on wines, events are a core component of Barberry
;; Garden. This module provides various functions to query, create, and manage
;; events. Most importantly, it offers the ability to retrieve detailed event
;; information, including statistics and reports.
;;

;;; Code:

(require 'brb)
(require 'vulpea)

;;; * Configurations

(defconst brb-event-narrator-id "bc8aa837-3348-45e6-8468-85510966527a"
  "ID of Barberry Garden narrator.

This must point to a valid `vulpea' note tagged as _people_.")

(defconst brb-event-tags '("wine" "event")
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
  "Return date of EVENT as string using %F format."
  (vulpea-utils-with-note event
    (when-let ((str (vulpea-buffer-prop-get "date")))
      (org-read-date nil nil str))))

;;; * Event creation

;;;###autoload
(defun brb-event-create ()
  "Create a new event.

This is very opinionated and serves only purposes of Barberry Garden
site. Feel free to redefine this function if needed.

But let's be honest. Who in the world is going to use this package?"
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

;; * Participants

(defun brb-event-participants (event)
  "Return list of participants from EVENT."
  (vulpea-note-meta-get-list event "participants" 'note))

(provide 'brb-event)
;;; brb-event.el ends here
