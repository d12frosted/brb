;;; brb-sn.el --- Utilities for posting reviews on social networks -*- lexical-binding: t; -*-
;;
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

;;; Code:

(require 's)
(require 'vulpea)
(require 'vino)
(require 'brb)
(require 'brb-widget)
(require 'brb-event)
(require 'widget-extra)

(defvar brb-sn-buffer-name "*Reviews for Social Networks*"
  "Name of SN buffer.")

(defvar brb-sn-config '((name . "vivino")
                        (page-limit . 500)))

(defun brb-sn-display ()
  "Display UI for posting reviews on social network."
  (interactive)
  (let* ((network (alist-get 'name brb-sn-config))
         (ratings (->> (vulpea-db-query-by-tags-every '("wine" "rating"))
                       (--filter (or (null (vulpea-note-meta-get it network))
                                     (string-equal "false" (vulpea-note-meta-get it network))))))
         (gratings (->> ratings
                        (--group-by (vulpea-note-meta-get it "event" 'link))
                        (--map (if (car it)
                                   (-replace-at 0 (vulpea-db-get-by-id (car it)) it)
                                 it))
                        ;; (--map (if (car it) (brb-event-date-string (car it)) "9999-99-99"))
                        (--sort
                         (string< (if (car it) (brb-event-date-string (car it)) "9999-99-99")
                                  (if (car other) (brb-event-date-string (car other)) "9999-99-99"))))))
    (widget-buffer-setup brb-sn-buffer-name
      (widget-create 'title "Reviews")
      (widget-insert (format "You have *%d* pending reviews.\n" (length ratings)))
      (widget-insert "\n")

      ;; display each group
      (--each gratings
        (brb-sn--render-group (car it) (cdr it))))))

(defun brb-sn--render-group (event ratings)
  "Render group of RATINGS by EVENT."
  (let* ((heading (if event (concat "Ratings from " (vulpea-note-title event)) "Orphan ratings")))
    (widget-create 'heading-1 heading)
    (when event
      (widget-insert "Date: " (brb-event-date-string event) "\n"))
    (widget-insert (format "Pending ratings: %d\n" (length ratings)))
    (widget-insert "\n")
    (--each (--sort
             (let ((date1 (vulpea-note-meta-get it "date"))
                   (date2 (vulpea-note-meta-get other "date")))
               (or (string< date1 date2)
                   (< (or (vulpea-note-meta-get it "order" 'number) 0)
                      (or (vulpea-note-meta-get other "order" 'number) 0))))
             ratings)
      (brb-sn--render-rating event it))))

(defun brb-sn--render-rating (event rating)
  "Render RATING from EVENT."
  (let* ((wine (vulpea-note-meta-get rating "wine" 'note))
         (score (format "☆ %.2f" (vulpea-note-meta-get rating "total" 'number)))
         (header (brb-sn-rating-header event rating wine))
         (content (brb-sn-rating-content event rating wine))
         (page-limit (alist-get 'page-limit brb-sn-config))
         (pages (if page-limit
                    (brb-sn--paginate-text header content page-limit)
                  (list (concat header "\n\n" content)))))
    (widget-create 'heading-2 (vulpea-note-title wine))
    (widget-insert "  Copy: ")
    (--each-indexed pages
      (widget-create 'push-button
                     :arg it
                     :notify #'(lambda (widget &rest _)
                                 (kill-new (widget-get widget :arg)))
                     (format "Page #%d" (+ 1 it-index)))
      (when (< it-index (length pages))
        (widget-insert " ")))
    (widget-insert "\n\n  ")
    (widget-create 'push-button
                   :arg rating
                   :notify #'(lambda (widget &rest _)
                               (vulpea-utils-with-note (widget-get widget :arg)
                                (vulpea-buffer-meta-set (alist-get 'name brb-sn-config) "true" t)
                                (save-buffer)))
                   "Mark as Posted")
    (widget-insert " ")
    (widget-create 'push-button
                   :arg wine
                   :notify (lambda (widget &rest _)
                             (vulpea-visit (widget-get widget :arg)))
                   "Visit Wine")
    (widget-insert " ")
    (widget-create 'push-button
                   :arg rating
                   :notify #'(lambda (widget &rest _)
                               (vulpea-visit (widget-get widget :arg)))
                   "Visit Rating")
    (widget-insert "\n")
    (widget-insert "\n")
    (widget-insert (->> (concat score "\n\n" header "\n\n" (s-word-wrap 80 content))
                        (s-lines)
                        (--map (concat "    " it))
                        (s-join "\n")))
    (widget-insert "\n")
    (widget-insert "\n")))

(defun brb-sn-rating-header (event rating wine)
  "Return header of WINE RATING from EVENT."
  (let* ((date (vulpea-note-meta-get rating "date"))
         (order (vulpea-note-meta-get rating "order" 'number))
         (volume (vulpea-note-meta-get wine "volume" 'number))
         (base (vulpea-note-meta-get wine "base" 'number))
         (degorgee (vulpea-note-meta-get wine "degorgee"))
         (sur-lie (vulpea-note-meta-get wine "sur lie"))
         (lines (list
                 date
                 (when event (format "Wine #%d on %s" order (vulpea-note-title event)))
                 (pcase volume
                   (`1500 "Magnum bottle")
                   (`1000 "Double bottle")
                   (`750 nil)
                   (`500 nil)
                   (`375 "Half bottle")
                   (_ (user-error "Unsupported volume %d" volume)))
                 (when base
                   (format "Based on %d" base))
                 (when (and sur-lie (not (string= "N/A" sur-lie)))
                   (concat sur-lie " on lees"))
                 (when degorgee
                   (if (string= "N/A" degorgee)
                       "Disgorgement date unknown"
                     (concat "Disgorged " (if-let ((time (ignore-errors (org-parse-time-string degorgee))))
                                              (concat "on " (format-time-string "%F" (encode-time time)))
                                            (concat "in " degorgee))))))))
    (string-join (--filter it lines) "\n")))

(defun brb-sn-rating-content (_event rating _wine)
  "Return content WINE RATING from EVENT."
  (with-temp-buffer
    (insert
     (vulpea-utils-with-note rating
       (let* ((meta (vulpea-buffer-meta))
              (pl (plist-get meta :pl)))
         (->> (buffer-substring-no-properties
               (org-element-property :end pl)
               (point-max))
              (s-replace-regexp
               org-link-any-re
               (lambda (txt) (match-string 3 txt)))
              (s-replace "—" " - ")
              (s-replace "’" "'")))))
    (let ((fill-column most-positive-fixnum))
      (fill-region (point-min) (point-max)))
    (s-trim (buffer-substring-no-properties (point-min) (point-max)))))

(defun brb-sn--split-into-sentences (text)
  "Split TEXT into a list of sentences."
  (let ((xs (reverse (s-split "\\. " text))))
    (reverse (cons (car xs) (--map (concat it ".") (cdr xs))))))

(defun brb-sn--paginate-text (header content limit)
  "Paginate text with HEADER and CONTENT, respecting LIMIT characters per page.
Returns a list of strings, where each string is a page."
  (let* ((header-with-spacing (concat header "\n\n"))
         (header-length (length header-with-spacing))
         (effective-limit (- limit header-length))
         (sentences (brb-sn--split-into-sentences content))
         (pages '())
         (current-page "")
         (page-number 1))

    ;; Check if content is short enough to fit in one page with header
    (if (<= (+ header-length (length content)) limit)
        (list (concat header-with-spacing content))

      ;; Process sentences for pagination
      (let ((total-pages
             (ceiling (/ (float (length content))
                         effective-limit))))

        ;; First page (includes header)
        (while (and sentences
                    (< (+ (length current-page)
                          (length (car sentences)))
                       effective-limit))
          (setq current-page
                (concat current-page (if (string= current-page "") "" " ")
                        (car sentences)))
          (setq sentences (cdr sentences)))

        (push (concat header-with-spacing
                      (format "[1/%d] " total-pages)
                      current-page)
              pages)

        ;; Remaining pages
        (while sentences
          (setq page-number (1+ page-number))
          (setq current-page "")

          ;; Build page content
          (while (and sentences
                      (< (+ (length current-page)
                            (length (car sentences)))
                         limit))
            (setq current-page
                  (concat current-page
                          (if (string= current-page "") "" " ")
                          (car sentences)))
            (setq sentences (cdr sentences)))

          ;; Add page with numbering
          (push (concat (format "[%d/%d] " page-number total-pages)
                        current-page)
                pages))

        (nreverse pages)))))

(provide 'brb-sn)
;;; brb-sn.el ends here
