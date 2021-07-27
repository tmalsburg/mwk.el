;;; mwk.el --- Magic Wikikasten - A Zettelkasten system  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Titus von der Malsburg <malsburg@posteo.de>
;; Author: Titus von der Malsburg <malsburg@posteo.de>

;;; Commentary:

;; See README.org for details.

;;; Code:

(require 'helm-grep)
;; (require 'helm-adaptive)
;; (require 'helm-files)
(require 'filenotify)
(require 'cl-seq)


;;
;; Chapter 1: Helm commands
;;

;;
;; Adaptation of `helm-do-grep-ag' for our own purposes.
;;

;;; Code:

(defvar helm-mwk-history nil)

;; This allows us to customize helm-mwk.  For now, only the history
;; differs from `helm-grep-ag-class'.
(defclass helm-mwk-references-class (helm-source-async)
  ((nohighlight :initform t)
   (pcre :initarg :pcre :initform t
         :documentation
         "  Backend is using pcre regexp engine when non--nil.")
   (keymap :initform helm-grep-map)
   (history :initform 'helm-mwk-history)
   (help-message :initform 'helm-grep-help-message)
   (filtered-candidate-transformer :initform #'helm-grep-fc-transformer)
   (persistent-action :initform 'helm-grep-persistent-action)
   (persistent-help :initform "Jump to line (`C-u' Record in mark ring)")
   (candidate-number-limit :initform 99999)
   (requires-pattern :initform 2)
   (nomark :initform t)
   (action :initform 'helm-grep-actions)
   (group :initform 'helm-grep)))

;; Copied from helm-grep-ag-1 with some modifications:
(defun helm-mwk (helm-pattern)
  "A helm command for searching strings in the Zettelkasten.

HELM-PATTERN is a pre-defined search term."
  (setq helm-source-grep-mwk-references
        (helm-make-source "Zettelkasten full-text search" 'helm-mwk-references-class
          :candidates-process
          (lambda () (helm-grep-ag-init (file-truename mwk-directory)))))
  (helm-set-local-variable 'helm-input-idle-delay helm-grep-input-idle-delay)
  (helm :sources 'helm-source-grep-mwk-references
        :input helm-pattern
        :truncate-lines helm-grep-truncate-lines
        :buffer (format "*helm %s*" (helm-grep--ag-command))))

;;
;; Full test search in Zettelkasten:
;;

(defun helm-mwk-search ()
  "Helm command for full text search in the Zettelkasten."
  (interactive)
  (helm-mwk ""))

;;
;; Find (back-)references to the current topic:
;;

(defun helm-mwk-references ()
  "Search for references from other Zettelkasten documents to the current one.

The search is limited to .org files in directory specified in
`mwk-directory' and excludes the current document."
  (interactive)
  ;; If the current document is outside mwk-document, we throw an error:
  (unless (string= (file-truename default-directory) (file-truename mwk-directory))
    (error "The current file is not in your Zettelkasten (%s)" mwk-directory))
  (let* ((wikinames (cadar (org-collect-keywords '("wikinames"))))
         (wikinames (if wikinames wikinames (cadar (org-collect-keywords '("title"))))))
    (unless wikinames
      (error "No wiki names in this file.  Add #+TITLE: and/or #+WIKINAMES: properties"))
    (let* ((wikinames (split-string wikinames "," t "[ \t]*"))
           (helm-pattern (mapconcat (lambda (s) (format "(\\b%s\\b)" (replace-regexp-in-string "[ \t]+" "\\\\ " s))) wikinames "|"))
           ;; Don't look for matches in the in the current file:
           (helm-grep-ag-command (format "ag --line-numbers -S --color -n --org --ignore '%s' --nogroup %%s %%s %%s"
                                         (file-name-nondirectory buffer-file-name))))
      (helm-mwk helm-pattern))))

;;
;; Find topics or create new ones:
;;

(defun mwk-helm-new-file-action (_)
  "Create a new Zettelkasten document."
  (let* ((title    helm-pattern)
         (filename (downcase title))
         (filename (replace-regexp-in-string "[ \t]" "_" filename))
         (filename (concat filename ".org"))
         (filename (expand-file-name filename mwk-directory)))
    (find-file filename)
    (unless (file-exists-p filename)
      (insert "#+TITLE: " title "\n#+WIKINAMES: "))))

(setq mwk-helm-new-file-source
      (helm-build-sync-source "New file"
        :candidates  (list "Create ...")
        :match       (lambda (_candidate) t)
        :fuzzy-match nil
        :action (helm-make-actions
                 "Create new file" 'mwk-helm-new-file-action)))

(defun mwk-topics-candidates-transformer (candidates)
  "Prepare a list of topics for visual presentation in Helm.

CANDIDATES contains the topics."
  (let ((width (with-helm-window (1- (window-body-width)))))
    (cl-loop
    for entry in candidates
    collect
    (let* ((file      (car entry))
           (title     (cdr (assoc 'title (cdr entry)))))
      (cons
       (concat
        (truncate-string-to-width title (- width (length file)) 0 ?\s)
        " "
        file)
       file)))))

(defun helm-mwk-topics ()
  "Search existing wiki topics.

The search is limited to .org files in the directory specified in `mwk-directory'."
  (interactive)
  (let ((candidates '()))
    (maphash (lambda (k v) (setq candidates (cons (cons k v) candidates))) mwk-topics)
    (helm :sources (list
                    (helm-build-sync-source "Zettelkasten topics"
                     :candidates (sort candidates (lambda (a b)
                                                    (file-newer-than-file-p
                                                     (expand-file-name (car a) mwk-directory)
                                                     (expand-file-name (car b) mwk-directory))))
                     :candidate-transformer 'mwk-topics-candidates-transformer
                     :fuzzy-match nil
                     :action (helm-make-actions
                              "Open file" (lambda (f) (find-file (expand-file-name f mwk-directory)))))
                    mwk-helm-new-file-source)
          :buffer "*helm mwk topics*")))
 

;;
;; Chapter 2: Global minor mode that watches the zettelkasten and
;; keeps an up-to-date list of topics.
;;
;; Watch directory in `mwk-directory' and reload list of topics if
;; something happens (new file, or modified file, or deleted file).
;;

(defcustom mwk-directory "~/Essentials/documents/zettelkasten/"
  "Path to zettelkasten directory.")

(defvar mwk-file-watch-descriptor nil
  "Watch descriptor for the zettelkasten.
Updates list of topics when there are changes.")

(defvar mwk-topics nil
  "Alist containing currently known topics.
Each entry's key is the path of the topic's org-file and the
value is the regular expression for finding references to the
topic.")

(defun mwk-scan-topics ()
  "Create an up-to-date list of Zettelkasten topics using silversearcher."
  (let* ((command    "cd %s; ag -n --nocolor --org --nogroup '%s'")
         (titles     (shell-command-to-string (format command mwk-directory "^#\\+title:")))
         (wikinames  (shell-command-to-string (format command mwk-directory "^#\\+wikinames:")))
         (titles     (butlast (split-string titles    "\n")))
         (wikinames  (butlast (split-string wikinames "\n"))))
    (setq mwk-topics (make-hash-table :test 'equal))
    ;; Crete hash entries for file with titles:
    (dolist (line titles)
      (let* ((fields (split-string line ":"))
             (file   (car fields))
             (title  (string-trim (nth 3 fields))))
        (puthash file (list (cons 'title title)) mwk-topics)))
    ;; Add wiki names to entries.
    (dolist (line wikinames)
      (let* ((fields     (split-string line ":"))
             (file       (car fields))
             (wikinames  (string-trim (nth 3 fields)))
             (value      (gethash file mwk-topics)))
        (when value  ; We'll ignore file that don't have a title.
          (puthash file (cons (cons 'wikinames wikinames) value) mwk-topics))))))


;;;###autoload
(define-minor-mode global-mwk-mode
  "A global mode that watches the Zettelkasten and keeps an up-to-date list of topics."
  :init-value nil
  :global t
  (if global-mwk-mode
      (progn
        ;; Set up file watcher:
        (setq mwk-file-watch-descriptor
              (file-notify-add-watch mwk-directory '(change) (lambda (_) (mwk-scan-topics))))
        (mwk-scan-topics))
    ;; Remove file watcher:
    (file-notify-rm-watch mwk-file-watch-descriptor)
    (setq mwk-file-watch-descriptor nil)
    (setq mwk-topics nil)))


;;
;; Chapter 3: Minor mode that creates and manages links for topics in
;; the zettelkasten
;;

(defvar-local mwk-matchers nil
  "List of links in local buffer.")

(defun mwk-make-link (regexp zettel-file)
  (let ((matcher (lambda (limit)
                   (when (re-search-forward regexp limit t)
                     (let ((map (make-sparse-keymap)))
                       (define-key map [mouse-1]
                         (lambda () (interactive) (find-file zettel-file)))
                       (set-text-properties
                        (match-beginning 0) (match-end 0)
                        `(local-map ,map mouse-face 'highlight
                                    help-echo "mouse-1: click me"))
                       t)))))
    (font-lock-add-keywords nil `((,matcher (0 'link t))) t)
    (push matcher mwk-matchers)))

(defun mwk-clear-links ()
  (dolist (link mwk-matchers)
    (font-lock-remove-keywords nil `((,link (0 'link t)))))
  (font-lock-flush)
  (setq mwk-matchers nil))

(defun mwk-update-links ()
  "Updates links to current topics in the zettelkasten."
  ;; Remove old links if any:
  (mwk-clear-links)
  ;; Create new links if any:
  (maphash (lambda (file alist)
             (let ((wikinames (cdr (assoc 'wikinames alist))))
               (if wikinames
                   (let* ((wikinames (split-string wikinames ", "))
                          (wikinames (seq-map (lambda (p) (concat "\\<" p "\\>")) wikinames)))
                     (dolist (wn wikinames)
                       (mwk-make-link wn file)
                       (font-lock-flush)))
                 (let ((title (cdr (assoc 'title alist))))
                   (mwk-make-link title file)
                   (font-lock-flush)))))
           mwk-topics))

(defun mwk-update-links-handler (_ _ _ _)
  "Updates links to current topics in the zettelkasten.

Wrapper around `mwk-update-links' to be used by variable
watcher."
  (mwk-update-links))

(define-minor-mode mwk-mode
  "Minor mode that creates link for topics in the zettelkasten."
  :init-value nil
  :lighter " MWK"
  (if mwk-mode
      (progn
        ;; Switch on global-mwk-mode if necessary:
        (unless global-mwk-mode
          (global-mwk-mode 1))
        ;; Activate links:
        (mwk-update-links)
        ;; Watch `mwk-topics' and update links when topics change:
        (add-variable-watcher 'mwk-topics 'mwk-update-links-handler))
    ;; Stop updating links `mwk-topics':
    (remove-variable-watcher 'mwk-topics 'mwk-update-links-handler)
    ;; Remove links:
    (mwk-clear-links)))

(defun mwk-flash-links ()
  "Switch on `mwk-mode' for 3 seconds to briefly show links."
  (interactive)
  (mwk-mode 1)
  (let ((buf (current-buffer)))
    (run-with-timer 3 nil (lambda () (switch-to-buffer buf) (mwk-mode -1)))))

(provide 'mwk)

;;; mwk.el ends here
