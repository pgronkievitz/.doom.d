(setq user-full-name "Patryk Gronkiewicz")
(if (eq (system-name) "themis")
    (setq user-mail-address "patryk.gronkiewicz@omniscopy.com")
    (setq user-mail-address "patryk@gronkiewicz.dev"))
(setq doom-font (font-spec :family "VictorMono Nerd Font" :size 14)
      doom-big-font (font-spec :family "VictorMono Nerd Font" :size 28)
      doom-variable-pitch-font (font-spec :family "Merriweather" :size 14)
      doom-serif-font (font-spec :family "UbuntuMono Nerd Font" :size 14))
(custom-set-faces!
  '(org-level-1 :height 1.5)
  '(org-level-2 :height 1.4)
  '(org-level-3 :height 1.3)
  '(org-level-4 :height 1.2)
  '(org-level-5 :height 1.1)
  '(org-table :inherit 'fixed-pitch)
  '(org-document-title :inherit 'variable-pitch :height 2.0)
  '(org-inline-src-block :inherit 'fixed-pitch)
  '(org-block :inherit 'fixed-pitch)
  '(line-number :inherit 'fixed-pitch)
  '(org-code :inherit 'fixed-pitch))
(setq doom-theme 'doom-horizon-light)
(setq fancy-splash-image "~/Pictures/emacs.svg")
(setq calendar-week-start-day 1
      calendar-date-style 'iso
      calendar-christian-all-holidays-flag t)
(setq calendar-holidays
 '((holiday-fixed 1 1 "New Year's Day")
   (holiday-fixed 2 14 "Valentine's Day")
   (holiday-fixed 3 17 "St. Patrick's Day")
   (holiday-fixed 4 1 "April Fools' Day")
   (holiday-fixed 5 26 "Mother's Day")
   (holiday-fixed 5 2 "Flag Day")
   (holiday-fixed 5 3 "Constitution Day")
   (holiday-fixed 6 0 "Father's Day")
   (holiday-fixed 11 11 "Independence Day")
   (holiday-fixed 5 1 "Labor Day")
   (holiday-easter-etc)
   (holiday-fixed 12 25 "Christmas")
   (if calendar-christian-all-holidays-flag
       (append
        (holiday-fixed 1 6 "Epiphany")
        (holiday-fixed 12 24 "Christmas Eve")
        (holiday-fixed 8 15 "Assumption")
        (holiday-advent 0 "Advent")))
   (solar-equinoxes-solstices)
   (holiday-sexp calendar-daylight-savings-starts
                 (format "Daylight Saving Time Begins %s"
                         (solar-time-string
                          (/ calendar-daylight-savings-starts-time
                             (float 60))
                          calendar-standard-time-zone-name)))
   (holiday-sexp calendar-daylight-savings-ends
                 (format "Daylight Saving Time Ends %s"
                         (solar-time-string
                          (/ calendar-daylight-savings-ends-time
                             (float 60))
                          calendar-daylight-time-zone-name)))))
(setq org-directory "~/Documents/notes/")
(use-package! screenshot)
(use-package! org-pretty-table
  :commands (org-pretty-table-mode global-org-pretty-table-mode))
(after! org-mode (global-org-pretty-table-mode))
(use-package! org-ol-tree
  :commands org-ol-tree)
(map! :map org-mode-map
      :after org
      :localleader
      :desc "Outline" "O" #'org-ol-tree)
(setq display-line-numbers-type 'relative)
(setq org-hidden-keywords '(title))
(setq org-startup-indented t
      org-superstar-headline-bullets-list '("◉" "◈" "○" "▷")
      org-superstar-special-todo-items t
      org-ellipsis "  "
      org-pretty-entities t
      org-hide-emphasis-markers t
      org-agenda-block-separator "~~~~~~~~~"
      org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t)
(setq
 org-pomodoro-keep-killed-pomodoro-time t)

(after! ox-hugo
  (setq org-blackfriday--org-element-string '((src-block . "Kod") (table . "Tabela") (figure . "Rysunek"))))

(use-package! websocket
  :after org-roam)
(use-package! org-roam-ui
  :after org
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))
(map! (:map org-mode-map
       :localleader
       :prefix ("m" . "org-roam")
       :desc "Open ORUI" :n "G" #'org-roam-ui-open))
;;; org-roam

;;;; create notes without entering
(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))
(map! :leader :desc "Create node without opening" "n r I" #'org-roam-node-insert-immediate)
;;; latex export
(setq org-latex-default-packages-alist
      '(("AUTO" "inputenc" t ("pdflatex"))
        ("T1" "fontenc" t ("pdflatex"))
        ("" "graphicx" t)
        ("" "longtable" nil)
        ("" "wrapfig" nil)
        ("" "rotating" nil)
        ("normalem" "ulem" t)
        ("" "amsmath" t)
        ("" "amssymb" t)
        ("" "capt-of" nil)
        ("" "listings")
        ("" "color")
        ("hidelinks" "hyperref" nil)
        ("AUTO" "babel" t ("pdflatex"))
        ("AUTO" "polyglossia" t ("xelatex" "lualatex")))
      org-latex-listings t)


;;; bibliography
(defvar my/bibs '("~/Documents/biblio.bib"))
(use-package citar-org
  :no-require
  :bind ; optional
  (:map org-mode-map
        ("C-c b" . #'org-cite-insert)) ; Also bound to C-c C-x C-@
  :custom
  (org-cite-global-bibliography my/bibs)
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar))
;;; latexmk export
(setq org-latex-pdf-process '("latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f"))
;;; orgmode misc
(use-package! org-alert)
(use-package! org-ol-tree)
(use-package! org-fragtog)
(use-package laas
  :hook ((LaTeX-mode org-mode) . laas-mode)
  :config ; do whatever here
  (aas-set-snippets 'laas-mode
                    ;; set condition!
                    :cond #'texmathp ; expand only while in math
                    "supp" "\\supp"
                    "On" "O(n)"
                    "O1" "O(1)"
                    "Olog" "O(\\log n)"
                    "Olon" "O(n \\log n)"
                    ;; bind to functions!
                    "Sum" (lambda () (interactive)
                            (yas-expand-snippet "\\sum_{$1}^{$2} $0"))
                    ;; add accent snippets
                    :cond #'laas-object-on-left-condition
                    "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))
                    "zz" (lambda () (interactive) (laas-wrap-previous-object "mathcal"))
                    :cond #'laas-org-mathp
                    "supp" "\\supp"
                    "On" "O(n)"
                    "O1" "O(1)"
                    "Olog" "O(\\log n)"
                    "Olon" "O(n \\log n)"
                    "ooo" "\\infty"
                    "RR" "\\mathbb{R}"
                    "ZZ" "\\mathbb{Z}"
                    "NN" "\\mathbb{N}"
                    "cc" "\\subset"
                    ;; bind to functions!
                    "Sum" (lambda () (interactive)
                            (yas-expand-snippet "\\sum_{$1^{$2} $0"))))

;;; agenda
;;;;;;;;;;;;
;; AGENDA ;;
;;;;;;;;;;;;

(setq org-lowest-priority ?E)

(use-package! pretty-agenda)
(use-package! org-super-agenda
  :commands org-super-agenda-mode)
(after! org-agenda
  (org-super-agenda-mode))

(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-todo-ignore-scheduled 'future
      org-agenda-block-separator nil
      org-agenda-tags-column 100 ;; from testing this seems to be a good value
      org-agenda-compact-blocks t)
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WIP(w)" "INTR(i)" "|" "DONE(d)" "DELEGATED(D)" "KILL(k)")
                          (sequence "PROJ(p)" "DONE(d)")))
(setq org-agenda-custom-commands
      '(("o" "Overview"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "Today"
                          :time-grid t
                          :date today
                          :todo "TODAY"
                          :scheduled today
                          :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '((:name "Next to do"
                           :todo "NEXT"
                           :order 1)
                          (:name "Unclosed loops"
                           :todo "WIP"
                           :order 2)
                          (:name "Important"
                           :tag "Important"
                           :priority "A"
                           :order 6)
                          (:name "Due Today"
                           :deadline today
                           :order 2)
                          (:name "Due Soon"
                           :deadline future
                           :order 8)
                          (:name "Overdue"
                           :deadline past
                           :face error
                           :order 7)
                          (:name "Self care"
                           :tag "selfhelp"
                           :order 9)
                          (:name "Assignments"
                           :tag "Assignment"
                           :order 10)
                          (:name "Issues"
                           :tag "Issue"
                           :order 12)
                          (:name "Emacs"
                           :tag "Emacs"
                           :order 13)
                          (:name "Projects"
                           :tag "Project"
                           :order 14)
                          (:name "Research"
                           :tag "Research"
                           :order 15)
                          (:name "To read"
                           :tag "Read"
                           :order 30)
                          (:name "Waiting"
                           :todo "WAITING"
                           :order 20)
                          (:name "Projekt inżynierski"
                           :tag "inz"
                           :order 31)
                          (:name "University"
                           :tag "uczelnia"
                           :order 32)
                          (:name "Computers"
                           :tag "computers"
                           :order 33)
                          (:name "Selfhosted"
                           :tag "selfhosted"
                           :order 34)
                          (:name "Trivial"
                           :priority<= "E"
                           :tag ("Trivial" "Unimportant")
                           :todo ("SOMEDAY")
                           :order 90)
                          (:discard (:tag ("Chore" "Routine" "Daily")))))))))))
(setq org-super-agenda-header-map (make-sparse-keymap))
(use-package! doct :commands doct)
(after! org-capture
  (defun org-capture-select-template-prettier (&optional keys)
    "Select a capture template, in a prettier way than default
  Lisp programs can force the template by setting KEYS to a string."
    (let ((org-capture-templates
           (or (org-contextualize-keys
                (org-capture-upgrade-templates org-capture-templates)
                org-capture-templates-contexts)
               '(("t" "Task" entry (file+headline "" "Tasks")
                  "* TODO %?\n  %u\n  %a")))))
      (if keys
          (or (assoc keys org-capture-templates)
              (error "No capture template referred to by \"%s\" keys" keys))
        (org-mks org-capture-templates
                 "Select a capture template\n━━━━━━━━━━━━━━━━━━━━━━━━━"
                 "Template key: "
                 `(("q" ,(concat (all-the-icons-octicon "stop" :face 'all-the-icons-red :v-adjust 0.01) "\tAbort")))))))
  (advice-add 'org-capture-select-template :override #'org-capture-select-template-prettier)
  
  (defun org-mks-pretty (table title &optional prompt specials)
    "Select a member of an alist with multiple keys. Prettified.
  
  TABLE is the alist which should contain entries where the car is a string.
  There should be two types of entries.
  
  1. prefix descriptions like (\"a\" \"Description\")
     This indicates that `a' is a prefix key for multi-letter selection, and
     that there are entries following with keys like \"ab\", \"ax\"…
  
  2. Select-able members must have more than two elements, with the first
     being the string of keys that lead to selecting it, and the second a
     short description string of the item.
  
  The command will then make a temporary buffer listing all entries
  that can be selected with a single key, and all the single key
  prefixes.  When you press the key for a single-letter entry, it is selected.
  When you press a prefix key, the commands (and maybe further prefixes)
  under this key will be shown and offered for selection.
  
  TITLE will be placed over the selection in the temporary buffer,
  PROMPT will be used when prompting for a key.  SPECIALS is an
  alist with (\"key\" \"description\") entries.  When one of these
  is selected, only the bare key is returned."
    (save-window-excursion
      (let ((inhibit-quit t)
            (buffer (org-switch-to-buffer-other-window "*Org Select*"))
            (prompt (or prompt "Select: "))
            case-fold-search
            current)
        (unwind-protect
            (catch 'exit
              (while t
                (setq-local evil-normal-state-cursor (list nil))
                (erase-buffer)
                (insert title "\n\n")
                (let ((des-keys nil)
                      (allowed-keys '("\C-g"))
                      (tab-alternatives '("\s" "\t" "\r"))
                      (cursor-type nil))
                  ;; Populate allowed keys and descriptions keys
                  ;; available with CURRENT selector.
                  (let ((re (format "\\`%s\\(.\\)\\'"
                                    (if current (regexp-quote current) "")))
                        (prefix (if current (concat current " ") "")))
                    (dolist (entry table)
                      (pcase entry
                        ;; Description.
                        (`(,(and key (pred (string-match re))) ,desc)
                         (let ((k (match-string 1 key)))
                           (push k des-keys)
                           ;; Keys ending in tab, space or RET are equivalent.
                           (if (member k tab-alternatives)
                               (push "\t" allowed-keys)
                             (push k allowed-keys))
                           (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) (propertize "›" 'face 'font-lock-comment-face) "  " desc "…" "\n")))
                        ;; Usable entry.
                        (`(,(and key (pred (string-match re))) ,desc . ,_)
                         (let ((k (match-string 1 key)))
                           (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) "   " desc "\n")
                           (push k allowed-keys)))
                        (_ nil))))
                  ;; Insert special entries, if any.
                  (when specials
                    (insert "─────────────────────────\n")
                    (pcase-dolist (`(,key ,description) specials)
                      (insert (format "%s   %s\n" (propertize key 'face '(bold all-the-icons-red)) description))
                      (push key allowed-keys)))
                  ;; Display UI and let user select an entry or
                  ;; a sub-level prefix.
                  (goto-char (point-min))
                  (unless (pos-visible-in-window-p (point-max))
                    (org-fit-window-to-buffer))
                  (let ((pressed (org--mks-read-key allowed-keys
                                                    prompt
                                                    (not (pos-visible-in-window-p (1- (point-max)))))))
                    (setq current (concat current pressed))
                    (cond
                     ((equal pressed "\C-g") (user-error "Abort"))
                     ;; Selection is a prefix: open a new menu.
                     ((member pressed des-keys))
                     ;; Selection matches an association: return it.
                     ((let ((entry (assoc current table)))
                        (and entry (throw 'exit entry))))
                     ;; Selection matches a special entry: return the
                     ;; selection prefix.
                     ((assoc current specials) (throw 'exit current))
                     (t (error "No entry available")))))))
          (when buffer (kill-buffer buffer))))))
  (advice-add 'org-mks :override #'org-mks-pretty)
  (setq org-capture-templates
        (doct '(("Personal todo" :keys "t"
                   :icon ("checklist" :set "octicon" :color "green")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Private"
                   :type entry
                   :template ("* TODO %?"
                              "%a"))
                  ("Personal note" :keys "n"
                   :icon ("sticky-note-o" :set "faicon" :color "green")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Private"
                   :type entry
                   :template ("* %?"
                              "%a"))
                ("University" :keys "u"
                 :icon ("university" :set "faicon" :color "blue")
                 :file +org-capture-todo-file
                 :prepend t
                 :template ("* TODO %?"
                            " %a")
                 :children (
("Koło" :keys "k" :icon ("university" :set "faicon" :color "magenta") :headline "Koło")
("Autoprezentacja i wystąpienia publiczne" :keys "a" :icon ("university" :set "faicon" :color "magenta") :headline "Autoprezentacja i wystąpienia publiczne")
("Ekonometria" :keys "e" :icon ("university" :set "faicon" :color "magenta") :headline "Ekonometria")
("Modelowanie danych" :keys "M" :icon ("university" :set "faicon" :color "magenta") :headline "Modelowanie danych")
("Nowoczesne metody uczenia maszynowego" :keys "m" :icon ("university" :set "faicon" :color "magenta") :headline "Nowoczesne metody uczenia maszynowego")
("Procesy stochastyczne" :keys "s" :icon ("university" :set "faicon" :color "magenta") :headline "Procesy stochastyczne")
("Rozwój kompetencji biznesowych" :keys "r" :icon ("university" :set "faicon" :color "magenta") :headline "Rozwój kompetencji biznesowych")
("Usługi sieciowe w biznesie" :keys "S" :icon ("university" :set "faicon" :color "magenta") :headline "Usługi sieciowe w biznesie")
("Wielowymiarowa analiza danych" :keys "d" :icon ("university" :set "faicon" :color "magenta") :headline "Wielowymiarowa analiza danych")
("Wnioskowanie w warunkach niepewności" :keys "n" :icon ("university" :set "faicon" :color "magenta") :headline "Wnioskowanie w warunkach niepewności")
("Teoria gier" :keys "g" :icon ("university" :set "faicon" :color "magenta") :headline "Teoria gier")
("J. angielski dla inżynierów" :keys "e" :icon ("university" :set "faicon" :color "magenta") :headline "J. angielski dla inżynierów")                ))
                                 ("Project" :keys "p"
                   :icon ("repo" :set "octicon" :color "silver")
                   :prepend t
                   :type entry
                   :headline "Inbox"
                   :template ("* %{time-or-todo} %?"
                              "%i"
                              "%a")
                   :file ""
                   :custom (:time-or-todo "")
                   :children (("Project-local todo" :keys "t"
                               :icon ("checklist" :set "octicon" :color "green")
                               :time-or-todo "TODO"
                               :file +org-capture-project-todo-file)
                              ("Project-local note" :keys "n"
                               :icon ("sticky-note" :set "faicon" :color "yellow")
                               :time-or-todo "%U"
                               :file +org-capture-project-notes-file)
                              ("Project-local changelog" :keys "c"
                               :icon ("list" :set "faicon" :color "blue")
                               :time-or-todo "%U"
                               :heading "Unreleased"
                               :file +org-capture-project-changelog-file)))
                  ("\tCentralised project templates"
                   :keys "o"
                   :type entry
                   :prepend t
                   :template ("* %{time-or-todo} %?"
                              "%i"
                              "%a")
                   :children (("Project todo"
                               :keys "t"
                               :prepend nil
                               :time-or-todo "TODO"
                               :heading "Tasks"
                               :file +org-capture-central-project-todo-file)
                              ("Project note"
                               :keys "n"
                               :time-or-todo "%U"
                               :heading "Notes"
                               :file +org-capture-central-project-notes-file)
                              ("Project changelog"
                               :keys "c"
                               :time-or-todo "%U"
                               :heading "Unreleased"
                               :file +org-capture-central-project-changelog-file)))
                ))))
(add-hook! 'org-mode-hook #'org-pretty-table-mode)
(add-hook! 'org-mode-hook #'+org-pretty-mode)
(add-hook! 'org-mode-hook #'hl-line-mode)
(add-hook! 'org-mode-hook #'mixed-pitch-mode)
(add-hook! 'org-mode-hook #'laas-mode)
(use-package! guess-language
  :config
  :init (add-hook 'text-mode-hook #'guess-language-mode)
(setq guess-language-langcodes '((en . ("en_US" "English"))
                                 (pl . ("pl_PL" "Polish")))
      guess-language-languages '(en pl)))
(setq ispell-dictionary "pl_PL")
(setq langtool-bin "/etc/profiles/per-user/pg/bin/languagetool-commandline"
      langtool-default-language nil)
;;; projectile
(setq projectile-project-search-path '(("~/Projects" . 2)))
;;; ess config
(setq ess-R-font-lock-keywords
      '((ess-R-fl-keyword:keywords . t)
        (ess-R-fl-keyword:constants . t)
        (ess-R-fl-keyword:modifiers . t)
        (ess-R-fl-keyword:fun-defs . t)
        (ess-R-fl-keyword:assign-ops . t)
        (ess-R-fl-keyword:%op% . t)
        (ess-fl-keyword:fun-calls . t)
        (ess-fl-keyword:numbers . t)
        (ess-fl-keyword:operators . t)
        (ess-fl-keyword:delimiters . t)
        (ess-fl-keyword:= . t)
        (ess-R-fl-keyword:F&T . t)))
;;;;;;;;;;;
;; EMAIL ;;
;;;;;;;;;;;

(defvar my-mu4e-account-alist
  '(("Private"
     (mu4e-sent-folder "/private/Saved Items")
     (mu4e-drafts-folder "/private/Drafts")
     (user-mail-address "patryk@gronkiewi.cz")
     (smtpmail-default-smtp-server "smtp.purelymail.com")
     (smtpmail-local-domain "purelymail.com")
     (smtpmail-smtp-user "patryk@gronkiewi.cz")
     (smtpmail-smtp-server "smtp.purelymail.com")
     (smtpmail-stream-type starttls)
     (smtpmail-smtp-service 587))
    ("University"
     (mu4e-sent-folder "/university/Saved Items")
     (mu4e-drafts-folder "/university/Drafts")
     (user-mail-address "164157@stud.prz.edu.pl")
     (smtpmail-default-smtp-server "stud.prz.edu.pl")
     (smtpmail-local-domain "stud.prz.edu.pl")
     (smtpmail-smtp-user "164157@stud.prz.edu.pl")
     (smtpmail-smtp-server "stud.prz.edu.pl")
     (smtpmail-stream-type starttls)
     (smtpmail-smtp-service 587))))
(defun my-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                my-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                             nil t nil nil (caar my-mu4e-account-alist))))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))

;; ask for account when composing mail
(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

(use-package! elpher)
(use-package! elfeed-protocol
  :after elfeed
  :config (elfeed-protocol-enable)
  :custom
  (elfeed-use-curl t)
  (elfeed-set-timeout 36000)
  (elfeed-feeds (list
                 (list "fever+https://pg@rss.lab.home"
                       :api-url "https://rss.lab.home/api/fever.php"
                       :use-authinfo t))))
