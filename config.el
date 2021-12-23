;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Patryk Gronkiewicz"
      user-mail-address "patryk@gronkiewicz.dev")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "FantasqueSansMono Nerd Font" :size 18)
      doom-big-font (font-spec :family "FantasqueSansMono Nerd Font" :size 36)
      doom-variable-pitch-font (font-spec :family "Ubuntu Nerd Font" :size 18)
      doom-serif-font (font-spec :family "UbuntuMono Nerd Font" :size 18))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'kaolin-valley-light)
(setq fancy-splash-image "~/Pictures/emacs.svg")

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/notes/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(setq calendar-week-start-day 1
      calendar-date-style 'iso)
(use-package! org-pretty-table
  :commands (org-pretty-table-mode global-org-pretty-table-mode))
(use-package! org-ol-tree
  :commands org-ol-tree)
(map! :map org-mode-map
      :after org
      :localleader
      :desc "Outline" "O" #'org-ol-tree)
      
(use-package! guess-language)
(setq guess-language-languages '(en pl)
      guess-language-min-paragraph-length 45)
(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.5))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.4))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.3))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.2))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.1)))))

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
       :desc "Open ORUI" :n "G" #'orui-open))
(setq ispell-dictionary "pl")

(setq calendar-christian-all-holidays-flag t)
(setq calendar-holidays
 '((holiday-fixed 1 1 "New Year's Day")
   (holiday-fixed 2 14 "Valentine's Day")
   (holiday-fixed 3 17 "St. Patrick's Day")
   (holiday-fixed 4 1 "April Fools' Day")
   (holiday-fixed 5 25 "Mother's Day")
   (holiday-fixed 5 2 "Flag Day")
   (holiday-fixed 6 0 "Father's Day")
   (holiday-fixed 11 11 "Independence Day")
   (holiday-fixed 5 1 "Labor Day")
   (holiday-easter-etc)
   (holiday-fixed 12 25 "Christmas")
   (if calendar-christian-all-holidays-flag
       (append
        (holiday-fixed 1 6 "Epiphany")
        (holiday-fixed 12 24 "Christmas Eve")
        ;; (holiday-julian 12 25 "Christmas (Julian calendar)")
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

;;; org-roam

;;;; create notes without entering
(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))
(map! :leader :desc "Create node without opening" "n r I" #'org-roam-node-insert-immediate)


;;; projectile
(setq projectile-project-search-path '(("~/Projects" . 2)))

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
        ("" "hyperref" nil)
        ("AUTO" "babel" t ("pdflatex"))
        ("AUTO" "polyglossia" t ("xelatex" "lualatex"))))

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
;;; latexmk export
(setq org-latex-pdf-process '("latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f"))
;;; orgmode misc
(use-package! org-alert)
(use-package! org-ol-tree)
(use-package! org-fragtog)
;;; agenda
;;;;;;;;;;;;
;; AGENDA ;;
;;;;;;;;;;;;

(setq org-lowest-priority ?E)

(use-package! nano-agenda)
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
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WIP(w)" "INTR(i)" "DONE(d)")))
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
                          (:name "University"
                           :tag "uczelnia"
                           :order 32)
                          (:name "Trivial"
                           :priority<= "E"
                           :tag ("Trivial" "Unimportant")
                           :todo ("SOMEDAY")
                           :order 90)
                          (:discard (:tag ("Chore" "Routine" "Daily")))))))))))
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
