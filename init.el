(doom!
       :completion
           (company
           +childframe)
           (vertico +childframe +icons)
       :ui
           doom                ; what makes DOOM look the way it does
           doom-dashboard      ; a nifty splash screen for Emacs
           doom-quit           ; DOOM quit-message prompts when you quit Emacs
           hl-todo             ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
           (ligatures +extra)  ; ligatures and symbols to make your code pretty again
           vi-tilde-fringe     ; fringe tildes to mark beyond EOB
           (modeline +light)            ; snazzy, Atom-inspired modeline, plus API
           (emoji +github +unicode)
           ophints             ; highlight the region an operation acts on
           (popup +all)        ; tame sudden yet inevitable temporary windows
           zen                 ; distraction-free coding or writing
           fill-column
           indent-guides       ; highlighted indent columns
           vc-gutter           ; vcs diff in the fringe
           (treemacs +lsp)              ; a project drawer, like neotree but cooler
           (window-select +numbers)     ; visually switch windows
           workspaces          ; tab emulation, persistence & separate workspaces
       :editor
           (evil +everywhere)       ; come to the dark side, we have cookies
           file-templates      ; auto-snippets for empty files
           fold                ; (nigh) universal code folding
           (format
           +onsave)           ; automated prettiness
           rotate-text       ; cycle region at point between text candidates
           snippets            ; my elves. They type so I don't have to
           word-wrap           ; soft wrapping with language-aware indent
       :emacs
           (dired
           +icons
           +ranger)         ; making dired pretty [functional]
           electric          ; smarter, keyword-based electric-indent
           (ibuffer +icons)         ; interactive buffer management
           (undo
           +tree)           ; persistent, smarter undo for your inevitable mistakes
           vc                ; version-control and Emacs, sitting in a tree
       :term
           vterm               ; the best terminal emulation in Emacs
       :checkers
           (syntax +childframe)     ; tasing you for every semicolon you forget
           (spell +flyspell +hunspell)        ; tasing you for misspelling mispelling
           grammar           ; tasing grammar mistake every you make
       :tools
           ;;ansible
           biblio
           debugger          ; FIXME stepping through code, to help you add bugs
           direnv
           (docker +lsp)
           editorconfig        ; let someone else argue about tabs vs spaces
           ein                 ; tame Jupyter notebooks with emacs
           (eval +overlay)     ; run code, run (also, repls)
           (lookup
           +offline
           +dictionary
           +docsets)           ; navigate your code and its documentation
           (lsp +peek)
           (magit
           +forge)            ; a git porcelain for Emacs
           make                ; run make tasks from Emacs
           pdf                 ; pdf enhancements
           rgb                 ; creating color strings
           terraform         ; infrastructure as code
           upload              ; map local to remote projects via ssh/ftp
       :os
           tty               ; improve the terminal Emacs experience
       :lang
           (beancount +lsp)         ; mind the GAAP
           ;;(cc
           ;; +lsp)               ; C > C++ == 1
           ;;(csharp +lsp +dotnet)            ; unity, .NET, and mono shenanigans
           data                ; config/data formats
           emacs-lisp          ; drown in parentheses
           (ess +lsp)               ; emacs speaks statistics
           ;;(go +lsp)         ; the hipster dialect
           (json
           +lsp)              ; At least it ain't XML
           ;;javascript        ; all(hope(abandon(ye(who(enter(here))))))
           (latex
           +latexmk
           +cdlatex)           ; writing papers in Emacs has never been so fun
           (lua +lsp +fennel)               ; one-based indices? one-based indices
           (markdown)
           nix                 ; I hereby declare "nix geht mehr!"
           (org
           +dragndrop
           +gnuplot
           +pandoc
           +pomodoro
           +pretty
           +hugo
           +present
           +roam2)              ; organize your plain life in plain text
           plantuml            ; diagrams for confusing people more
           (python
           +lsp
           +conda
           +poetry
           +pyright)             ; beautiful is better than ugly
           rest              ; Emacs as a REST client
           ;;rst               ; ReST in peace
           ;;(rust
           ;;+lsp)              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
           sh                ; she sells {ba,z,fi}sh shells on the C xor
           web               ; the tubes
           (yaml
           +lsp)              ; JSON, but readable
       :email
           (mu4e +org)
       :app
           calendar
           irc               ; how neckbeards socialize
           rss        ; emacs as an RSS reader
       :config
           (default +bindings +smartparens)
)
