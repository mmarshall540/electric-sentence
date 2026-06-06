;;; electric-sentence.el --- Electric sentence endings  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Martin Marshall

;; Author: Martin Marshall <law@martinmarshall.com>
;; Keywords: text, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; See the included README.org file for commentary regarding this
;; package.

;;; Code:

(defgroup electric-sentence nil
  "Add an extra space after sentences."
  :group 'electricity
  :prefix "electric-sentence")

(defvar electric-sentence-end-after-quote t
  "Whether to add an extra space after quotes.") ; boolean

(defvar electric-sentence-end-quotes '(?\" ?”)
  "List of close-quote characters.
This only has effect if `electric-sentence-end-after-quote' is
non-nil.")                              ; list of chars

(defvar electric-sentence-3rd-space-warning
  "2 spaces already found after last sentence!"
  "The warning message to issue when a 3rd space is inserted.
Only affects 3rd space after a sentence-ending, not otherwise.  Set to
nil for no warning message.") ; string or nil

(defvar electric-sentence-3rd-space-inhibit t
  "Whether to prevent insertion of a 3rd space after a sentence.
This only affects the 3rd space after a sentence-ending, and not
otherwise.  Thus, if this option is enabled, you can still escape from
the limitation by typing \\[quoted-insert] \`SPC' just once.") ; boolean

(defvar electric-sentence-enders '(?? ?! ?.)
  "List of characters we consider as ending a sentence.") ; char list

(defcustom electric-sentence-lighter " M-e"
  "Mode-line lighter for `electric-sentence-mode'."
  :type 'string)

;; This needs to be refactored to make it easier to add abbreviations
;; without editing the code.  It should be possible to use Custom to
;; add an abbreviation, without having to know regular expression
;; syntax.  Maybe use a custom `:set' keyword to convert the custom
;; value to a regular expression which is stored separately?  (Problem
;; is that some people will use `setq' out of ignorance and wonder why
;; it doesn't work for them.)
(defcustom electric-sentence-abbrev-regexp
  "\\(\\_<\\|\\.\\)\\([A-Za-z0-9 ]\\|lbs?\\|Rd\\|[Ll]n\\|[Cc]o\\|Inc\\|[DJMS]r\\|[Mv]s\\|[CFMPSp]t\\|alt\\|[Ee]tc\\|[Dd]iv\\|es[pt]\\|Cir\\|Hon\\|Ltd\\|Rev\\|Ste\\|[MD]rs\\|App\\|Sup\\|Apt\\|Ave\\|Assn\\|Blvd\\|[Dd]ept\\|Inst\\|Prof\\|Univ\\)\\."
  "A regular expression to match abbreviations.
This is for matching abbreviations before a period, when those
abbreviations would not usually end a sentence.

Note that it is very difficult to come up with a comprehensive set of
abbreviations.  The best approach is probably to copy from sources in
your field, which you are most likely to use.  For example, the Indigo
Book contains lists of abbreviations for use in legal case titles."
  ;; "\\_<\\(Mrs?\\|Ms\\|[A-Z]\\)\\."
  ;; Other options will be `nil' and a custom regexp string.
  :type 'regexp
  :link '(url-link
          "https://law.resource.org/pub/us/code/blue/IndigoBook.html#T11")
  :link '(url-link
          "https://editorsmanual.com/articles/periods-in-abbreviations/"))

(defvar electric-sentence-max-abbrev-length 10
  "This length includes the trailing period and possibly a prefix.
Thus, you may need to add as much as 2 to the number of letters in the
longest potentially-matching abbreviation in order for your longest
abbreviations to match the check.  This is used to determine how far
back in the buffer we check for an abbreviation.  It should be an
integer that will be long enough to catch any of the abbreviations
matchable by `electric-sentence-abbrev-regexp'.  It is better to
overestimate than to underestimate this.")  ; integer

(defvar electric-sentence-prog-modes '(prog-mode)
  "List of modes in which to limit the behavior to comments and strings.
This will take effect in any modes that have a listed mode as an
ancestor.  The default value is a list containing `prog-mode'.  So, for
example, `lisp-interaction-mode' is derived from `emacs-lisp-mode',
which is derived from `lisp-data-mode', which is itself derived from
`prog-mode'.  So no change to the default value is needed for this
setting to work in any of the foregoing modes.  However, this is a
user-option due to the possibility of programming modes that *aren't*
derived from `prog-mode'.")             ; list of major-mode symbols

(defvar electric-sentence-treat-org-src-as-prog t
  "Whether to treat Org-mode src-blocks like Prog-modes.
In other words, inhibit the behavior in Org-src blocks, except for in
comments and strings.  This makes NO distinction based on the language
used in a src-block.  For example, with this option enabled, the
behavior will be inhibited in Org's HTML src-blocks (except for comments
and strings), even though HTML is not derived from `prog-mode'.") ; boolean

;; Can this be better integrated with the built-in variables for
;; recognizing sentence endings?
(defun electric-sentence-post-self-insert-function ()
  "Maybe insert an extra space after a sentence."
  (when (and (eq last-command-event ? )
             (or (and (not (derived-mode-p
                            electric-sentence-prog-modes))
                      (or (not electric-sentence-treat-org-src-as-prog)
                          (not (eq major-mode 'org-mode))
                          (not (org-in-src-block-p))))
                 (nth 8 (syntax-ppss (point))))) ; comment or string
    ;; Remember that when this runs, a space was just self-inserted.
    ;; So we have to start at (- (point) 1).
    (let ((charb4pt-1 (char-before (- (point) 1)))
          (charb4pt-2 (char-before (- (point) 2)))
          (charb4pt-3 (char-before (- (point) 3)))
          (charb4pt-4 (char-before (- (point) 4))))
      (cond
       ((or (and (memq charb4pt-1 electric-sentence-enders)
                 ;; Check for `electric-sentence-abbrev-regexp'.
                 (not (electric-sentence--abbrev-check
                       (- (point) 1))))
            (and electric-sentence-end-after-quote
                 (memq charb4pt-1 electric-sentence-end-quotes)
                 (memq charb4pt-2 electric-sentence-enders)
                 (not (electric-sentence--abbrev-check
                       (- (point) 2)))))
        (let ((post-self-insert-hook
               (remove 'electric-sentence-post-self-insert-function
                       post-self-insert-hook)))
          (funcall this-command 1)))
       ;; Check if we just added a third space.
       ((and (eq charb4pt-1 ? )
             (eq charb4pt-2 ? )
             (or (memq charb4pt-3 electric-sentence-enders)
                 (and electric-sentence-end-after-quote
                      (memq charb4pt-3 electric-sentence-end-quotes)
                      (memq charb4pt-4 electric-sentence-enders))))
        (when electric-sentence-3rd-space-warning
          (message electric-sentence-3rd-space-warning))
        (when electric-sentence-3rd-space-inhibit
          (delete-char -1)))))))

(defun electric-sentence--abbrev-check (checkpt)
  "Return non-nil if there is a matching abbreviation before CHECKPT.
But if `electric-sentence-abbrev-regexp' is an empty string, return nil."
  (save-excursion
    (goto-char checkpt)
    (and (not (string-empty-p electric-sentence-abbrev-regexp))
         (looking-back
          electric-sentence-abbrev-regexp
          (- checkpt electric-sentence-max-abbrev-length)))))

(define-minor-mode electric-sentence-mode
  "A minor mode to ease creation of sentence-endings in Emacs.
Specifically, this mode enables a command that automatically adds 2
spaces instead of only one when you press SPC after certain punctuation
marks."
  :lighter electric-sentence-lighter
  (if electric-sentence-mode
      (add-hook 'post-self-insert-hook
                'electric-sentence-post-self-insert-function nil t)
    (remove-hook 'post-self-insert-hook
                 'electric-sentence-post-self-insert-function t)))

(provide 'electric-sentence)
;;; electric-sentence.el ends here
