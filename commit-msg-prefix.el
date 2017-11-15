;;; commit-msg-prefix.el --- Insert commit message prefix (issue number)  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Raimon Grau

;; Author: Raimon Grau <raimonster@gmail.com>
;; Keywords: vc, tools

;; URL: http://github.com/kidd/commit-msg-prefix.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "24") (s "1.10.0") (dash "2.9.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Package to help adding information on commits by checking older
;; commits and extracting a particular substring from each of them.

;; Useful to follow organisation policies on how to write commits.

;; The default command presents a searchable list of the previous
;; commits (more recent first), and lets you select the one you want.
;; This list of candidates is configurable via
;; `commit-msg-prefix-log-command`.  Once selected, the relevant part
;; of the commit line will be extracted from the choosen candidate
;; via the regex in `commit-msg-prefix-regex', and the matched text
;; will be inserted in the current buffer.


;;; Code:

(require 's)
(require 'dash)

(defgroup commit-msg-prefix nil
  "Pick a past commit message to build a new commit message."
  :prefix "commit-msg-prefix-"
  :group 'applications)

(defcustom commit-msg-prefix-log-command "git log --pretty=format:\"%s\""
  "Main vcs command to run to populate the candidates list."
  :group 'commit-msg-prefix
  :type 'string)

(defcustom commit-msg-prefix-log-flags ""
  "Extra flags for `commit-msg-prefix-log-command'.
To narrow/extend the candidates listing.  For example:
\"--author=rgrau --since=1.week.ago --no-merges\""
  :group 'commit-msg-prefix
  :type 'string)

(defcustom commit-msg-prefix-regex "^\\([^ ]*\\)"
  "Regex to match against the populated list. The first match
will be inserted on the current buffer"
  :group 'commit-msg-prefix
  :type 'string)

(defcustom commit-msg-prefix-input-method 'ido-completing-read
  "Input method for ‘commit-msg-prefix’."
  :group 'commit-msg-prefix
  :type '(choice ('completing-read
                  'ido-completing-read
                  'helm-comp-read
                  'ivy-read)))

(defvar commit-msg-prefix-prompt "pick commit:")

(defun commit-msg-prefix-input-fun ()
  "Show picker with candidates."
  (funcall commit-msg-prefix-input-method
           commit-msg-prefix-prompt
           (commit-msg-prefix-1)))

(defun commit-msg-prefix-1 ()
  "Internal function to fetch all candidates."
  (let ((vc-command (format "%s %s"
                            commit-msg-prefix-log-command
                            commit-msg-prefix-log-flags)))
    (cons
     (s-prepend (car (vc-git-branches))
                " - *current branch*")
     (s-lines (shell-command-to-string vc-command)))))

;;;###autoload
(defun commit-msg-prefix ()
  "Insert the relevant part of the chosen commit.
Relevant meaning the result of `commit-msg-prefix-regex'
substitution."
  (interactive)
  (insert
   (second
    (s-match commit-msg-prefix-regex
             (commit-msg-prefix-input-fun)))))

(provide 'commit-msg-prefix)
;;; commit-msg-prefix.el ends here
