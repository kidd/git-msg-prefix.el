;;; commit-msg-prefix.el --- Insert commit message prefix (issue number)  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Raimon Grau

;; Author: Raimon Grau <raimonster@gmail.com>
;; Keywords: vc, tools

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

;;  (add-hook 'git-commit-mode-hook 'commit-msg-prefix)

;;; Code:

(require 's)
(require 'dash)

(defvar commit-msg-prefix-log-command "git log --pretty=format:\"%s\""
  "Main vcs command to run to populate the candidates list.")

(defvar commit-msg-prefix-log-flags ""
  "extra flags that will be concatenated onto
  `commit-msg-prefix-log-command' to narrow/extend the candidates
  listing.  For example:
 \"--author=rgrau --since=1.week.ago --no-merges\"")

(defvar commit-msg-prefix-regex "^\\([^ ]*\\) "
  "Regex to match against the populated list. The first match
  will be inserted on the current buffer")

(defvar commit-msg-prefix-prompt "pick commit:")

(defcustom commit-msg-prefix-input-method 'ido-completing-read
  "Input method for commit-msg-prefix"
  :group 'commit-msg-prefix
  :type '(choice ('completing-read
                  'ido-completing-read
                  'commit-msg-prefix-helm-read
                  'ivy-read)))

(defun commit-msg-prefix-input-fun ()
  (funcall commit-msg-prefix-input-method
           commit-msg-prefix-prompt
           (commit-msg-prefix-1)))

(defun commit-msg-prefix-helm-read (prompt log-lines)
  (helm :sources (helm-build-sync-source prompt
                   :candidates log-lines)))


(defun commit-msg-prefix-1 ()
  (let ((vc-command (format "%s %s"
                            commit-msg-prefix-log-command
                            commit-msg-prefix-log-flags)))
    (s-lines (shell-command-to-string vc-command))))

;;;###autoload
(defun commit-msg-prefix ()
  (interactive)
  (insert
   (second
    (s-match commit-msg-prefix-regex
             (commit-msg-prefix-input-fun)))))

(provide 'commit-msg-prefix)
;;; commit-msg-prefix.el ends here
