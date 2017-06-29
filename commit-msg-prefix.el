;;; commit-msg-prefix.el --- Insert commit message prefix (issue number)  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Raimon Grau

;; Author: Raimon Grau <rgrau@noname>
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

;;

;;; Code:

(require 's)
(require 'dash)

(defvar git-msg-prefix-log-command " git log --pretty=format:\"%s\"")
(defvar git-log-flags "")

(defvar git-msg-prefix-log-flags "")
(defvar git-msg-prefix-regex "^\\([^ ]*\\) ")

(defun commit-msg-prefix ()
  (let* ((git-command (format "%s %s"
                              git-msg-prefix-log-command
                              git-msg-prefix-log-flags))
         (log (s-lines (shell-command-to-string git-command)))
         (issues
          (mapcar (lambda (x)
                    (second (s-match git-msg-prefix-regex x)))
                   log)))
    (-zip issues log)))

;(commit-msg-prefix)





(provide 'commit-msg-prefix)
;;; commit-msg-prefix.el ends here
