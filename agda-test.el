;; test fiveness: five is 5;
;; test sevenocity: seven is 7;

(defvar agda2-error-buffer-name "*Agda errors*")
(defun agda2-error-buffer () (find-file-noselect agda2-error-buffer-name))

(defun agda2-run-test (testname lhs rhs)
  (with-current-buffer (agda2-error-buffer)
    (insert (format "Testname: %s, LHS: %s, RHS: %s\n" testname lhs rhs))))

;; test negativity: -1 is minus one;

(defun agda2-test-all ()
  (interactive)
  (let ((rx "test\\s-*\\(.*?\\)\\s-*:\\s-*\\(.*?\\)\\s-+is\\s-+\\(.*\\);"))
    (save-excursion
      (pop-to-buffer agda2-error-buffer-name t)
      (delete-region (point-min) (point-max)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward rx nil t)
	;; Now `(match-string n)` for some integer `n` gives you the Nth
	;; capturing group from the most recent match of `rx`.
	(agda2-run-test (match-string 1) (match-string 2) (match-string 3))))))