(require 'cl)

;; test fiveness: five is 5;
;; test sevenocity: seven is 7;

(defvar agda2-error-buffer-name "*Agda errors*")
(defvar agda2-test-regexp
  "test\\s-*\\(.*?\\)\\s-*:\\s-*\\(.*?\\)\\s-+is\\s-+\\(.*\\);")

(defun agda2-run-test (testname lhs rhs)
  (with-current-buffer agda2-error-buffer-name
    (insert (format "Testname: %s, LHS: %s, RHS: %s\n" testname lhs rhs))
    (agda2-go "cmd_compute"))

;; test negativity: -1 is minus one;

(defun agda2-tests-in-current-buffer ()
  (save-excursion
    (goto-char (point-min))
    (loop while (re-search-forward agda2-test-regexp nil t)
	  collect (list (match-string 1) (match-string 2) (match-string 3)))))

(defun agda2-clear-error-buffer ()
  (save-excursion
    (pop-to-buffer (get-buffer-create agda2-error-buffer-name) t)
    (delete-region (point-min) (point-max))))

(defun agda2-test-list (tests)
  (agda2-clear-error-buffer)
  (mapcar (lambda (args) (apply 'agda2-run-test args)) tests))


(defun agda2-test-all ()
  (interactive)
  (agda2-test-list (agda2-tests-in-current-buffer)))

(local-set-key "\C-ct" 'agda2-test-all)
