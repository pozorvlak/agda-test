(require 'cl)

(defvar agda2-error-buffer-name "*Agda errors*")
(defvar agda2-information-buffer-name "*Agda information*")
(defvar agda2-test-regexp
  "test\\s-*\\(.*?\\)\\s-*:\\s-*\\(.*?\\)\\s-+is\\s-+\\(.*\\);")

(defun agda2-normalise-string (expr)
  "The authors of agda2-mode, in their infinite mercy and wisdom,
  implemented agda2-compute-normalised by having GHCi write elisp
  code to insert the normalised value into a buffer, which
  agda2-compute-normalised then executes. Hence, the easiest way
  to get at the normalised value of an Agda expression is to call
  agda2-compute-normalised and then inspect the *Agda
  information* buffer. It may look like you need to inspect the
  *Normal Form* buffer, but your minibuffer is full of
  lies. Trust me on this."
  (agda2-compute-normalised-toplevel (substring-no-properties expr))
  (with-current-buffer agda2-information-buffer-name
    (buffer-substring-no-properties (point-min) (point-max))))

(defun agda2-run-test (testname lhs rhs)
  (let ((lhsval (agda2-normalise-string lhs))
	(rhsval (agda2-normalise-string rhs)))
    (with-current-buffer agda2-error-buffer-name
      (insert (format "Testname: %s, LHS: %s, RHS: %s\n" testname lhs rhs))
      (insert
       (if (string-equal lhsval rhsval)
	   (format "Success! LHS=%s, RHS=%s\n" lhsval rhsval)
	 (format "Failure: LHS=%s, RHS=%s\n" lhsval rhsval))))))

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

(defun agda2-install-test-keybindings ()
  (interactive)
  (local-set-key "\C-ct" 'agda2-test-all))

(add-hook 'agda2-mode-hook 'agda2-install-test-keybindings)
