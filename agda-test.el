(require 'cl) ;; agda2-mode does anyway, because haskell-indent does anyway...

(defvar agda2-test-buffer-name "*Agda test results*"
  "The name of the buffer that contains the results of Agda unit tests.")
(defvar agda2-information-buffer-name "*Agda information*"
  "The True Name of the buffer to which the Agda interpreter sends its output.")
(defvar agda2-test-regexp
  "test\\s-*\\(.*?\\)\\s-*:\\s-*\\(.*?\\)\\s-+is\\s-+\\(.*\\);"
  "Regexp to find test expressions in Agda files")

(defun agda2-normalise-string (expr)
  "Returns the normal form of the Agda expression EXPR.

The authors of agda2-mode, in their infinite mercy and wisdom,
implemented agda2-compute-normalised by having GHCi write elisp
code to insert the normalised value into a buffer, which
agda2-compute-normalised then executes. Hence, the easiest way to
get at the normalised value of an Agda expression is to call
agda2-compute-normalised and then inspect the *Agda information*
buffer. [It may look like you need to inspect the *Normal Form*
buffer, but your minibuffer is full of lies. Trust me on this.]
An unfortunate consequence is that calling this function will
blow away anything in your *Agda information* buffer."
  (agda2-compute-normalised-toplevel (substring-no-properties expr))
  (with-current-buffer agda2-information-buffer-name
    (buffer-substring-no-properties (point-min) (point-max))))

(defun agda2-run-test (testname lhs rhs)
  "Test to see if the Agda expressions LHS and RHS have the same normal form.
The name of the test for reporting purposes is TESTNAME.
Output will be placed in the buffer `agda2-test-buffer-name'."
  (let ((lhsval (agda2-normalise-string lhs))
	(rhsval (agda2-normalise-string rhs)))
    (with-current-buffer agda2-test-buffer-name
      (insert (format "Testname: %s, LHS: %s, RHS: %s\n" testname lhs rhs))
      (insert
       (if (string-equal lhsval rhsval)
	   (format "Success! LHS=%s, RHS=%s\n" lhsval rhsval)
	 (format "Failure: LHS=%s, RHS=%s\n" lhsval rhsval))))))

(defun agda2-tests-in-current-buffer ()
  "Find all the Agda unit tests in the current buffer. Returns a
list of (NAME ACTUAL EXPECTED) triples."
  (save-excursion
    (goto-char (point-min))
    (loop while (re-search-forward agda2-test-regexp nil t)
	  collect (list (match-string 1) (match-string 2) (match-string 3)))))

(defun agda2-clear-test-buffer ()
  "Clear the Agda test result buffer (whose name is given by
`agda2-test-buffer-name')."
  (save-excursion
    (pop-to-buffer (get-buffer-create agda2-test-buffer-name) t)
    (delete-region (point-min) (point-max))))

(defun agda2-test-list (tests)
  "Run a list of Agda unit tests, passed as the argument TESTS in
the form of a list of (TESTNAME ACTUAL EXPECTED) triples."
  (agda2-clear-test-buffer)
  (mapcar (lambda (args) (apply 'agda2-run-test args)) tests))

(defun agda2-test-all ()
  "Run all the Agda unit tests in the current buffer."
  (interactive)
  (agda2-test-list (agda2-tests-in-current-buffer)))

(defun agda2-install-test-keybindings ()
  "Install keybindings for running Agda unit tests."
  (interactive)
  (local-set-key "\C-ct" 'agda2-test-all))

(add-hook 'agda2-mode-hook 'agda2-install-test-keybindings)
