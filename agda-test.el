;;; agda-test.el --- Commands for running Agda unit tests

;;; Commentary:
;; You may well be wondering why anyone would want to write unit tests
;; for a dependently-typed language.  Couldn't I just express my
;; constraints in the type system? Well, maybe I'll be able to do this
;; eventually - it certainly sounds like a nice thing to be able to
;; do, and it's for that reason I'm learning Agda.  But I'm not there
;; yet, and until I get there (and possibly even after...) I'm going
;; to need to write tests so I can be sure that my code really does
;; mean what I meant to tell it to mean.
;;
;; You can add tests to a file by embedding comments of the form
;;
;;     {- test TESTNAME: ACTUAL is EXPECTED -}
;;
;; in your Agda source code.  For instance:
;;
;;     {- test 2+1: (suc (suc zero)) +N (suc zero) is (suc (suc (suc zero)))
;;
;; When you invoke `agda2-test-all', you should hopefully get a cheery
;; little message saying
;;
;; 1..1
;; ok 1 - 2+1
;;
;; Test output uses the Test Anything Protocol (see
;; http://testanything.org), for which many aggregating and reporting
;; tools already exist.
;;
;; Since, as I understand things, there is no agreement about how best
;; to model equality in dependently typed languages, I've punted on
;; the problem and gone for the Simplest Thing That Could Possibly
;; Work: string equality of normal forms.
;;
;; This code was written for Conor McBride's class ``Dependently-Typed
;; programming with Agda'' at Edinburgh University in early 2011.

;;; History:
;; 2011-02-03 First version by Miles Gould and Aaron Crane.

(require 'cl) ;; agda2-mode does anyway, because haskell-indent does anyway...

;;; Code:

(defvar agda2-test-buffer-name "*Agda test results*"
  "The name of the buffer that contains the results of Agda unit tests.")

(defvar agda2-information-buffer-name "*Agda information*"
  "The True Name of the buffer to which the Agda interpreter sends its output.")

(defvar agda2-test-regexp
  "test\\s-*\\(.*?\\)\\s-*:\\s-*\\(.*?\\)\\s-+is\\s-+\\(.*\\);"
  "Regexp to find test expressions in Agda files.")

(defun agda2-normalise-string (expr)
  "Return the normal form of the Agda expression EXPR.

The authors of `agda2-mode', in their infinite mercy and wisdom,
implemented `agda2-compute-normalised' by having GHCi write elisp
code to insert the normalised value into a buffer.  This code is
passed back to `agda2-compute-normalised', which then executes
it.  Hence, the easiest way to get at the normalised value of an
Agda expression is to call `agda2-compute-normalised' and then
inspect the *Agda information* buffer.  [It may look like you need
to inspect the *Normal Form* buffer, but your minibuffer is full
of lies.  Trust me on this.]  An unfortunate consequence is that
calling this function will blow away anything in your *Agda
information* buffer."
  (agda2-compute-normalised-toplevel (substring-no-properties expr))
  (with-current-buffer agda2-information-buffer-name
    (buffer-substring-no-properties (point-min) (point-max))))

(defun agda2-run-test (testname lhs rhs &optional testnum)
  "Run a single Agda unit test.
The name of the test for reporting purposes is TESTNAME.  The
test succeeds if the Agda expressions LHS and RHS have the same
normal form (determined by string equality).  If TESTNUM is
given, the test is treated as the TESTNUM'th test in the current
run, otherwise it is treated as the first.  Output will be placed
in the buffer `agda2-test-buffer-name'."
  (let ((lhsval (agda2-normalise-string lhs))
        (rhsval (agda2-normalise-string rhs))
        (test-number (if testnum testnum 1)))
    (with-current-buffer agda2-test-buffer-name
      (insert
       (if (string-equal lhsval rhsval)
           (format "ok %d - %s\n" test-number testname)
         (format "not ok %d - %s\n    got %s\n    expected %s\n"
                 test-number testname lhsval rhsval))))))

(defun agda2-tests-in-current-buffer ()
  "Find all the Agda unit tests in the current buffer.
Returns a list of (TESTNAME ACTUAL EXPECTED TESTNUM) quads."
  (save-excursion
    (goto-char (point-min))
    (loop while (re-search-forward agda2-test-regexp nil t)
          for num from 1
          collect (list (match-string 1) (match-string 2) (match-string 3) num))))

(defun agda2-clear-test-buffer ()
  "Clear the Agda test result buffer.
The name of the test result buffer is given by `agda2-test-buffer-name'."
  (save-excursion
    (pop-to-buffer (get-buffer-create agda2-test-buffer-name) t)
    (delete-region (point-min) (point-max))))

(defun agda2-test-list (tests)
  "Run a list of Agda unit tests.
The list of tests is passed as the argument TESTS in the form of
a list of (TESTNAME ACTUAL EXPECTED TESTNUM) quads."
  (agda2-clear-test-buffer)
  (mapcar (lambda (args) (apply 'agda2-run-test args)) tests))

(defun agda2-test-all ()
  "Run all the Agda unit tests in the current buffer.
Tests are strings of the form `test TESTNAME: EXPECTED is
ACTUAL;', where TESTNAME is the name of the test (used for
reporting), and EXPECTED and ACTUAL are Agda expressions which
are expected to have the same normal form (determined by string
equality).  All three of TESTNAME, EXPECTED and ACTUAL may
include spaces.  You can embed tests in comments or TeX code."
  (interactive)
  (agda2-test-list (agda2-tests-in-current-buffer)))

(defun agda2-install-test-keybindings ()
  "Install keybindings for running Agda unit tests."
  (interactive)
  (local-set-key "\C-ct" 'agda2-test-all))

(add-hook 'agda2-mode-hook 'agda2-install-test-keybindings)

(provide 'agda-test)

;;; agda-test.el ends here
