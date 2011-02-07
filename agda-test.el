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
;; You can add test cases to a file by embedding comments of the form
;;
;;     {- test TESTNAME: ACTUAL is EXPECTED; -}
;;
;; in your Agda source code.  For instance:
;;
;;     {- test 1+1: (suc zero) +N (suc zero) is (suc (suc zero)); -}
;;
;; When you invoke `agda2-test-run', you should hopefully get a cheery
;; little message saying
;;
;; 1..1
;; ok 1 - 1+1
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
(require 'button)
(require 'agda2-mode)

;;; Code:

(defvar agda2-test-buffer-name "*Agda test results*"
  "The name of the buffer that contains the results of Agda unit tests.")

(defvar agda2-test-information-buffer-name "*Agda information*"
  "The True Name of the buffer to which the Agda interpreter sends its output.")

(defvar agda2-test-regexp
  "\\_<test\\s-+\\(.*?\\)\\s-*:\\s-*\\(.*?\\)\\s-+is\\s-+\\(.*?\\)\\s-*;"
  "Regexp to find test cases in Agda files.")

(defun agda2-test-normalise-string (expr)
  "Return the normal form of the Agda expression EXPR.

The authors of `agda2-mode', in their infinite mercy and wisdom,
implemented `agda2-compute-normalised' by having GHCi write elisp
code to insert the normalised value into a buffer.  This code is
passed back to `agda2-compute-normalised', which then executes
it.  Hence, the easiest way to get at the normalised value of an
Agda expression is to call `agda2-compute-normalised' and then
inspect the *Agda information* buffer.  [It may look like you
need to inspect the *Normal Form* buffer, but your modeline is
full of lies.  Trust me on this.]  An unfortunate consequence is
that calling this function will blow away anything in your *Agda
information* buffer."
  (agda2-compute-normalised-toplevel (substring-no-properties expr))
  (with-current-buffer agda2-test-information-buffer-name
    (buffer-substring-no-properties (point-min) (point-max))))

(defun agda2-test-tap-button-invoke (button)
  "Jump from a test case's TAP output to its source"
  (let ((source (button-get button 'agda2-test-source)))
    (with-current-buffer (get-buffer-create "frooble")
      (insert (format "source: %s\n" source))))
  (let ((source (or (button-get button 'agda2-test-source)
                    (error "This test's source is unknown"))))
    (switch-to-buffer (cdr source))
    (goto-char (car source))))

(defun agda2-test-run-case (testnum testname lhs rhs &optional pos)
  "Run a single Agda unit test.
The name of the test for reporting purposes is TESTNAME.  The
test succeeds if the Agda expressions LHS and RHS have the same
normal form (determined by string equality).  The test is treated
as the TESTNUM'th test in the current run.  Output will be placed
in the buffer `agda2-test-buffer-name'."
  (let ((lhsval (agda2-test-normalise-string lhs))
        (rhsval (agda2-test-normalise-string rhs))
        (buffer (current-buffer)))
    (with-current-buffer agda2-test-buffer-name
      (let* ((ok (string-equal lhsval rhsval))
             (label (concat (if ok "" "not ")
                            (format "ok %d - %s" testnum testname)))
             (tail (if ok "\n"
                     (format "\n    got %s\n    expected %s\n" lhsval rhsval))))
        (if (null pos)
            (insert label)
          (insert-text-button
           label
           'action 'agda2-test-tap-button-invoke
           'face (if ok 'default 'font-lock-warning-face)
           'follow-link t
           'help-echo "RET or click to jump to this test's source"
           'agda2-test-source (cons pos buffer)))
        (insert tail)))))

(defun agda2-test-find-next-case (&optional limit)
  "Find the next Agda unit test in the current buffer.
Returns nil if none is found.
The optional second argument is a buffer position; if supplied,
the case must not extend beyond that position.
On exit, the regexp match data reflect the case found."
  (and (re-search-forward agda2-test-regexp limit t)
       (list (match-string 1) (match-string 2) (match-string 3)
             (match-beginning 0))))

(defun agda2-test-find-region (start end)
  "Find all the Agda unit tests in the current buffer.
Returns a list of (TESTNAME ACTUAL EXPECTED) triples."
  (save-excursion
    (goto-char start)
    (loop for case = (agda2-test-find-next-case end)
          while case
          collect case)))

(defun agda2-test-find-near-point ()
  "Find a test case near point.
Uses the last case that ends on the same line as point, or the
next in the buffer otherwise; or throws an error if no suitable
case is found."
  (let ((original-eol (point-at-eol))
        case)
    (save-excursion
      (goto-char (point-min))
      (loop for c = (agda2-test-find-next-case original-eol)
            while c
            if (= (point-at-eol) original-eol) do (setq case c))
      (list (or case
                (agda2-test-find-next-case)
                (error "No test case found near point"))))))

(defun agda2-test-clear-buffer ()
  "Clear the Agda test result buffer.
The name of the test result buffer is given by `agda2-test-buffer-name'."
  (save-excursion
    (pop-to-buffer (get-buffer-create agda2-test-buffer-name) t)
    (delete-region (point-min) (point-max))))

(defun agda2-test-prove (tests)
  "Run a list of Agda unit tests.
The list of tests is passed as the argument TESTS in the form of
a list of (TESTNAME ACTUAL EXPECTED) triples."
  (agda2-test-clear-buffer)
  (or tests (error "No tests to run"))
  (with-current-buffer agda2-test-buffer-name
    (insert (format "1..%d\n" (length tests))))
  (loop for num from 1
        for case in tests
        do (apply 'agda2-test-run-case num case)))

(defun agda2-test-run-all ()
  "Run all Agda unit tests in the current buffer.
Tests are strings of the form `test TESTNAME: EXPECTED is ACTUAL;',
where TESTNAME is the name of the test (used for reporting), and
EXPECTED and ACTUAL are Agda expressions which are expected to
have the same normal form (determined by string equality).  All
three of TESTNAME, EXPECTED and ACTUAL may include spaces.  You
can embed tests in comments or TeX code."
  (interactive)
  (agda2-test-prove (agda2-test-find-region (point-min) (point-max))))

(defun agda2-test-run-one ()
  "Run an Agda unit test that's nearby in the current buffer.
It chooses the last test which ends on the same line as point, or
the next test in the buffer if there is none, or throws an error
otherwise.

See `agda2-test-run-all' for documentation of how to specify tests."
  (interactive)
  (agda2-test-prove (agda2-test-find-near-point)))

(defun agda2-test-run-region (start end)
  "Run all the Agda unit tests in the current region.
See `agda2-test-run-all' for documentation of how to specify tests."
  (interactive "r")
  (agda2-test-prove (agda2-test-find-region start end)))

(loop for (k f) in '(("\C-v" agda2-test-run-one)
                     ("\C-r" agda2-test-run-region)
                     ("\C-a" agda2-test-run-all))
      do (define-key agda2-mode-map (concat "\C-c\C-v" k) f)) ; mnemonic: verify

(provide 'agda-test)

;;; agda-test.el ends here
