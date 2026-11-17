;; * My Emacs Utility Functions -*- lexical-binding: t; -*-

;; ** Requires
;; cl-lib and ert are now part of standard gnu emacs,
;; you just need to import their symbols.
(require 'cl-lib)
(require 'ert)

;; ** Places as First-Class Values

(defmacro place (aplace)
  "create a function value from a generalized place expression which can be used to set or get the value at that place"
  (gv-letplace (getter setter) aplace
    `(lambda (&rest new-val)
       (if new-val
	   ,(funcall setter '(car new-val))
	 ,getter ) ) ) )

(ert-deftest place-test ()
  (let ( (ref1 (let ( (var 0) ) (place var)))
	 (ref2 (let ( (var 0) ) (place var))) )
    (should (= (funcall ref1) 0))
    (should (= (funcall ref2) 0))
    (funcall ref1 1)
    (funcall ref2 -1)
    (should (= (funcall ref1) 1))
    (should (= (funcall ref2) -1))
    ) )


;; ** (condense-context) required for (my)

;; We expect context to express some dynamic context for a debugging
;; or error report, from the &rest argument of some function.
;; We expect the first element to be a context list for that function
;; with possible additional elements providing names and/or values
;; giving more context.  We want the context to be in the form of a
;; singleton list we can incorporate as a single value in a format message.
;; ==> Transform context into a singleton list if it isn't one already!
(cl-defun condense-context (context)
  "return any context as a singleton by wrapping it and/or folding its cdr elements into its car"
  (cond
   ;; return an empty list as such
   ( (null context) context )
   ;; if context is not a list, return it wrapped!
   ( (atom context) (list context) )
   ;; return a singleton as such
   ( (and (consp context) (atom (cdr context))) context )
   ( t ; otherwise, first fold it's cdr into its car
     (let ( (context (append (car context) (cdr context))) )
       ;; unless it's a singleton now, wrap it into one!
       (if (and (consp context) (atom (cdr context)))
	   context
	 (list context) ) ) ) ) )

(condense-context nil)
(condense-context '("org-capture.el"))
(condense-context '("org-capture.el" some-function))
(condense-context '(("org-capture.el" some-function) 'var-name))

;; ** (my) needs (contense-context)

;; version with workaround for Common Lisp &rest &key infelicity
(cl-defun my (type-predicate value &rest context &key (error 'error))
  "like cl-the but with a better error message"
  (if (funcall type-predicate value)
      value
    ;; Remove &key arguments from front of &rest arguments,
    ;; workaround for Common Lisp infelicity.
    (while (and (consp context) (memq (car context) '(:error)))
      (pop context) (pop context) )
    (let ( (message "expected type %S but got %S")
	   (context (condense-context context)) ; updated context
	   (args (list type-predicate value)) )
      ;; update message and args if context non-null
      (let ( (message (if (null context) message (concat "In %S " message)))
	     (args (if (null context) args (cons context args))) )
	;; throw an error with the current message and any contextI included
	(apply error (cons message args)) ) ) ) )

;; (cl-defun my (type-predicate value &rest context &key (error 'error))
;;   "like cl-the but with a better error message"
;;   ;; Remove &key arguments from front of &rest arguments,
;;   ;; workaround for Common Lisp infelicity.
;;   (while (and (consp context) (memq (car context) '(:error)))
;;     (pop context) (pop context) )
;;   (if (funcall type-predicate value)
;;       value
;;     ;; BEGIN workaround for cl-defun &key brokenness!!
;;     ;; if context starts with overriding error, pull out new-error
;;     (let ( (new-error (and (>= (length context) 2)
;; 			   (eq :error (cl-first context))
;; 			   (symbolp (cl-second context))
;; 			   (symbol-function (cl-second context))
;; 			   (cl-second context) )) )
;;       ;; update error and context appropriately
;;       (let ( (error (or new-error 'error))
;; 	     (context (if new-error (cddr context) context)) )
;; 	;; END workaround
;; 	(let ( (message "expected type %S but got %S")
;; 	       (context (condense-context context)) ; updated context
;; 	       (args (list type-predicate value)) )
;; 	  ;; update message and args if context non-null
;; 	  (let ( (message (if (null context) message (concat "In %S " message)))
;; 		 (args (if (null context) args (cons context args))) )
;; 	    ;; throw an error with the current message and any contextI included
;; 	    (apply error (cons message args)) ) ) ) ) ) )

(ert-deftest my-test ()
  (should (eq 'hello (my 'symbolp 'hello)))
  
  (should-error (my 'stringp 'hello))
  (should-error (my 'stringp 'hello "org-capture.el"))
  (should-error (my 'stringp 'hello "org-capture.el" 'funny))
  (should-error (my 'stringp 'hello '("org-capture.el" funny)))
  (should-error (my 'stringp 'hello '("org-capture.el" funny) 'foo))
  (should-error (my 'stringp 'hello '("org-capture.el" funny) 'foo 5 'bar))

  ;; but what message to all of those errors display?
  (should (equal (my 'stringp 'hello :error 'format)
		 "expected type stringp but got hello" ))
  (should (equal (my 'stringp 'hello :error 'format "org-capture.el")
		 "In (\"org-capture.el\") expected type stringp but got hello" ))
  (should (equal (my 'stringp 'hello :error 'format "org-capture.el" 'funny)
		 "In ((\"org-capture.el\" funny)) expected type stringp but got hello" ))
  (should (equal (my 'stringp 'hello :error 'format '("org-capture.el" funny) 'foo)
		 "In ((\"org-capture.el\" funny foo)) expected type stringp but got hello" ))
  (should (equal (my 'stringp 'hello :error 'format '("org-capture.el" funny) 'foo 5 'bar)
		 "In ((\"org-capture.el\" funny foo 5 bar)) expected type stringp but got hello" ))  )

;; ** (enlist) - enlist if all non-nil, error otherwise!

;; I'm very proud of this function!
(cl-defun non-null-enlist (names &rest items)
  "return list of items, all must be non-nil; otherwise return nil and complain about nil items by name"
  (if (cl-notany #'null items)
      items
    (error "%s error: expected all %S non-nil in %S"
	   'non-null-enlist names items) ) )

;; ** File and Directory Testing Functions

(defun file-true-attributes (f)
  "returns attributes of file's truename or nil"
  (file-attributes (file-truename f)) )

(defun file-exists? (f)
  "file-true-attributes not nil"
  (file-true-attributes f) )

(defun directory-exists? (d)
  "ninth element of file-true-attributes begins with d - not portable!!"
  (let ( (attrs (file-true-attributes d)) )
    (and attrs (= (aref (cl-ninth attrs) 0) ?d)) ) )

(ert-deftest directory-exists?-org-directory ()
  (should (directory-exists? org-directory)) )
;; ** Workaround for cl-defun conflict between &key and &rest args

;; Common Lisp mis-feature
;; - Arguments bound by &key will also be part of the &rest list!!

;; Possible Workarounds
;; 1. Remove &key arguments from &rest arguments
;;  (dolist ( key '(:k1 :k2) ) (cl-remf args key))
;; 2. Require that key arguments be first in &rest, then
;; (while (and (consp args) (memq (car args) '(:k1 :k2))) (pop args) (pop args))

;; *** Experiments

;; This doesn't work: Why??
(cl-defun kludge1 (arg1 &rest rest-args &key (key 'key-default))
  (dolist ( key '(:key) ) (cl-remf rest-args key))
  (message "key %s rest-args %S" key rest-args) )

(kludge1 'hello :key 'key-value "org-capture.el")

;; Error Message, also got a long *Backtrace*
;; (error "Keyword argument \"org-capture.el\" not one of (:key)")

(kludge1 'hello "org-capture.el" :key 'key-value)

;; (error "Keyword argument \"org-capture.el\" not one of (:key)")
;; Error Message, also got a long *Backtrace*

;; What we have to work with
(cl-defun kludge2 (arg1 &rest rest-args)
  (message "rest-args %S" rest-args) )

(kludge2 'hello :key 'key-value "org-capture.el")

;; Message Result:
;; rest-args (:key key-value "org-capture.el")

(cl-defun kludge3 (arg1 &rest rest-args)
  (let ( (key (or (and (consp rest-args) (eq (car rest-args) :key)
			 (progn (pop rest-args) (pop rest-args)))
		    'key-default )) )
    (message "key %s rest-args %S" key rest-args) ) )

(kludge3 'hello :key 'key-value "org-capture.el")

;; *** pop-alist - a function now, part of a macro later?

;; we should evolve cl-defun into my-defun which is better
;; - fix &rest &key clash
;; - add optional type checking

;; lst is a list which might begin with an association list,
;; keys is a list of (either keywordp (cons-of keywordp any))
;; where the any value is interpreted as a default:
;; destructively pull out from lst any initial alist where
;; the alist keys match the keys in keys, returning that alist
;; nconc'ed with the tail of any elements of keys which have
;; a default value.
(cl-defun pop-alist--keys (keys key-accum alist-accum &key (context '(pop-alist--keys)))
  (cond ( (null keys) (cons key-accum alist-accum) )
	( (atom keys) (error "%S error: expected keys list, got %s" context keys) )
	( (and (consp (car keys)) (keywordp (caar keys)))
	  (pop-alist--keys (cdr keys)
			  (cons (caar keys) key-accum)
			  (cons (car keys) alist-accum) ) )
	( (keywordp (car keys))
	  (pop-alist--keys (cdr keys) (cons (car keys) key-accum) alist-accum) )
	( t (error "%S error: expected non-keyword in keys list, %s" context ( car keys)) ) ) )

(pop-alist--keys '(:a (:b b0) :c (:d d0)) nil nil)

(pop-alist--keys '(:a (:b b0) :c (:d d0)) nil nil :context '("my-lib.el" test))

(ert-deftest ert--pop-alist--keys-1 ()
  (should (equal
	   (pop-alist--keys '(:a (:b b0) :c (:d d0)) nil nil)
	   '((:d :c :b :a) (:d d0) (:b b0)) )) )

;; Split a list into an initial alist
;; - where the keys are part of the alist-spec
;; and the rest of the list.  Return of cons of
;; the latter and the former.
(defun pop-alist (lst alist-spec)
  (let ( (context (list "my-lib.el" 'pop-alist)) )
    (let ( (keys+alist (pop-alist--keys alist-spec nil nil :context context)) )
      (let ( (keys (car keys+alist))
	     (alist-accum (cdr keys+alist)) )
	(while (and (consp lst) (keywordp (car lst)) (memq (car lst) keys))
	  (let ( (key (pop lst)) )
	    (let ( (val (pop lst)) )
	      (push (cons key val) alist-accum) ) ) )
	(cons lst alist-accum) ) ) ) )

(ert-deftest ert--pop-alist-1 ()
  (should 
   (equal 
    (let ( (alist-spec '(:a (:b b0) :c (:d d0))) )
      (let ( (lst '(:a aa :d dd foo bar)) )
	(pop-alist lst alist-spec)) )
    '((foo bar) (:d . dd) (:a . aa) (:d d0) (:b b0)) ) ) )

;; keywords :foo :bar
(cl-defun kludge4 (arg1 &rest rest-args)
  (let* ( (keys-spec '(:foo (:bar 'none)))
	  (rest+alist (pop-alist rest-args keys-spec))
	  (rest (car keys-spec))
	  (keys-alist (cdr keys-spec))
	  (foo (assq :foo keys-alist))
	  (bar (assq :foo keys-alist)) )
    (format "arg1 %S foo %S bar %S rest %S\n" arg1 foo bar rest) ) )

;; Doesn't yet work!!!
(kludge4 'hello 'more 'stuff)
;; Result: (kludge4 'hello :foo 'foo!)

