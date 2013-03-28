(use sxpath)

(define b52s
  '(rock-lobster
     (@ (a 22) (b 33) (x 1111))
     (love-shack
       (a-little-place "where we can get together")
       (baby))
     (deadbeat-club
       (@ (get "a job?") (im "trying to think")))))

(define att-list (cadr b52s))

(define love-shack (caddr b52s))

(define deadbeat (cadddr b52s))

(define exp0 (sxpath '(@)))

(define exp1 (sxpath '(/ (@))))

(define exp2 (sxpath '((@))))

(define exp3 (sxpath '((/ (@)))))

(define exp4 (sxpath '(* (@))))

(define exp5 (sxpath '((* (@)))))

(define exp6 (sxpath '(*any*)))

(define exp7 (sxpath '(*attribute*)))

(define exp8 (sxpath '(*attr*)))

(define exp9 (sxpath '(@ *)))
