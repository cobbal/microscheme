(define the-continuation #f)
 
 (define (test)
   (let ((i 0))
     ; call/cc calls its first function argument, passing
     ; a continuation variable representing this point in
     ; the program as the argument to that function.
     ;
     ; In this case, the function argument assigns that
     ; continuation to the variable the-continuation.
     ;
     (call/cc (lambda (k) (set! the-continuation k)))
     ;
     ; The next time the-continuation is called, we start here.
     (set! i (+ i 1))
     i))