# Alphalang


execution example
```
$ cd alphalang
$ sbt
> run "(((lambda (f) ((lambda (p) (f (lambda (a) ((p p) a)))) (lambda (p) (f (lambda (a) ((p p) a)))))) (lambda (fib) (lambda (n) (if (< n 1) 1 (+ (fib (- n 1)) (fib (- n 2))))))) 10)"
[info] Running example.AlphaLang (((lambda (f) ((lambda (p) (f (lambda (a) ((p p) a)))) (lambda (p) (f (lambda (a) ((p p) a)))))) (lambda (fib) (lambda (n) (if (< n 1) 1 (+ (fib (- n 1)) (fib (- n 2))))))) 10)
source code:(((lambda (f) ((lambda (p) (f (lambda (a) ((p p) a)))) (lambda (p) (f (lambda (a) ((p p) a)))))) (lambda (fib) (lambda (n) (if (< n 1) 1 (+ (fib (- n 1)) (fib (- n 2))))))) 10)
parsed tree:Form(Form((lambda ( List(symbol:f) ) Form((lambda ( List(symbol:p) ) Form(symbol:fList( (lambda ( List(symbol:a) ) Form(Form(symbol:pList( symbol:p))List( symbol:a))))))List( (lambda ( List(symbol:p) ) Form(symbol:fList( (lambda ( List(symbol:a) ) Form(Form(symbol:pList( symbol:p))List( symbol:a)))))))))List( (lambda ( List(symbol:fib) ) (lambda ( List(symbol:n) ) if Form(symbol:<List( symbol:n,  literal:1)) then literal:1 else Form(symbol:+List( Form(symbol:fibList( Form(symbol:-List( symbol:n,  literal:1)))),  Form(symbol:fibList( Form(symbol:-List( symbol:n,  literal:2))))))))))List( literal:10))
literal:144
[success] Total time: 0 s, completed 2017/08/16 20:53:25
```
