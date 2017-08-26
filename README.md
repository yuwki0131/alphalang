# Alphalang

yet yet another lisp-like programming language

### execution example

```
$ cd alphalang
$ sbt
> > run "(((lambda (f) ((lambda (p) (f (lambda (a) ((p p) a)))) (lambda (p) (f (lambda (a) ((p p) a)))))) (lambda (fib) (lambda (n) (if (= n 0) 0 (if (= n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))))) 10)"
[info] Running example.AlphaLang (((lambda (f) ((lambda (p) (f (lambda (a) ((p p) a)))) (lambda (p) (f (lambda (a) ((p p) a)))))) (lambda (fib) (lambda (n) (if (= n 0) 0 (if (= n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))))) 10)
source code:(((lambda (f) ((lambda (p) (f (lambda (a) ((p p) a)))) (lambda (p) (f (lambda (a) ((p p) a)))))) (lambda (fib) (lambda (n) (if (= n 0) 0 (if (= n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))))) 10)
parsed tree:Form(Form((lambda ( List(symbol:f) ) Form((lambda ( List(symbol:p) ) Form(symbol:fList( (lambda ( List(symbol:a) ) Form(Form(symbol:pList( symbol:p))List( symbol:a))))))List( (lambda ( List(symbol:p) ) Form(symbol:fList( (lambda ( List(symbol:a) ) Form(Form(symbol:pList( symbol:p))List( symbol:a)))))))))List( (lambda ( List(symbol:fib) ) (lambda ( List(symbol:n) ) if Form(symbol:=List( symbol:n,  literal:0)) then literal:0 else if Form(symbol:=List( symbol:n,  literal:1)) then literal:1 else Form(symbol:+List( Form(symbol:fibList( Form(symbol:-List( symbol:n,  literal:1)))),  Form(symbol:fibList( Form(symbol:-List( symbol:n,  literal:2))))))))))List( literal:10))
literal:55
```

## Sample Programs

### plus

```
(+ 1 2)
```

### Fibonacci number

```
(((lambda (f) ((lambda (p) (f (lambda (a) ((p p) a)))) (lambda (p) (f (lambda (a) ((p p) a)))))) (lambda (fib) (lambda (n) (if (= n 0) 0 (if (= n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))))) 10)
```

## milestone

TODO: Add retrec

TODO: Add define

TODO: Add List

TODO: Add Map

TODO: Add Set

TODO: Add String

TODO: Add Testcode

TODO: Add do
