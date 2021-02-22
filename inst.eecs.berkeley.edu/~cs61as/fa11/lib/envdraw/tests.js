var assertEval = function(expr, expected) {
  evalInput(expr);
  var actual = results[results.length - 1];
  if (actual !== String(expected)) {
    console.error('Assertion failed: expected %o but got %o', expected, actual);
  }
}

silent = true;
console.group('Tests');
// console.groupCollapsed('Tests');

// Numbers
assertEval('0', 0);
assertEval('1', 1);
assertEval('-1', -1);
assertEval('1.0', 1.0);
assertEval('3.141592653589', 3.141592653589);
assertEval('123abc', '');

// Addition and subtraction
assertEval('(+ 1 2)', 3);
assertEval('(+ 1 2 3)', 6);
assertEval('(- 2 1)', 1);
assertEval('(- 9 3 2)', 4);
assertEval('(+ 1)', 1);
assertEval('(- 1)', -1);

// Multiplication
assertEval('(* 0 1)', 0);
assertEval('(* 2 10)', 20);

// Exponentiation
assertEval('(expt 1 10)', 1);
assertEval('(expt 2 4)', 16);
assertEval('(expt 10 5)', 100000);

// Numerical comparison
assertEval('(= 0 1)', '#f');
assertEval('(= 1 1)', '#t');
assertEval('(= 7 7 7 7 7)', '#t');
assertEval('(= 7 7 7 7 4)', '#f');
assertEval('(= 1)', '');

// Simple variable definitions
assertEval('(define a 1)', '');
assertEval('a', 1);
assertEval('(define b (+ 2 3))', '');
assertEval('b', 5);
assertEval('(define a -1)', '');
assertEval('a', -1);
assertEval('(define a a)', '');
assertEval('a', -1);
assertEval('(define b a)', '');
assertEval('b', -1);

// Variable mutation
assertEval('(set! a 2)', '');
assertEval('a', 2);
assertEval('(set! b (+ b 1))', '');
assertEval('b', 0);
assertEval('(set! z 100)', '');
assertEval('z', '');

// Lambdas
assertEval('((lambda () 1))', 1);
assertEval('((lambda (x) x) 10)', 10);
assertEval('((lambda (x y) (+ x y)) 1 2)', 3);
assertEval('(define allen (lambda (x y z) (- x (+ y z))))', '');
assertEval('(allen 1 2 3)', -4);

// Pairs and pair representations
assertEval('(cons 1 2)', "'(1 . 2)");
assertEval('(cons (cons 1 2) 3)', "'((1 . 2) . 3)");
assertEval('(cons 1 (cons 2 3))', "'(1 2 . 3)");
assertEval('(cons (cons (cons 1 2) 3) 4)', "'(((1 . 2) . 3) . 4)");
assertEval('(cons 1 (cons 2 (cons 3 4)))', "'(1 2 3 . 4)");

// car and cdr
assertEval('(car (cons 1 2))', 1);
assertEval('(cdr (cons 1 2))', 2);
assertEval('(cdr (cons 1 (cons 2 3)))', "'(2 . 3)");
assertEval('(car (cons (cons 1 2) 3))', "'(1 . 2)");

// cons special cases
assertEval('(cons)', '');
assertEval('(cons 1)', '');
assertEval('(cons 1 banana)', '');
assertEval('(define apple cons)', '')
assertEval('(apple (apple 1 2) 3)', "'((1 . 2) . 3)");

// null, null?, and cons structures with null
assertEval('null', "'()");
assertEval('(null? null)', '#t');
assertEval('(null? (list))', '#t');
assertEval('(null? (list 1))', '#f');
assertEval('(null? (list 1 2 3))', '#f');
assertEval('(cons null null)', "'(())");
assertEval('(cons 1 null)', "'(1)");
assertEval('(cons 1 (cons 2 (cons 3 (cons 4 null))))', "'(1 2 3 4)");
assertEval('(cons null 1)', "'(() . 1)");
assertEval('(cons null (cons null 1))', "'(() () . 1)");

// if
assertEval('(if 1 2 3)', 2);
assertEval('(if (= 1 1) 2 3)', 2);
assertEval('(if (= 1 0) 2 3)', 3);
assertEval('(if (= 1 1) (set! a 100) 3)', '');
assertEval('a', 100);
assertEval('(if (= 1 0) (set! a 200) 3)', 3);
assertEval('a', '100');

// Booleans and negation
assertEval('#f', '#f');
assertEval('#t', '#t');
assertEval('(not #t)', '#f');
assertEval('(not #f)', '#t');
assertEval('(not (= 1 2))', '#t');
assertEval('(not (= 2 2))', '#f');

// Procedures and environments
assertEval('(define (foo x) (define a 5) (+ x a))', '');
assertEval('(foo 13)', 18);
assertEval('a', 100);
assertEval('(define (fact n) (if (= n 1) 1 (* n (fact (- n 1)))))', '');
assertEval('(fact 10)', 3628800);
assertEval('((lambda (x y) ((lambda (y z) (+ y z)) 1 y)) 3 4)', 5);

// Dotted-tail notation
assertEval('(define (foo . args) args)', '');
assertEval('foo', '#&lt;procedure&gt;');
assertEval('(foo)', "'()");
assertEval('(foo 1)', "'(1)");
assertEval('(foo 1 2 3)', "'(1 2 3)");
assertEval('(define (foo2 x . args) x)', '');
assertEval('(foo2)', '');
assertEval('(foo2 1)', 1);
assertEval('(foo2 1 2 3)', 1);
assertEval('(define (foo3 x y . args) args)', '');
assertEval('(foo3 1 2 3)', "'(3)");
assertEval('(foo3 1 2 3 4)', "'(3 4)");

// Dotted-tail notation bad syntax
assertEval('(define (fob . . args) 1)', '');
assertEval('fob', '');
assertEval('(define (fod . args args2) 1)', '');
assertEval('fod', '');

// Lists
assertEval('(list)', "'()");
assertEval('(list 1)', "'(1)");
assertEval('(list 1 2 3)', "'(1 2 3)");
assertEval('(car (list 1 2 3))', 1);
assertEval('(cdr (list 1 2 3))', "'(2 3)");

// List length
assertEval('(length (list))', 0);
assertEval('(length (list 1))', 1);
assertEval('(length (list 1 2 3))', 3);

// List reversal
assertEval('(reverse (list))', "'()");
assertEval('(reverse (list 1))', "'(1)");
assertEval('(reverse (list 1 2))', "'(2 1)");
assertEval('(reverse (list 1 2 3))', "'(3 2 1)");

// map
assertEval('(map add1 (list))', "'()");
assertEval('(map add1 (list 1))', "'(2)");
assertEval('(map add1 (list 1 3 5))', "'(2 4 6)");
assertEval('(map (lambda (x) (cons x (+ x 1))) (list 1 3 5))', "'((1 . 2) (3 . 4) (5 . 6))");

// cond
assertEval('(cond ((= 1 1) 5))', '5');
assertEval('(cond ((= 1 1) 10) (else 3))', '10');
assertEval('(cond ((= 1 2) 1) (else 3))', '3');
assertEval('(cond ((= 1 2) 1) ((= 2 3) 2) (else 3))', '3');
assertEval('((lambda (x) (cond ((= x 1) 2) ((= x 3) 4) (#f 5) (else 7))) 1)', '2');
assertEval('((lambda (x) (cond ((= x 1) 2) ((= x 3) 4) (#f 5) (else 7))) 3)', '4');
assertEval('((lambda (x) (cond ((= x 1) 2) ((= x 3) 4) (#f 5) (else 7))) 11)', '7');

// let
assertEval('(let () (+ 1 2))', '3');
assertEval('(let ((a 3)) (+ 1 2) (+ a 4))', '7');
assertEval('((let ((a 1) (b 2)) (lambda (x) (+ x a b))) 11)', '14');

silent = false;
console.groupEnd();
