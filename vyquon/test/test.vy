; Set necessary symbols
(setvar 'nil 'nil)
(setvar 'false 'false)
(setvar 'true 'true)

; Is this a number?
(setvar 'number? 
    (fn (x)
        (or (int? x) (float? x))))

;; Problem one
(setvar 'average-2 
    (fn (a b)
        (if (and (number? a) (number? b))
           (/ (+ a b) 2)
           nil)))

(setvar 'second
    (fn (x) (first (rest x))))

(setvar '1-
    (fn (x) (- x 1)))

(setvar 'remove-if-not
    (fn (predicate lst)
        (if lst
            (if (predicate (first lst)) 
                (pair (first lst) (remove-if-not predicate (rest lst)))
                (remove-if-not predicate (rest lst))) )))

;; Problem two
(setvar 'average-r
    (fn (lst)
        (setvar 'only-numeric (remove-if-not number? lst))
        (if (not only-numeric) nil
            (if (= (length only-numeric) 2) (average-2 (first only-numeric) (second only-numeric))

                (/ (+ (first only-numeric) (* (1- (length only-numeric)) (average-r (rest only-numeric)))) (length only-numeric))))))

;; Problem three
(setvar 'last
    (fn (lst)
        (if (not (rest lst))
            (first lst)
            (last (rest lst)))))

(setvar 'empty?
    (fn (lst) (not lst)))

;; Problem four
(setvar 'member?
    (fn (item lst)
         (if (empty? lst)
            false
            (if (is item (first lst))
                true
                (member? item (rest lst))))))

;; Problem five
(setvar 'count-x-in-y 
    (fn (item lst)
        (if (empty? lst)
            0
            (if (is item (first lst))
                (+ 1 (count-x-in-y item (rest lst)))
                (count-x-in-y item (rest lst))))))

(setvar 'my-length
    (fn (lst)
        (if (empty? lst) 
            0
            (+ 1 (my-length (rest lst))))))
