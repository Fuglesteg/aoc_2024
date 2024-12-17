(defvar *example*
"190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")

(defun parse (input)
  (loop for line in (remove "" (split-string input (string #\Newline)) :test #'string=)
        collect (destructuring-bind (sum values) (split-string line ": ")
                  (cons (parse-integer sum)
                        (mapcar #'parse-integer (split-string values " "))))))

(defun combinations (&rest lists)
  (if (endp lists)
      (list nil)
      (mapcan (lambda (inner-val)
                (mapcar (lambda (outer-val)
                          (cons outer-val
                                inner-val))
                        (car lists)))
              (apply #'combinations (cdr lists)))))

(defun product (values &key (length (length values)))
  (apply #'combinations
         (loop for i from 0 below length
               collect (copy-seq values))))

(defun reduce-pairwise (values functions)
  (let ((x (car values))
        (y (car (cdr values))))
    (if (not y)
        x
        (reduce-pairwise (cons (funcall (car functions) x y)
                               (cddr values))
                         (cdr functions)))))

(defun part1 (input)
  (let ((values (parse input)))
    (loop for (sum . values) in values
          for operation-combinations = (product (list #'+ #'*) :length (1- (length values)))
          when (find sum (loop for operations in operation-combinations
                           collect (reduce-pairwise values operations))
                     :test #'=)
          sum sum)))

(part1 (uiop:read-file-string #P"input"))

(defun || (&rest numbers)
  (parse-integer (format nil "~{~d~}" numbers)))

(defun part2 (input)
  (let ((values (parse input)))
    (loop for (sum . values) in values
          for operation-combinations = (product (list #'+ #'* #'||) :length (1- (length values)))
          when (find sum (loop for operations in operation-combinations
                               collect (reduce-pairwise values operations))
                     :test #'=)
          sum sum)))

(part2 (uiop:read-file-string #P"input"))
