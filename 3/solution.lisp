(defvar *example* "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

;(parse find "mul(" collect :digit* until #\, collect :digit* until #\))

(defun part1 (input)
  (let ((sum 0) accumulator)
    (loop for char across input
          for i from 0
          do (progn
               (setf accumulator (append accumulator (list char)))
               (when (not (string~ (coerce accumulator 'string) "mul("))
                 (setf accumulator nil))
               (when (string= (coerce accumulator 'string) "mul(")
                 (setf accumulator nil)
                 (let ((first-argument (string-collect-until (subseq input (1+ i))
                                                              :collectp #'digit-char-p
                                                              :untilp (lambda (char) (char= char #\,)))))
                   (when first-argument
                     (let ((second-argument (string-collect-until (subseq input (+ i (length first-argument) 2))
                                                                  :collectp #'digit-char-p
                                                                  :untilp (lambda (char) (char= char #\))))))
                       (when second-argument
                         (incf sum (* (parse-integer first-argument)
                                               (parse-integer second-argument))))))))))
    sum))

(format t "Part 1: ~a" (part1 (uiop:read-file-string #P"input")))

(defvar *example2* "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(defun part2 (input)
  (let ((sum 0)
        (eval t))
    (loop for index = (string-find input (if eval "don't()" "do()"))
          do (when eval
               (incf sum (part1 (subseq input 0 index)))
               (unless index
                 (loop-finish)))
          do (setf eval (not eval))
          do (setf input (subseq input (or index 0))))
    sum))

(format t "Part 2: ~a" (part2 (uiop:read-file-string #P"input")))
