(defvar *example*
  "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(defun parse (input)
  (let* ((section-separator (string-find input (coerce '(#\Newline #\Newline) 'string)))
         (first-section (subseq input 0 section-separator))
         (second-section (subseq input (+ 2 section-separator))))
    (values (loop for pair in (split-string-remove-empties first-section :separator '(#\Newline))
                  collect (destructuring-bind (left right) (uiop:split-string pair :separator '(#\|))
                            (cons (parse-integer left) (parse-integer right))))
            (loop for update in (split-string-remove-empties second-section :separator '(#\Newline))
                  collect (mapcar #'parse-integer (split-string-remove-empties update :separator '(#\,)))))))

(parse *example*)

(defun part1 (input)
  (multiple-value-bind (priorities updates) (parse input)
    (let ((priorities-lookup (reduce (lambda (lookup value)
                                       (let ((existing (assoc value lookup)))
                                         (if existing
                                             (incf (cdr existing))
                                             (setf lookup (acons value 1 lookup))))
                                       lookup)
                                     priorities
                                     :initial-value '()
                                     :key #'cdr)))
      (loop for update in updates
            when (tree-equal
                  update
                  (sort (copy-seq update) (lambda (page1 page2)
                                 (< (or (cdr (assoc page1 priorities-lookup)) 0)
                                    (or (cdr (assoc page2 priorities-lookup)) 0)))))
            sum (nth (floor (/ (length update) 2)) update)))))

(part1 (uiop:read-file-string #P"input"))