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
  "Parse the input into a list of cons pairs of priorities and a list of updates"
  (let* ((section-separator (string-find input (coerce '(#\Newline #\Newline) 'string)))
         (first-section (subseq input 0 section-separator))
         (second-section (subseq input (+ 2 section-separator))))
    (values (loop for pair in (split-string-remove-empties first-section :separator '(#\Newline))
                  collect (destructuring-bind (left right) (uiop:split-string pair :separator '(#\|))
                            (cons (parse-integer left) (parse-integer right))))
            (loop for update in (split-string-remove-empties second-section :separator '(#\Newline))
                  collect (mapcar #'parse-integer (split-string-remove-empties update :separator '(#\,)))))))

(parse *example*)

(defun priorities-lookup (priorities)
  (reduce (lambda (lookup value)
            (let ((existing (assoc (car value) lookup)))
              (if existing
                  (setf (cdr existing) (append (list (cdr value)) (cdr existing)))
                  (setf lookup (acons (car value) (list (cdr value)) lookup)))
              lookup))
          priorities
          :initial-value '()))

(defun update-acceptable-p (update priorities-lookup)
  (loop for page in update
        collect page into pages-so-far
        when (let ((non-acceptable-previous-pages (cdr (assoc page priorities-lookup))))
               (find-if (lambda (non-acceptable-page)
                          (find non-acceptable-page pages-so-far))
                        non-acceptable-previous-pages))
        return nil
        finally (return t)))

(defun middle (sequence)
  (elt sequence (floor (/ (length sequence) 2))))

(defun part1 (input)
  (multiple-value-bind (priorities updates) (parse input)
    (let ((priorities-lookup (priorities-lookup priorities)))
      (loop for update in updates
            when (update-acceptable-p update priorities-lookup)
            sum (middle update)))))

(part1 (uiop:read-file-string #P"input"))

(defun update-sort (update priorities-lookup)
  (sort update
        (lambda (page1 page2)
          (let ((non-acceptable-previous-pages (cdr (assoc page2 priorities-lookup))))
            (find page1 non-acceptable-previous-pages)))))

(defun part2 (input)
  (multiple-value-bind (priorities updates) (parse input)
    (let ((priorities-lookup (priorities-lookup priorities)))
      (loop for update in updates
            unless (update-acceptable-p update priorities-lookup)
            sum (middle (update-sort update priorities-lookup))))))

(part2 (uiop:read-file-string #P"input"))
    