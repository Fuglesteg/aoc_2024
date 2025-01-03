(defvar *example* "3 4
4 3
2 5
1  3
3 9
3 3")

(defun split-input-into-two-lists (input)
  (let ((numbers (split-string-remove-empties input :separator '(#\Newline #\Space #\Tab))))
    (loop for (x y) on numbers by #'cddr
          collect (parse-integer x) into left
          collect (parse-integer y) into right
          finally (return (values left right)))))

(defun part-1 (input)
  (multiple-value-bind (left right) (split-input-into-two-lists input)
    (setf left (sort left #'<))
    (setf right (sort right #'<))
    (apply #'+
           (loop for x in left
                 for y in right
                 collect (abs (- x y))))))

(part-1 (uiop:read-file-string "input"))

(defun part-2 (input)
  (multiple-value-bind (left right) (split-input-into-two-lists input)
    (let (similarities)
      (apply #'+
             (mapcar
              (lambda (x)
                (let ((similarity-value (cdr (assoc x similarities))))
                  (unless similarity-value
                    (progn
                      (setf similarity-value (count-if (lambda (y) (= y x)) right))
                      (setf similarities (acons x similarity-value similarities))))
                  (* x similarity-value)))
              left)))))

(part-2 (uiop:read-file-string "input"))
