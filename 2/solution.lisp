(defvar *example*
"7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

(defun parse-input (input)
  (mapcar 
   (lambda (line)
     (mapcar #'parse-integer (uiop:split-string line :separator '(#\Space))))
   (uiop:split-string input :separator '(#\Newline))))

(defun direction (trend)
  (cond ((< trend 0) :down)
        ((> trend 0) :up)
        (t nil)))

(defun in-range-p (x min max)
  (and (<= x max)
       (>= x min)))

(defun report-trends (report)
  (loop for (x y) on report
        while y
        collect (- y x)))

(defun dampen-trends (trends &key (dampening 1))
  (loop for trend in trends
        for direction = (direction trend) then direction
        for i from 0
        when (if (case direction
                   (:up (in-range-p trend 1 3))
                   (:down (in-range-p trend -3 -1))
                   (nil nil))
                 t
                 (if (< 0 dampening)
                     (progn (decf dampening)
                            (when (not (or (= i 0)
                                           (= (1+ i) (length trends))))
                              (incf (nth (1+ i) trends) trend))
                            nil)
                     t))
        collect trend))

(defun remove-nth (n list)
  (append (subseq list 0 n) (subseq list (1+ n))))

(defun trends-safe (trends)
  (loop for trend in trends
        for direction = (direction trend) then direction
        always (case direction
                 (:up (in-range-p trend 1 3))
                 (:down (in-range-p trend -3 -1))
                 (nil nil))
        finally (return t)))

(defun report-safe-with-dampening-p (report)
  (or (trends-safe (report-trends report))
      (loop for i from 0 below (length report)
            for dampened-report = (remove-nth i report)
            when (trends-safe (report-trends dampened-report))
            return t)))

(defun part-1 (input)
  (count t (mapcar (lambda (report) 
                     (trends-safe (report-trends report)))
                   (parse-input input))))

(part-1 (uiop:read-file-string "input"))

(defun part-2 (input)
  (count t (mapcar #'report-safe-with-dampening-p
                   (parse-input input))))

(part-2 (uiop:read-file-string "input"))