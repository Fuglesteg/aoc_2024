(defvar *example*
  "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")

(defun parse (input)
  (let (obstacles
        guard-position
        (lines (split-string-remove-empties input :separator '(#\Newline))))
    (loop for line in lines
          for y from 0
          do (loop for char across line
                   for x from 0
                   do (case char
                        (#\# (push (cons x y) obstacles))
                        (#\^ (setf guard-position (cons x y))))))
    (list :obstacles obstacles
          :guard (list :position guard-position :direction :up)
          :width (length (first lines))
          :height (length lines))))

(defun guard-new-position (guard)
  (destructuring-bind (&key position direction) guard
    (destructuring-bind (x . y) position
      (case direction
        (:up (cons x (- y 1)))
        (:down (cons x (+ 1 y)))
        (:left (cons (- x 1) y))
        (:right (cons (+ 1 x) y))))))

(defun guard-legal-position-p (position obstacles)
  (not (find-if (lambda (obstacle-position)
                  (destructuring-bind (position-x . position-y) position
                    (destructuring-bind (obstacle-position-x . obstacle-position-y) obstacle-position
                      (and (= position-x obstacle-position-x)
                           (= position-y obstacle-position-y)))))
                obstacles)))

(defun guard-turn (guard)
  (setf (getf guard :direction)
        (case (getf guard :direction)
          (:up :right)
          (:right :down)
          (:down :left)
          (:left :up)))
    guard)

(defun guard-move (guard obstacles)
  (loop for new-position = (guard-new-position guard)
        until (guard-legal-position-p new-position obstacles)
        do (setf guard (guard-turn guard))
        finally (setf (getf guard :position) new-position))
  guard)

(defun guard-out-of-bounds-p (guard width height)
  (destructuring-bind (&key position &allow-other-keys) guard
    (destructuring-bind (x . y) position
      (or (>= y height)
          (< y 0)
          (>= x width)
          (< x 0)))))

(defun part1 (input)
  (destructuring-bind (&key obstacles guard width height) (parse input)
    (let ((positions (loop until (guard-out-of-bounds-p guard width height)
                           do (setf guard (guard-move guard obstacles))
                           collect (getf guard :position))))
      (+ (length (remove-duplicates positions :test #'equal))))))
    
(part1 (uiop:read-file-string #P"input"))

(defun part2 (input)
  (destructuring-bind (&key obstacles guard width height) (parse input)
    (let ((original-guard (copy-seq guard))
          (positions (loop until (guard-out-of-bounds-p guard width height)
                           do (setf guard (guard-move guard obstacles))
                           collect (getf guard :position))))
      (loop for position in positions
            do (setf guard (copy-seq original-guard))
            unless (loop until (guard-out-of-bounds-p guard width height)
                         for ticks from 0
                         never (> ticks 200)
                         do (setf guard (guard-move guard (append (list position) obstacles)))
                         finally (return t))
            sum 1)))) ; => 2460

(part2 (uiop:read-file-string #P"input"))