(defvar *example*
"............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............")

(defun parse (input)
  (let (width height)
    (values
     (loop for line in (remove "" 
                               (sequence-split input '(#\Newline)) 
                               :test #'string=)
           for y from 0
           finally (setf height y)
           append (loop for char across line
                        for x from 0
                        when (char/= char #\.)
                        collect (cons char (cons x y))
                        finally (setf width (max (or width 0) x))))
     width
     height)))

(defun antenna-siblings (antenna antennas)
  (remove-if-not (lambda (current-antenna)
                   (and (not (equal antenna current-antenna))
                        (char= (car antenna)
                               (car current-antenna))))
                 antennas))

(defun antenna-antinodes (antenna antennas)
  (let ((siblings (antenna-siblings antenna antennas)))
    (loop for (nil sibling-x . sibling-y) in siblings
          collect (destructuring-bind (_frequency x . y) antenna
                    (declare (ignore _frequency))
                   (let ((diff-x (- x sibling-x))
                         (diff-y (- y sibling-y)))
                     (cons (+ x diff-x)
                           (+ y diff-y)))))))

(defun antinode-within-bounds-p (antinode width height)
  (destructuring-bind (x . y) antinode
    (and (<= 0 x)
         (>= width x)
         (<= 0 y)
         (>= height y))))

(defun part1 (input)
  (multiple-value-bind (antennas width height) (parse input)
    (let ((antinodes (loop for antenna in antennas
                           append (antenna-antinodes antenna antennas))))
       (length
        (remove-if-not
        (lambda (antinode)
          (antinode-within-bounds-p antinode width height))
        (remove-duplicates 
         antinodes
         :test #'equal))))))

(part1 (uiop:read-file-string #P"input"))

(defun antenna-antinodes-with-resonant-harmonics (antenna antennas)
  (let ((siblings (antenna-siblings antenna antennas)))
    (loop for (nil sibling-x . sibling-y) in siblings
          append (destructuring-bind (_frequency x . y) antenna
                    (declare (ignore _frequency))
                    (let ((diff-x (- x sibling-x))
                          (diff-y (- y sibling-y)))
                      (loop for i from 0 to 50
                            collect (cons (+ x (* diff-x i)) 
                                          (+ y (* diff-y i)))))))))

(defun part2 (input)
  (multiple-value-bind (antennas width height) (parse input)
    (let ((antinodes (loop for antenna in antennas
                           append (antenna-antinodes-with-resonant-harmonics
                                   antenna
                                   antennas))))
      (length
       (remove-if-not
       (lambda (antinode)
         (antinode-within-bounds-p antinode width height))
       (remove-duplicates 
        antinodes
        :test #'equal))))))

(part2 (uiop:read-file-string #P"input"))