(defvar *example*
  "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

(defun lines-column (lines column)
  (loop for line in lines
        collect (char line column)))

(defun lines-right-diagonal-column (lines column)
  (loop for line in lines
        while (< column (length line))
        collect (char line column)
        do (incf column)))

(defun lines-right-diagonals (lines)
  (append
   (loop for column below (length (first lines))
         collect (coerce (lines-right-diagonal-column lines column) 'string))
   (loop for current-lines = (cdr lines) then (cdr current-lines)
         while current-lines
         collect (coerce (lines-right-diagonal-column current-lines 0) 'string))))

(defun lines-left-diagonal-column (lines column)
  (loop for line in lines
        while (>= column 0)
        collect (char line column)
        do (decf column)))

(defun lines-left-diagonals (lines)
  (let ((width (length (first lines))))
    (append
     (loop for column below width
           collect (coerce (lines-left-diagonal-column lines column) 'string))
     (loop for current-lines = (cdr lines) then (cdr current-lines)
           while current-lines
           collect (coerce (lines-left-diagonal-column current-lines (1- width)) 'string)))))

(defun part1 (input)
  (let* ((lines (split-string-remove-empties input :separator '(#\Newline)))
         (width (length (first lines)))
         (columns (loop for column from 0 below width
                        collect (coerce (lines-column lines column) 'string)))
         (right-diagonals (lines-right-diagonals lines))
         (left-diagonals (lines-left-diagonals lines))
         (lines-to-search (append lines columns right-diagonals left-diagonals)))
    (apply #'+ (mapcar (lambda (line) 
                         (+ (string-occurrences line "XMAS")
                            (string-occurrences line "SAMX")))
                       lines-to-search))))

(part1 (uiop:read-file-string #P"input"))

(defun lines-char (lines x y)
  (char (nth y lines) x))

(defun part2 (input)
  (let* ((lines (split-string-remove-empties input :separator '(#\Newline)))
         (crosses (loop for column from 1 below (1- (length (first lines)))
                        append (loop for row from 1 below (1- (length lines))
                                      collect (coerce (list (lines-char lines (1- column) (1- row))
                                                            (lines-char lines column row)
                                                            (lines-char lines (1+ column) (1+ row))
                                                            (lines-char lines (1+ column) (1- row))
                                                            (lines-char lines column row)
                                                            (lines-char lines (1- column) (1+ row)))
                                                      'string)))))
    (apply #'+ (mapcar (lambda (line)
                         (let ((left (subseq line 0 3))
                               (right (subseq line 3 6)))
                           (if (and (or (string= left "SAM")
                                    (string= left "MAS"))
                                (or (string= right "SAM")
                                    (string= right "MAS")))
                               1
                               0)))
                         crosses))))

(part2 (uiop:read-file-string #P"input"))
