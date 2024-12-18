(defun string-collect-until (string &key collectp untilp accept-garbage)
  "Collect characters from the string when collectp is t until untilp i t,
if accept-garbage is t we skip over things that are neither collectp or untilp"
  (let (result)
    (loop for char across string
          until (funcall untilp char)
          do (if (funcall collectp char)
                 (setf result (append result (list char)))
                 (when (not accept-garbage)
                   (return-from string-collect-until nil))))
    (coerce result 'string)))

(defun string~ (string1 string2)
  "Compare if the strings match up to the end of the shortest string"
  (let ((min-length (min (length string1)
                         (length string2))))
    (string= string1 string2
             :end1 min-length
             :end2 min-length)))

(defun string-occurrences (string needle)
  (let ((occurrences 0))
    (loop for index = (search needle string)
          while index
          do (when index
               (incf occurrences))
          do (setf string (subseq string (+ (length needle) index))))
    occurrences))

(defun string-empty-p (string)
  (or (not (stringp string))
      (string= string "")))

(defun split-string-remove-empties (string &key separator)
  (remove-if #'string-empty-p (uiop:split-string string :separator separator)))

(defun sequence-split (string separator)
  (loop for start = (search separator string)
        while start
        for portion = (subseq string 0 start)
        do (setf string (subseq string (+ start (length separator))))
        collect portion into result
        finally (return (append result (list string)))))