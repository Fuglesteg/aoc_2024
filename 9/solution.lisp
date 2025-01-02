(defvar *example* "2333133121414131402")

(defun make-file-block (size &key file-id file-size)
  (make-array 
   size
   :fill-pointer file-size
   :element-type 'integer
   :initial-element file-id))

(defun parse (input)
  (let ((input (remove #\Newline input :test #'char=)))
    (loop for id from 0 below (/ (length input) 2)
          for used-space = (elt input (* id 2))
          for free-space = (ignore-errors (elt input (1+ (* id 2))))
          collect (make-file-block (+ (digit-char-p (or free-space #\0))
                                      (digit-char-p used-space))
                                   :file-id id
                                   :file-size (digit-char-p used-space)))))

(defun file-block-available-space-p (file-block)
  (/= (length file-block) (array-total-size file-block)))

(defun file-blocks-compress (file-blocks)
  (loop with first-block-index = 0
        with last-block-index = (1- (length file-blocks))
        for first-empty-block = (nth first-block-index file-blocks)
        for last-filled-block = (nth last-block-index file-blocks)
        do (cond
             ((not (file-block-available-space-p first-empty-block))
              (incf first-block-index))
             ((= 0 (length last-filled-block)) (decf last-block-index))
             ((= first-block-index last-block-index) (loop-finish))
             (t (vector-push (vector-pop last-filled-block) first-empty-block))))
  file-blocks)

(defun file-blocks-checksum (file-blocks)
  (loop for file-id in (apply #'concatenate (cons 'list file-blocks))
        for location from 0
        sum (* file-id location)))

(defun part1 (input)
  (let ((file-blocks (parse input)))
    (file-blocks-compress file-blocks)
    (file-blocks-checksum file-blocks)))

(part1 (uiop:read-file-string #P"input"))

(defun file-block-available-space (file-block)
  (- (array-total-size file-block) (length file-block)))

(defun copy-file-blocks (file-blocks)
  (loop for file-block in file-blocks
        collect (make-array (array-total-size file-block) 
                            :fill-pointer (length file-block)
                            :initial-element (elt file-block 0))))

(defun file-blocks-rearrange-files (file-blocks)
  (let ((files (copy-file-blocks file-blocks)))
    (loop with first-block-index = 0
          with file-index = (1- (length files))
          for first-empty-block = (nth first-block-index file-blocks)
          for file = (nth file-index file-blocks)
          do (cond
               ((<= file-index 0) (loop-finish))
               ((<= (length file) 0) (decf file-index))
               ((>= first-block-index file-index) (setf first-block-index 0)
                                                        (decf file-index))
               ((> (length file)
                   (file-block-available-space first-empty-block))
                (incf first-block-index))
               ((>= 0 (length file)) (decf file-index)
                                                  (setf first-block-index 0))
               (t (let ((file-in-block 
                          (make-file-block (length first-empty-block)
                                           :file-id (elt first-empty-block 0)
                                           :file-size (length first-empty-block)))
                        (moved-file
                          (make-file-block (+ (length first-empty-block)
                                              (- (file-block-available-space
                                                  first-empty-block)
                                                 (length file)))
                                           :file-id (elt file 0)
                                           :file-size (length first-empty-block))))
                    (setf file-blocks
                          (print (append (subseq (print file-blocks) 0 first-block-index)
                                  (list (print file-in-block)
                                        (print moved-file))
                                  (subseq file-blocks (1+ first-block-index)))))
                    #+nil (setf (subseq file-blocks 
                                  first-block-index
                                  (1+ first-block-index))
                          (list file-in-block
                                moved-file))
                    ;; empty last file block
                    (setf (fill-pointer file) 0)))))
    file-blocks))

(defun part2 (input)
  (let ((file-blocks (parse input)))
    (setf file-blocks (file-blocks-rearrange-files file-blocks))
    file-blocks))

(part2 *example*)