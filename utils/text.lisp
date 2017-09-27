
(ql:quickload "split-sequence")


(defun get-words (txt)
  (loop for word in (split-sequence:split-sequence #\  txt)
        collect (list word (length word))))


(defun get-words* (txt)
  (loop for section in (split-sequence:split-sequence #\NewLine txt)
        collect (get-words section)))


(defun show-sections (words)
  (loop for section in words do
        (print "---------------------------------------------------------")
        (print section)))

