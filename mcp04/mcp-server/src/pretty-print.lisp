(in-package :mcp-server)

(defun pretty-print-tf (content)
  "Format Terraform (HCL) content for better readability."
  (let ((result "")
        (indent-level 0)
        (in-string nil))
    (dotimes (i (length content))
      (let ((char (char content i)))
        (cond
          ;; Detect string literals to avoid formatting inside them
          ((and (char= char #\") (not (char= (char content (1- i)) #\\)))
           (setf in-string (not in-string))
           (setf result (concatenate 'string result (string char))))
          (in-string
           (setf result (concatenate 'string result (string char))))
          ;; Format outside strings
          ((and (char= char #\() (not in-string))
           (progn
             (setf result (concatenate 'string result (string char) (format nil "~%") ))
             (incf indent-level)
             (setf result (concatenate 'string result (make-string (* 2 indent-level) :initial-element #\Space)))))
          ((and (char= char #\)) (not in-string))
           (progn
             (decf indent-level)
             (setf result (concatenate 'string result (format nil "~%") ))
             (setf result (concatenate 'string result (make-string (* 2 indent-level) :initial-element #\Space) (string char)))))
          ((and (char= char #\{) (not in-string))
           (progn
             (setf result (concatenate 'string result (string char) (format nil "~%") ))
             (incf indent-level)
             (setf result (concatenate 'string result (make-string (* 2 indent-level) :initial-element #\Space)))))
          ((and (char= char #\}) (not in-string))
           (progn
             (decf indent-level)
             (setf result (concatenate 'string result (format nil "~%") ))
             (setf result (concatenate 'string result (make-string (* 2 indent-level) :initial-element #\Space) (string char)))))
          ((and (char= char #\Newline) (not in-string))
           (setf result (concatenate 'string result (string char) (make-string (* 2 indent-level) :initial-element #\Space))))
          (t
           (setf result (concatenate 'string result (string char)))))))
    result))
