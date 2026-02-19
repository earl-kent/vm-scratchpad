

(in-package vm-scratchpad)



(defvar *value1*)
(defvar *mv-count*)

(defun values1 (a)                               \
  (setf *value1* a)
  (setf *mv-count* 1))



(defstruct (byte-code-vector (:conc-name byte-code-))
  (ptr 0 :type integer)
  (bytes nil  :type (array (unsigned-byte 8)) ))

(defun byte-code-read-next (byte-code-vector)
  (prog1 (aref (byte-code-bytes byte-code-vector)
	       (byte-code-ptr byte-code-vector))
    (incf (byte-code-ptr byte-code-vector))))

(defun byte-code-access-and-set (byte-code-vector index)
  (setf (byte-code-ptr byte-code-vector) index)
  (byte-code-read-next byte-code-vector))

(defvar *the-byte-code-vector* )

(defun bytes-print-table (byte-code-bytes)
  (let ((bytes (etypecase byte-code-bytes
		 (byte-code-vector
		  (coerce (byte-code-bytes byte-code-bytes) 'list))
		 (list byte-code-bytes)))
	(index 0))
    (dolist (byte bytes)
      (format t "~2,D   ~3,'0D   #b~8,'0B~%" index byte byte)
      (incf index))))


(defun byte-code-collect (byte-code-vector)
  (setf (byte-code-ptr byte-code-vector) 0)
  (loop repeat (length (byte-code-bytes byte-code-vector))
	collect (byte-code-read-next byte-code-vector)))


(defun create-byte-code-vector ()
  (let ((bytes '(#b00000001
		 #b00000010
		 #b00000100
		 #b00001000
		 #b00010000
		 #b00100000
		 #b01000000
		 #b10000000
		 #b00000011
		 #b00011000
		 #b01100000
		 #b00000111
		 #b00011110
		 #b01110100
		 #b11101000
		 #b11110000
		 #b00101111
		 #b01111000
		 #b11111000
		 #b01111100
		 #b00011111
		 #b11111111)))
    (setf *the-byte-code-vector*
	  (make-byte-code-vector
	   :bytes
	   (make-array (length bytes)
		       :element-type '(unsigned-byte 8)
		       :initial-contents bytes)))))


;; Assembles next operand (usigned-byte 16) from the next one or two
;; (usigned-byte 8)s in the byte stream.
(defun u-operand (byte-code-voctor)
  (let ((b (byte-code-read-next byte-code-voctor)))
    (if (ldb-test (byte 1 7) b)
	(progn
	  (mask-field (byte 7 0) b)
	  (dpb (byte-code-read-next byte-code-voctor) (byte 8 0)
	       (ash (mask-field (byte 7 0) b) 8 )))
	b)))

(defun u-operand-test ()
  (setf (byte-code-ptr *the-byte-code-vector*) 14)
  (format t "~16,'0B~%" (u-operand *the-byte-code-vector*)))

;; Macro U_operand(where);
;; moves the next Operand (an Unsigned Integer)
;; to (uintL)where or (uintC)where
;; and advances the Bytecodepointer. */
;; #define U_operand(where)  \
;;   { where = *byteptr++;           /* read first Byte */ \
;;     if ((uintB)where & bit(7))    /* Bit 7 set? */ \
;;       { where &= ~bit(7);         /* yes -> delete */ \
;;         where = where << 8;                            \
;;         where |= *byteptr++;          /* and read next Byte */ \
;;   }   }


;; Note this is slightly different from the clisp dotimesw which takes
;; 3 parameters. We assume that the first parameter is never used for
;; side effects, so we get rid of it.
(defun dotimesc (count statement)
  (dotimes-check-sizeof count uintw)
  (dotimes (n count)
    (funcall statement)))
