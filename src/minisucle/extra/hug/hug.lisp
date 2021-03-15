;;;; hug.lisp

(in-package #:hug)

(defun setup ()
  #+nil
  (py4cl2:pyexec
   "
import torch
from transformers import BertTokenizer, BertModel,BertForMaskedLM
tokenizer = BertTokenizer.from_pretrained('bert-base-cased')
model = BertForMaskedLM.from_pretrained('bert-base-cased')   
"
   )
#+nil
  (py4cl2:pyexec
   "
import torch
from transformers import BertTokenizer, BertModel,BertForMaskedLM
tokenizer = BertTokenizer.from_pretrained('bert-large-cased-whole-word-masking')
model = BertForMaskedLM.from_pretrained('bert-large-cased-whole-word-masking')
"
   )
;;#+nil
  (py4cl2:pyexec
   "
import torch
from transformers import BertTokenizer, BertModel,BertForMaskedLM
tokenizer = BertTokenizer.from_pretrained('bert-large-uncased')
model = BertForMaskedLM.from_pretrained('bert-large-uncased')
"
   )
  ;;get tokens
  (alltokens)
  (setf *longest-token* (longest-token))
  (setup-special-tokens))

(defparameter *id_cls* nil)
(defparameter *id_sep* nil)
(defparameter *id_mask* nil)

(defun mask-id ()
  (py4cl2:pyexec
   "
inputs = tokenizer(\"[CLS][SEP][MASK]\", return_tensors='pt')
# print(inputs)
len = inputs.input_ids
# print([len[0][1], len[0][2], len[0][3]])"
   )
  (let ((array (py4cl2:pyeval "[len[0][1].item(), len[0][2].item(), len[0][3].item()]")))
    (list :cls (aref array 0)
	  :sep (aref array 1)
	  :mask (aref array 2)))
  )

(defun setup-special-tokens ()
  (let ((data (mask-id)))
    (setf *id_cls* (getf data :cls)
	  *id_sep* (getf data :sep)
	  *id_mask* (getf data :mask))))

(defparameter *teststr* "[MASK] [MASK] [MASK] of the United States mismanagement of the Coronavirus is its distrust of science.")
(defun results
    (&optional (str *teststr*))
  (setf (py4cl2:pyeval "input_txt")
	(py4cl2:pythonize str))
  (setf (py4cl2:pyeval "top") 10)
  (py4cl2:pyexec
   "
inputs = tokenizer(input_txt, return_tensors='pt')
# print(inputs)
# 
len = list(inputs.input_ids.size())[1]
# print(len)
outputs = model(**inputs)
predictions = outputs[0]
# print(predictions[0])
sorted_preds, sorted_idx = predictions[0].sort(dim=-1, descending=True)
# print(sorted_preds)
# print(sorted_idx.size())

inputids = inputs.input_ids[0].tolist()

predicted = [[[sorted_idx[i, k].item(),sorted_preds[i, k].item()] for k in range(top)] for i in range(0,len)]
"
   )

  (py4cl2:pyeval "[inputids, predicted]"))

;;https://stackoverflow.com/questions/46826218/pytorch-how-to-get-the-shape-of-a-tensor-as-a-list-of-int
(defun safe-subseq (str start end)
  (subseq str (max 0 start)
	  (min end (length str))))
(defun partialp (str)
  (string=
   (safe-subseq str 0 2)
   "##"))
(defun foo (&optional (str *teststr*))
  (let ((res (results str))
	(first t))
    (loop :for seq :across res :collect
       (with-output-to-string (stream)
	 (loop :for str :across seq :do
	    (progn	   
	      (cond ((partialp str)
		     (setf str (safe-subseq str 2 (length str)))
		     (write-char #\| stream))
		    ((not first)
		     (write-char #\Space stream)))
	      (write-string str stream)
	      (when first
		(setf first nil))))))))

"Websites: Thousands of \"mirror sites\" exist that republish content from Wikipedia: two prominent ones, that also include content from other reference sources, are Reference.com and Answers.com. Another example is Wapedia, which began to display Wikipedia content in a mobile-device-friendly format before Wikipedia itself did."
(defparameter *teststrs*
  '("[MASK] [MASK] [MASK] of the United States mismanagement of the Coronavirus is its distrust of science."
    ;;"grass, [MASK], frog, [MASK], dog, [MASK], cat, [MASK], seed, [MASK], tree, [MASK], computer, [MASK], fire hydrant, [MASK], sidewalk, [MASK], ladybug, [MASK], octopus, [MASK], mask, [MASK],"
    "[MASK],[MASK],[MASK],[MASK],[MASK],[MASK],"
    "The sun, mars, earth and [MASK][MASK][MASK][MASK][MASK][MASK][MASK][MASK][MASK][MASK][MASK][MASK][MASK][MASK]"
    ;;"[CLS] The earth revolves around the [MASK]."
    ;;"[MASK] [MASK] [MASK] of the United States mismanagement of the Coronavirus is its distrust of science."
    "Websites: Thousands of \"mirror sites\" exist [MASK] republish content from Wikipedia: two prominent ones, that also include content [MASK] [MASK], are Reference.com and Answers.com. [MASK] [MASK] [MASK] [MASK], which began to display Wikipedia content in a mobile-device-friendly format before Wikipedia itself did."))
(defun test ()
  (mapcar 'foo *teststrs*))


;;yandex-images-download Firefox --keywords "vodka, bears, balalaika" --limit 10

(alexandria:define-constant +mask+ "[MASK]" :test #'string=)
(defun test1 (&optional (a +mask+) (b +mask+) (c +mask+))
  (let ((thing (format nil
		    ;;"When you combine ~a and ~a you get ~a."
		    "Combining ~a and ~a produces ~a."
		    a b c)))
    (write-string thing)
    (car (foo thing))))

(defun test12 (&optional (a +mask+) (b +mask+) (c +mask+))
  (format nil
	  ;;"When you combine ~a and ~a you get ~a."
	  "Combining ~a and ~a produces ~a."
	  a b c))

(defun mask (n)
  (with-output-to-string (str)
    (loop :repeat n :do
      (write-string +mask+ str))))

(defun roundfloat (f)
  (let ((n 4.0))
    (/ (round (* f n))
       n)))

;;FIXME::misnomer, not only masks.
(defun predict (&optional (str *teststr*))
  (format t "~%~a" str)
  (let* ((thing (results str))
	 (input (elt thing 0))
	 (output (elt thing 1)))
    ;;FIXME::hack to shuffle data, fix python instead?
    (let* ((input-tokens (map 'list 'gettoken input))
	   (fixed-outputs
	     (map 'list 'create-prediction
		  input-tokens
		  (map 'list (lambda (index)
			       (map 'list (lambda (tok)
					    (list (gettoken (aref tok 0))
						  (aref tok 1)))
				    index))
		       output))))
      fixed-outputs)))
#+nil
(defun onlymasks (&optional (str *teststr*))
  (write-string str)
  (let ((thing (results str)))
    (setf thing (coerce thing 'list))
    (let ((input (first thing))
	  (rest (cdr thing))
	  (mask (getf (mask-id) :mask)))
      (mapcar (lambda (item)
		(remove nil
			(map 'list
			     (lambda (a b c d e)
			       (declare (ignorable a))
			       (when (= b mask)
				 (list c d e)))
			     (aref input 0)
			     (aref input 1)
			     (aref item 0)
			     (aref item 1)
			     (aref item 2))))
	      rest))))
#+nil
(defun onlymasks2 (&optional (str *teststr*))
  (mapcar (lambda (x)
		   (mapcar 'first x))
	  (onlymasks str))
   
  (apply 'mapcar 'list
	 (mapcar (lambda (x)
		   (mapcar 'first x))
		 (onlymasks str))))

;;(onlymasks "[[X O X][[MASK] [MASK] [MASK]] [[MASK] [MASK] [MASK]]]")

;;python3.6 run_generation.py  --model_type=gpt2 --model_name_or_path=gpt2
;;python3.6 run_generation.py  --model_type=ctrl --model_name_or_path=ctrl
;;python3.6 run_generation.py  --model_type=xlnet --model_name_or_path=xlnet ;doesnt work
;;python3.6 run_generation.py  --model_type=xlm --model_name_or_path=xlm ;doesnt work
;;python3.6 run_generation.py  --model_type=transfo-xl --model_name_or_path=trasnfo-xl

(defun create-prediction (original-token list-of-tokens)
  (make-prediction :tokens (coerce list-of-tokens 'vector)
		   :original-token original-token))
(struct-to-clos:struct->class
    (defstruct prediction
      original-token
      tokens))

(set-pprint-dispatch
 'prediction
 (lambda (stream o)
   (format stream "~%=====")
   (let* ((str (with-output-to-string (str)
		 (write (prediction-original-token o) :stream str)))
	  (len (length str)))
     (format stream "~a" str)
     (loop :repeat (- 50 len) :do
       (write-char #\= stream)))
   (loop :for tokpair :across (prediction-tokens o) :do
     (terpri stream)
     (destructuring-bind (tok probability) tokpair
       (let* ((str (token-string tok))
	      (strlen (length str)))
	 (write-string str stream)
	 (loop :repeat (+ 1 (- *longest-token* strlen)) :do
	   (write-char #\Space stream)))
       (loop :repeat (min probability 64) :do
	 (write-char #\* stream)))))
 )

(struct-to-clos:struct->class
    (defstruct token
      id
      string
      partialp))
(set-pprint-dispatch 'token
		     (lambda (Stream o)
		       (format stream "'~a'" (token-string o)))
 )
(defparameter *tokens* nil)
(defparameter *str->token* (make-hash-table :test 'equal))
(defparameter *longest-token* nil)
(defmethod gettoken ((n fixnum))
  (aref *tokens* n))
(defmethod gettoken ((n token)) n)
(defmethod gettoken ((n string))
  (gethash n *str->token*))
(defun alltokens ()
  (py4cl2:pyexec
   "
what = []
for i in tokenizer.get_vocab():
   what.append(i)")
  (let ((ids (py4cl2:pyeval "tokenizer.convert_tokens_to_ids(what)"))
	(tokens (py4cl2:pyeval "what")))
    (setf *tokens* (make-array (length tokens)))
    (map 'nil
	 (lambda (id name)
	   (setf (aref *tokens* id)
		 (make-token :id id :string name :partialp (partialp name))))
	 ids tokens)

    (loop :for token :across *tokens* :do
      (setf (gethash (token-string token)
		     *str->token*)
	    token))))

(defun longest-token ()
  (reduce 'max *tokens* :key (lambda (x) (length (token-string x))) :initial-value 0))

(/ (* 24000 (* 128 128 4)) 1024 1024 1024 1.0)

(/ (* 512 512 512 4) (* 1024 1024 1024 1.0))

(defun whatmadeof (&optional (string "man"))
  (predict (format nil "A ~a is made of [MASK] [MASK], [MASK] [MASK], [MASK] [MASK], and [MASK] [MASK]."
		      string)))
(defun join (&optional (list '( 1 2 3 4 )))
  (format nil "~{~A~^, ~}" list))
#+nil
(defun joinspace (&optional (list '( 1 2 3 4 )))
  (format nil "~{~A~^ ~}" list))
#+nil
(defun joinnone (&optional (list '( 1 2 3 4 )))
  (format nil "~{~A~}" list))
(defun maskn (items &optional (andp t))
  (let ((items (mapcar 'mask items)))
    (symbol-macrolet ((laststr (car (last items))))
      (when (and (not (= 1 (length items)))
		 andp)
	(setf laststr (format nil "and ~a" laststr)))
      ;;add a period
      (setf laststr (format nil "~a." laststr)))
    (join items)))

(defun whatmadeof2 (&optional (string "man") (mask '(2 2 2 2)))
  (predict (format nil "~a is made of ~a" string (maskn mask))))

(defun cmd (&optional (obj "man") (question "is made of") (mask '(2 2 2 2)))
  (predict (format nil "~a ~a ~a" obj question (maskn mask))))
(defun cm (&optional (thing "man is made of") (mask '(2 2 2 2)))
  (predict (format nil "~a ~a" thing (maskn mask))))

(struct-to-clos:struct->class
    (defstruct half-sentence
      words
      predictions))
;;words looks like '("word" nil "thing" nil)
;;nil represents the mask
(defmethod fulfilledp ((hs half-sentence))
  (not (position nil (half-sentence-words hs))))
(defmethod spaces-left ((hs half-sentence))
  (count nil (half-sentence-words hs)))
(defun create-half-sentence (&optional words)
  (make-half-sentence :words (coerce words 'vector)))
(defparameter *hs* (create-half-sentence '("The man is " nil " which means that " nil ".")))

(defun serialize-hs (&optional (hs *hs*))
  (tokens->string
   (map 'list (lambda (item)
		(or item +mask+))
	(half-sentence-words hs))))
(defparameter *cancel* t)
(defun foo (&rest words)
  (setf *cancel* nil)
  (ban-tokens)
  (let* ((hs (create-half-sentence words))
	 (timeout (spaces-left hs)))
    (tagbody
     :loop
       (let ((pred (predict (serialize-hs hs))))
	 (setf (half-sentence-predictions hs) pred)
	 (when (not (fulfilledp hs))
	   (multiple-value-bind (i tok) (find-best-index hs)
	     ;;(print (list i tok))
	     ;;(print (elt pred i))
	     (when (and (not *cancel*) tok)
	       (fulfill i tok hs)
	       (and (<= 0 (decf timeout))
		    (go :loop)))))))
    (format nil "~a" (serialize-hs hs))
    hs))

(defun fulfill (maskindex token hs)
  (let ((words (half-sentence-words hs))
	(masksseen 0))
    (let ((realindex (block out (dotimes (i (length words))
				  (let ((word (elt words i)))
				    (when (eql nil word)
				      (when (eql masksseen maskindex)
					(return-from out i))
				      (incf masksseen))))
			    nil)))
      (assert (not (null realindex)))
      (setf (elt words realindex) (token-string token)))))

(defun ban-tokens ()
  (ban "[UNK]")
  (map nil 'ban '("." "?" "!")) ;;we do not want the sentence to end abruptly.
  (map nil 'ban '(":" ";")) ;;we do not want the sentence to end abruptly.
  ;(map nil 'ban '("?" "." "(" ")" ";" ":" "|" "!" "-" "\"" "'" "..." "#" "/" "`"))
  ;(map nil 'ban '("+" "=" "^" "%" "$" "@" "<" ">"))
  ;(map nil 'ban '("," "the" "on" "of" "and"))
  ;(map nil 'ban (test5))
  )

(defun test5 ()
  (remove-if (lambda (x)
	       (every (lambda (char)
			(typep char 'standard-char))
		      (token-string x)))
	     (coerce *tokens* 'list)))
(defparameter *banned* (make-hash-table :test 'eq))
(defun ban (x)
  (setf (gethash (gettoken x) *banned*) t))
(defun bannedp (x)
  (gethash (gettoken x) *banned*))

;;FIXME::may be quadratic?
(defun find-best-index (hs)
  (let ((max 0)
	(min most-positive-fixnum)
	
	(maxindex 0)
	(maxtoken nil)

	(minindex 0)
	(mintoken nil)
	
	(maskindex 0))
    (with-slots (words predictions) hs
      (loop :for p :in predictions :do
	(when (maskp p)
	  (loop :for pair :across (prediction-tokens p) :do
	    (destructuring-bind (tok prob) pair
	      (when (not (bannedp tok))
		(when (> prob max)
		  (setf max prob
			maxindex maskindex
			maxtoken tok))	      
		(when (< prob min)
		  (setf min prob
			minindex maskindex
			mintoken tok)))))
	  (incf maskindex))))
    ;;(values minindex mintoken)
    (values maxindex maxtoken)
    ;;(values nil nil)
    ))

;;Ban all non ascii characters?


(defmethod maskp ((obj token))
  (maskp (token-id obj)))
(defmethod maskp ((obj prediction))
  (maskp (prediction-original-token obj)))
(defmethod maskp ((obj fixnum))
  (= obj *id_mask*))

;;(foo "The man goes to " () " to buy the " () () " for " () ".")
;;(foo "The man goes to " () " " () " to buy the " () " " () " for " () ".")

(defun foo2 (&rest words)
  (setf words
	(mapcan (lambda (x)
		  (typecase x
		    (number (make-list x :initial-element nil))
		    (symbol (list (string-downcase (string x))))
		    (otherwise (list x))))
		words))
  (apply 'foo words))

(defmacro f (&rest words)
  `(foo2 ,@(mapcar (lambda (x) `(quote ,x)) words)))

(defun footest (&optional (a 10) (b 1))
  (mapcar 'serialize-hs
	  (mapcar 'foo2
		  (alexandria:iota a :start b))))

(defun tokens->string (&optional (strings '("dirt" "asdfasdfsadf" "##s")))
  (setf (py4cl2:pyeval "temp") strings)
  (py4cl2:pyeval "tokenizer.convert_tokens_to_string(temp)"))


;;(f a ups box if made of the following materials ":" 10 ".")
;;(f in order to win 2 ", one must do" 2 "," 2 "," and 2 ".")
;;(f a cybernetic matrix is made of the following materials ": cybernetic lace," 12 ".")
;;(f ocean "," shore "," 6 bayside "," urban outfitters 6 "," gap "," outback steakhouse ".")
;;(f what 2 life 2 stone 2)
;;(f "All the following are required to manufacture a banana:" 10 ".")
;;(f "The black ball is next to the green mountain. The pole is" 2 "meters away from the black ball and" 2 "meters away from the green mountain.")

;;(f "The black ball is next to the green mountain. The pole is" 3 "away from the black ball and" 3 "away from the green mountain.")
;;(f "The green mountain is next to the black ball. The pole is" 3 "away from the black ball and" 3 "away from the green mountain.")
