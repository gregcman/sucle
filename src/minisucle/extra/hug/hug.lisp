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
  (setf *longest-token* (longest-token)))

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

  (py4cl2:pyeval "[[tokenizer.convert_ids_to_tokens(inputids), inputids],predicted]"))

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

(defun onlymasks3 (&optional (str *teststr*))
  (write-string str)
  (let* ((thing (results str))
	 (input (elt thing 0))
	 (output (elt thing 1)))
    ;;FIXME::hack to shuffle data, fix python instead?
    (let ((paired-input (map 'list 'list (aref input 0) (aref input 1)))
	  (fixed-outputs
	    (map 'list 'create-prediction
		 (map 'list (lambda (index)
			      (map 'list (lambda (tok)
					   (list (gettoken (aref tok 0))
						 (aref tok 1)))
				   index))
		      output))))
      ;;#+nil
      (let ((mask (getf (mask-id) :mask)))
	(mapcar 'rest
		(remove-if-not
		 (lambda (x)
		   (= mask (second (first x))))
		 (mapcar 'cons paired-input
			 fixed-outputs)))))))
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

(defun create-prediction (list-of-tokens)
  (make-prediction  :tokens (coerce list-of-tokens 'vector)))
(struct-to-clos:struct->class
    (defstruct prediction
      tokens))

(set-pprint-dispatch
 'prediction
 (lambda (stream o)
   (format stream "~%====")
   (terpri stream)
   (loop :for tokpair :across (prediction-tokens o) :do
     (destructuring-bind (tok probability) tokpair
       (let* ((str (token-string tok))
	      (strlen (length str)))
	 (write-string str stream)
	 (loop :repeat (+ 1 (- *longest-token* strlen)) :do
	   (write-char #\Space stream)))
       (loop :repeat (min probability 64) :do
	 (write-char #\* stream))
       (terpri stream))))
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
(defparameter *longest-token* nil)
(defun gettoken (n)
  (aref *tokens* n))
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
    #+nil
    (sort 
     (remove-if 'partialp
		(coerce tokens
			'list
			))
     'string<)))

(defun longest-token ()
  (reduce 'max *tokens* :key (lambda (x) (length (token-string x))) :initial-value 0))

(/ (* 24000 (* 128 128 4)) 1024 1024 1024 1.0)

(/ (* 512 512 512 4) (* 1024 1024 1024 1.0))

(defun whatmadeof (&optional (string "man"))
  (onlymasks3 (format nil "A ~a is made of [MASK] [MASK], [MASK] [MASK], [MASK] [MASK], and [MASK] [MASK]."
		      string)))
(defun join (&optional (list '( 1 2 3 4 )))
  (format nil "~{~A~^, ~}" list))
(defun maskn (items &optional (andp t))
  (let ((items (mapcar 'mask items)))
    (when andp
      (symbol-macrolet ((laststr (car (last items))))
	(setf laststr (format nil "and ~a." laststr))))
    (join items)))

(defun whatmadeof2 (&optional (string "man") (mask '(2 2 2 2)))
  (onlymasks3 (format nil "~a is made of ~a" string (maskn mask))))

(defun cmd (&optional (obj "man") (question "is made of") (mask '(2 2 2 2)))
  (onlymasks3 (format nil "~a ~a ~a" obj question (maskn mask))))
(defun cm (&optional (thing "man is made of") (mask '(2 2 2 2)))
  (onlymasks3 (format nil "~a ~a" thing (maskn mask))))
