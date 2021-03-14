;;;; hug.lisp

(in-package #:hug)

(defun setup ()
  (py4cl2:pyexec
   "
import torch
from transformers import BertTokenizer, BertModel,BertForMaskedLM
tokenizer = BertTokenizer.from_pretrained('bert-base-cased')
model = BertForMaskedLM.from_pretrained('bert-base-cased')   
"
   ))

(defun results
    (&optional (str "[MASK] [MASK] [MASK] of the United States mismanagement of the Coronavirus is its distrust of science."))
  (setf (py4cl2:pyeval "input_txt")
	(py4cl2:pythonize str))
  (py4cl2:pyexec
   "
inputs = tokenizer(input_txt, return_tensors='pt')
outputs = model(**inputs)
predictions = outputs[0]
sorted_preds, sorted_idx = predictions[0].sort(dim=-1, descending=True)
for k in range(10):
   predicted_index = [sorted_idx[i, k].item() for i in range(0,24)]
   predicted_token = [tokenizer.convert_ids_to_tokens([predicted_index[x]])[0] for x in range(1,24)]
   print(predicted_token)"
   ))
