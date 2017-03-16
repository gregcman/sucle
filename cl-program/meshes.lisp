(in-package :sandbox)

(defmacro vvv (darkness u v x y z)
  `(progn (%gl:vertex-attrib-1f 8 ,darkness)
	  (%gl:vertex-attrib-2f 2 ,u ,v)
	  (%gl:vertex-attrib-3f 0 ,x ,y ,z)))

(defun draw-background ()
  (let ((distance 0.99999997))
    (gl:with-primitives :quads
      (vvv 1.0 0.0 0.0 -1.0 -1.0 distance)
      (vvv 1.0 1.0 0.0 1.0 -1.0 distance)
      (vvv 1.0 1.0 1.0 1.0 1.0 distance)
      (vvv 1.0 0.0 1.0 -1.0 1.0 distance))))

(defun draw-skybox ()
  (let ((h0 0.0)
	(h1 (/ 1.0 3.0))
	(h2 (/ 2.0 3.0))
	(h3 (/ 3.0 3.0))
	(w0 0.0)
	(w1 (/ 1.0 4.0))
	(w2 (/ 2.0 4.0))
	(w3 (/ 3.0 4.0))
	(w4 (/ 4.0 4.0)))
    (let ((neg -128.0)
	  (pos 128.0))
      (gl:with-primitives :quads
	;;j+
	(vvv 1.0 w2 h3 neg pos neg)
	(vvv 1.0 w2 h2 pos pos neg)
	(vvv 1.0 w1 h2 pos pos pos)
	(vvv 1.0 w1 h3 neg pos pos)

	;;j-
	(vvv 1.0 w2 h0 neg neg neg)
	(vvv 1.0 w1 h0 neg neg pos)
	(vvv 1.0 w1 h1 pos neg pos)
	(vvv 1.0 w2 h1 pos neg neg)

	;;k-
	(vvv 1.0 w3 h2 neg pos neg)
	(vvv 1.0 w3 h1 neg neg neg)
	(vvv 1.0 w2 h1 pos neg neg)
	(vvv 1.0 w2 h2 pos pos neg)

	;;k+
	(vvv 1.0 w1 h1 pos neg pos)
	(vvv 1.0 w0 h1 neg neg pos)
	(vvv 1.0 w0 h2 neg pos pos)
	(vvv 1.0 w1 h2 pos pos pos)
	
	;;i-
	(vvv 1.0 w3 h1 neg neg neg)
	(vvv 1.0 w3 h2 neg pos neg)
	(vvv 1.0 w4 h2 neg pos pos)
	(vvv 1.0 w4 h1 neg neg pos)

	;;i+
	(vvv 1.0 w2 h1 pos neg neg)
	(vvv 1.0 w1 h1 pos neg pos)
	(vvv 1.0 w1 h2 pos pos pos)
	(vvv 1.0 w2 h2 pos pos neg)))))

(defun draw-sun ()
  (let ((distance 1.0))
    (gl:with-primitives :quads
      (vvv 1.0 1.0 1.0  1.0 -1.0 distance)
      (vvv 1.0 1.0 0.0 -1.0 -1.0 distance)
      (vvv 1.0 0.0 0.0 -1.0  1.0 distance)
      (vvv 1.0 0.0 1.0  1.0  1.0 distance))))

(defun draw-moon ()
  (let ((distance -1.0))
    (gl:with-primitives :quads
      (vvv 1.0 1.0 1.0  -1.0 1.0 distance)
      (vvv 1.0 1.0 0.0 -1.0 -1.0 distance)
      (vvv 1.0 0.0 0.0 1.0  -1.0 distance)
      (vvv 1.0 0.0 1.0  1.0  1.0 distance))))

(defun draw-box (minx miny minz maxx maxy maxz)
  (let ((h0 0.0)
	(h1 (/ 1.0 3.0))
	(h2 (/ 2.0 3.0))
	(h3 (/ 3.0 3.0))
	(w0 0.0)
	(w1 (/ 1.0 4.0))
	(w2 (/ 2.0 4.0))
	(w3 (/ 3.0 4.0))
	(w4 (/ 4.0 4.0)))
    (gl:with-primitives :quads
      (vvv 0.0 w2 h3 minx maxy minz)
      (vvv 0.0 w2 h2 maxx maxy minz)
      (vvv 0.0 w1 h2 maxx maxy maxz)
      (vvv 0.0 w1 h3 minx maxy maxz)

      ;;j-
      (vvv 0.0 w2 h0 minx miny minz)
      (vvv 0.0 w1 h0 minx miny maxz)
      (vvv 0.0 w1 h1 maxx miny maxz)
      (vvv 0.0 w2 h1 maxx miny minz)

      ;;k-
      (vvv 0.0 w3 h2 minx maxy minz)
      (vvv 0.0 w3 h1 minx miny minz)
      (vvv 0.0 w2 h1 maxx miny minz)
      (vvv 0.0 w2 h2 maxx maxy minz)

      ;;k+
      (vvv 0.0 w1 h1 maxx miny maxz)
      (vvv 0.0 w0 h1 minx miny maxz)
      (vvv 0.0 w0 h2 minx maxy maxz)
      (vvv 0.0 w1 h2 maxx maxy maxz)
      
      ;;i-
      (vvv 0.0 w3 h1 minx miny minz)
      (vvv 0.0 w3 h2 minx maxy minz)
      (vvv 0.0 w4 h2 minx maxy maxz)
      (vvv 0.0 w4 h1 minx miny maxz)

      ;;i+
      (vvv 0.0 w2 h1 maxx miny minz)
      (vvv 0.0 w1 h1 maxx miny maxz)
      (vvv 0.0 w1 h2 maxx maxy maxz)
      (vvv 0.0 w2 h2 maxx maxy minz))))


(defun draw-hotbar ()
  (let ((distance 0.0))
    (let ((top-tex 1.0)
	  (bot-tex (/ (- 256.0 22.0) 256.0))
	  (lef-tex 0.0)
	  (rig-tex (/ 182.0 256.0)))
      (let ((wid (/ (1+ (* 4.5 *hotbar-box-size*)) e:*width*)))
	(let ((top-ndc (+ -1.0 (/ *hotbar-box-size* e:*height*)))
	      (bot-ndc -1.0)
	      (lef-ndc (- 0.0 wid))
	      (rig-ndc (+ 0.0 wid)))	  
	  (gl:with-primitives :quads
	    (vvv 1.0
		 lef-tex bot-tex 
		 lef-ndc bot-ndc 
		 distance)
	    (vvv 1.0
		 rig-tex bot-tex
		 rig-ndc bot-ndc
		 distance)
	    (vvv 1.0
		 rig-tex top-tex
		 rig-ndc top-ndc
		 distance)
	    (vvv 1.0
		 lef-tex top-tex
		 lef-ndc top-ndc
		 distance)))))))

(defun draw-hotbar-selector ()
  (let ((distance 0.0))
    (let ((top-tex (/ (- 256.0 22.0) 256.0))
	  (bot-tex (/ (- 256.0 46.0) 256.0))
	  (lef-tex 0.0)
	  (rig-tex (/ 24.0 256.0)))
      (let ((wid (/ (+ 8 *hotbar-box-size*) e:*width* 2))
	    (offset (/ (* (- *hotbar-selection* 4) *hotbar-box-size*) e:*width*)))
	(let ((top-ndc (+ -1.0 (/ (+ 2.0 *hotbar-box-size*) e:*height* )))
	      (bot-ndc (- -1.0 (/ 2.0 e:*height*)))
	      (lef-ndc (+ offset (- wid)))
	      (rig-ndc (+ offset wid)))	  
	  (gl:with-primitives :quads
	    (vvv 1.0
		 lef-tex bot-tex 
		 lef-ndc bot-ndc 
		 distance)
	    (vvv 1.0
		 rig-tex bot-tex
		 rig-ndc bot-ndc
		 distance)
	    (vvv 1.0
		 rig-tex top-tex
		 rig-ndc top-ndc
		 distance)
	    (vvv 1.0
		 lef-tex top-tex
		 lef-ndc top-ndc
		 distance)))))))

(defun mesh-crosshair ()
  (let ((distance 0.0))
    (let ((top-tex (/ 253.0 256.0))
	  (bot-tex (/ 244.0 256.0))
	  (lef-tex (/ 243.0 256.0))
	  (rig-tex (/ 252.0 256.0)))
      (let ((wid (/ *crosshair-size* e:*width*))
	    (hei (/ *crosshair-size* e:*height*)))
	(let ((top-ndc (+ hei))
	      (bot-ndc (- hei))
	      (lef-ndc (+ wid))
	      (rig-ndc (- wid)))	  
	  (gl:with-primitives :quads
	    (vvv 1.0
		 lef-tex bot-tex 
		 lef-ndc bot-ndc 
		 distance)
	    (vvv 1.0
		 rig-tex bot-tex
		 rig-ndc bot-ndc
		 distance)
	    (vvv 1.0
		 rig-tex top-tex
		 rig-ndc top-ndc
		 distance)
	    (vvv 1.0
		 lef-tex top-tex
		 lef-ndc top-ndc
		 distance)))))))
