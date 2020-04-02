(defpackage #:wfc
  (:use :cl))
(in-package #:wfc)

;;https://github.com/mxgmn/WaveFunctionCollapse
;;https://robertheaton.com/2018/12/17/wavefunction-collapse-algorithm/
;;https://exutumno.itch.io/wavefunctioncollapse
;;https://bitbucket.org/mxgmn/basic3dwfc/src/master/Model.cs
;;http://graphics.stanford.edu/%7Epmerrell/thesis.pdf
(defun player-pos ()
  (physics::pos sucle::*ent*))
(defparameter *hash* nil)
(defun compute-region (&aux (x0 -20) (y0 22) (z0 -30) (hash (make-hash-table :test 'eq)))
  (declare (optimize (speed 3) (safety 0)))
  (let (;;radius
	(r 50)
	(count 0))
    (declare (type fixnum count))
    (utility:dobox ((x1 (- r) r)
		    (y1 (- r) r)
		    (z1 (- r) r))
      (incf count)
      (let ((x (+ x1 x0))
	    (y (+ y1 y0))
	    (z (+ z1 z0)))
	(let ((3x3 (3x3-neighborhood x y z)))
	  (incf (gethash 3x3 hash 0)))))
    (setf *hash* hash)
    (values hash
	    (locally (declare (optimize (speed 0) safety))
	      (utility:floatify
	       (/ (hash-table-count hash)
		  count))))))


(defun 3x3-neighborhood (x0 y0 z0)
  (declare (optimize (speed 3) (safety 0))
	   (type fixnum x0 y0 z0))
  (let ((acc 0)
	(index 0))
    (declare (type fixnum acc)
	     (type (integer 0 54) index))
    (utility:dobox
	((x (+ -1 x0) (+ 2 x0))
	 (y (+ -1 y0) (+ 2 y0))
	 (z (+ -1 z0) (+ 2 z0)))
      (let ((id (translator
		 (world::getblock x y z))))
	(setf acc (logior acc (the fixnum (ash (the (integer 0 3) id) index)))))
      (incf index 2))
    (values acc)))

(defun explode-3x3 (3x3)
  (let ((array (make-array '(3 3 3)))
	(index 0))
    (utility:dobox
	((x 0 3)
	 (y 0 3)
	 (z 0 3))
      (setf (aref array x y z)
	    (ldb (byte 2 index) 3x3))
      (incf index 2))
    array))

(defparameter *adjacency* (make-hash-table :test 'equal))
(defun add-3x3 (array count &optional (hash *adjacency*))
  (let ((center (aref array 1 1 1)))
    (utility:dobox
	((x 0 3)
	 (y 0 3)
	 (z 0 3))
      (unless (and (= x 1) (= y 1) (= z 1))
	(incf (gethash (list center (aref array x y z) x y z) hash 0)
	      count))))
  (values))

(defun construct-adjacency (&optional (hash *hash*) (adj *adjacency*))
  (utility:dohash
      (3x3 count) hash
      (add-3x3 (explode-3x3 3x3) count adj)))

(defun types ()
  (remove-duplicates 
   (mapcar 'type-of
	   (alexandria:hash-table-keys *hash*))
   :test 'equalp))
(defun freq ()
  (subseq 
   (sort 
    (alexandria:hash-table-alist *hash*)
    '>
    :key 'cdr)
   0 100))
(defun freqadj ()
  (subseq 
   (sort 
    (alexandria:hash-table-alist *adjacency*)
    '>
    :key 'cdr)
   0 ;;100
   ))

;;ground
;;air
;;plant
;;artificial

(defun translator (id)
  (ecase (block-data:data id :name)
    ;;Nothing
    ((:air) 0)
    ;;Land
    ((:grass :dirt :stone :sand :gravel) 1)
    ;;Plants
    ((:leaves :log) 2)
    ;;Artificial
    ((:planks) 3)))
"
bool[][][][] wave;
bool[][][] changes;
int[][][] observed;
double[] stationary;"
(defparameter *fmx* 100)
(defparameter *fmy* 100)
(defparameter *fmz* 100)
;;The number of possible outcomes
;;FIXME
(defparameter *t* 10)
(defparameter *stationary* (make-array *t* :initial-contents (alexandria:iota 10 :start 1)))
(defparameter *logprob* (map 'vector 'log *stationary*))
(defparameter *logt* (log *t*))
(defparameter *wave* (let ((arr (make-array (list *fmx* *fmy* *fmz*))))
		       (dotimes (i (array-total-size arr))
			 (setf (row-major-aref arr i)
			       (make-array *t* :element-type 'bit)))
		       arr))
(defparameter *changes* (make-array (list *fmx* *fmy* *fmz*) :element-type 'bit))
(defparameter *observed* (make-array (list *fmx* *fmy* *fmz*) ;;:element-type 'bit
				     ))
(defconstant +true+ 1)
(defconstant +false+ 0)

(defun observe ()
  (let ((min 1e+3) sum main_sum log_sum noise entropy
	(argminx -1)
	(argminy -1)
	(argminz -1)
	(amount 0)
	w)
    ;;FIXME::dobox code duplicated, iterating over arrays
    ;;Iterate through the box
    (utility:dobox
	((x 0 *fmx*)
	 (y 0 *fmy*)
	 (z 0 *fmz*))
      ;;w is the state at the point in wave     
      (setf w (aref *wave* x y z))
      (setf amount 0)
      (setf sum 0)

      ;;Iterate through the possible outputs
      (dotimes (_t *t*)
	(when (= +true+ (sbit w _t))
	  (incf amount)
	  (incf sum (aref *stationary* _t))))
      (when (zerop sum)
	(return-from observe nil))
      (setf noise (* (random most-positive-double-float) 1e-6))
      (cond ((= amount 1) (setf entropy 0))
	    ((= amount *t*) (setf entropy *logt*))
	    (t
	     (setf main_sum 0)
	     (setf log_sum (log sum))
	     (dotimes (_t *t*)
	       (when (= +true+ (sbit w _t))
		 (incf main_sum (* (aref *stationary* _t)
				   (aref *logprob* _t)))))
	     (setf entropy (- log_sum (/ main_sum sum)))))

      (when (and (plusp entropy)
		 (< (+ entropy noise)
		    min))
	(setf min (+ entropy noise)
	      argminx x
	      argminy y
	      argminz z)))
    ;;Nothing is observed, so end the entire algorithm.
    ;;argminx,y,z are unchanged, so nothing happens, just exit.
    (when (and (= -1 argminx)
	       (= -1 argminy)
	       (= -1 argminz))
      (utility:dobox
	((x 0 *fmx*)
	 (y 0 *fmy*)
	 (z 0 *fmz*))
	(let ((w (aref *wave* x y z)))
	  (block break
	    (dotimes (_t *t*)
	      (when (= +true+ (sbit w _t))
		(setf (aref *observed* x y z) _t)
		(return-from break))))))
      (return-from observe t))
    ;; double[] distribution = new double[T];
    ;;         //Stationary[T] contains weights?
    ;; 	for (int t = 0; t < T; t++) distribution[t] = wave[argminx][argminy][argminz][t] ? stationary[t] : 0;
    ;; // selects a random element of the array?
    ;; 	int r = distribution.Random(random.NextDouble());
    ;;         //Collapse the lowest entropy spot?
    ;; 	for (int t = 0; t < T; t++) wave[argminx][argminy][argminz][t] = t == r;
    ;; //;Same as above?
    ;; 	for (int t = 0; t < T; t++) wave[argminx][argminy][argminz][t] = (t == r);
    ;; 	changes[argminx][argminy][argminz] = true;

    ;; 	return null;
    (let ((w (aref *wave* argminx argminy argminz))
	  distribution)
      (setf distribution
	    (map 'vector (lambda (bit stationary-value)
			   (if (= +true+ bit)
			       stationary-value
			       0))
		 w
		 *stationary*))

      (let ((r (pick-random distribution)))
	(dotimes (_t *t*)
	  (setf (sbit w _t) (if (= _t r) +true+ +false+))))

      (setf (aref *changes* argminx argminy argminz) t)
      (return-from observe (values nil)))))

(defun pick-random (&optional (distribution #(1 0 20 3 0)))
  (let* ((total (reduce '+ distribution))
	 (stop (random (+ 0.0 total))))
    (dotimes (i (length distribution))
      (let ((probability (aref distribution i)))
	(decf stop probability)
	(when (<= stop 0)
	  (return-from pick-random i))))))

"
bool? Observe()
	{
		double min = 1E+3, sum, mainSum, logSum, noise, entropy;
		int argminx = -1, argminy = -1, argminz = -1, amount;
		bool[] w;

                //Iterate through the box
		for (int x = 0; x < FMX; x++) for (int y = 0; y < FMY; y++) for (int z = 0; z < FMZ; z++)
				{
					w = wave[x][y][z];
                                        //w is the state at the point in wave     
					amount = 0;
					sum = 0;
                                       
                                        //Iterate through the possible outputs
					for (int t = 0; t < T; t++) if (w[t])
						{
							amount += 1;
                                                        //is this the weight?
							sum += stationary[t];
						}
                                        
					if (sum == 0) return false;

					noise = 1E-6 * random.NextDouble();

					if (amount == 1) entropy = 0;
					else if (amount == T) entropy = logT;
					else
					{
						mainSum = 0;
						logSum = Math.Log(sum);
						for (int t = 0; t < T; t++) if (w[t]) mainSum += stationary[t] * logProb[t];
						entropy = logSum - mainSum / sum;
					}

					if (entropy > 0 && entropy + noise < min)
					{
						min = entropy + noise;
						argminx = x;
						argminy = y;
						argminz = z;
					}
				}

                //Nothing is observed, so end the entire algorithm.
                //argminx,y,z are unchanged, so nothing happens, just exit.
		if (argminx == -1 && argminy == -1 && argminz == -1)
		{
			for (int x = 0; x < FMX; x++) for (int y = 0; y < FMY; y++) for (int z = 0; z < FMZ; z++) for (int t = 0; t < T; t++) if (wave[x][y][z][t])
							{
								observed[x][y][z] = t;
								break;
							}						

			return true;
		}		

		double[] distribution = new double[T];
                //Stationary[T] contains weights?
		for (int t = 0; t < T; t++) distribution[t] = wave[argminx][argminy][argminz][t] ? stationary[t] : 0;
		int r = distribution.Random(random.NextDouble());
                //Collapse the lowest entropy spot?
		for (int t = 0; t < T; t++) wave[argminx][argminy][argminz][t] = t == r;
		changes[argminx][argminy][argminz] = true;

		return null;
	}
"

(defun propagate ())

"
	bool Propagate()
	{
		bool change = false, b;
                //Iterate across every voxel and the 6 sides of the cube
		for (int x2 = 0; x2 < FMX; x2++) for (int y2 = 0; y2 < FMY; y2++) for (int z2 = 0; z2 < FMZ; z2++) for (int d = 0; d < 6; d++)
					{
						int x1 = x2, y1 = y2, z1 = z2;
						if (d == 0)
						{
                                                        //For edge
							if (x2 == 0)
							{
								if (!periodic) continue;
								else x1 = FMX - 1;
							}
							else x1 = x2 - 1;
						}
						else if (d == 1)
						{
                                                        //For edge
							if (y2 == FMY - 1)
							{
								if (!periodic) continue;
								else y1 = 0;
							}
							else y1 = y2 + 1;
						}
						else if (d == 2)
						{
                                                        //For edge
							if (x2 == FMX - 1)
							{
								if (!periodic) continue;
								else x1 = 0;
							}
							else x1 = x2 + 1;
						}
						else if (d == 3)
						{
                                                        //For edge
							if (y2 == 0)
							{
								if (!periodic) continue;
								else y1 = FMY - 1;
							}
							else y1 = y2 - 1;
						}
						else if (d == 4)
						{
                                                        //For edge
							if (z2 == FMZ - 1)
							{
								if (!periodic) continue;
								else z1 = 0;
							}
							else z1 = z2 + 1;
						}
						else
						{
                                                        //For edge/ wrap-around
							if (z2 == 0)
							{
								if (!periodic) continue;
								else z1 = FMZ - 1;
							}
							else z1 = z2 - 1;
						}
                                                //This spot did not change, so skip it.
						if (!changes[x1][y1][z1]) continue;

						bool[] w1 = wave[x1][y1][z1];
						bool[] w2 = wave[x2][y2][z2];
                                                //Here we compare w1 to w2
                                                //w2 is the block examined, w1 is the adjacent block
						for (int t2 = 0; t2 < T; t2++) if (w2[t2])
							{
                                                                //d is the side
								bool[] prop = propagator[d][t2];
								b = false;

								for (int t1 = 0; t1 < T && !b; t1++) if (w1[t1]) b = prop[t1];
								if (!b)
								{
									w2[t2] = false;
									changes[x2][y2][z2] = true;
									change = true;
								}
							}
					}				
                //When propagations are over
		return change;
	}
"
(defun run ()

  (clear)
  (loop :named out :do
     (let ((result (observe)))
       (when result
	 (return-from out))
       (loop :while (Propagate)))))

"
	public bool Run(int seed)
	{
		logT = Math.Log(T);
		logProb = new double[T];
		for (int t = 0; t < T; t++) logProb[t] = Math.Log(stationary[t]);

		Clear();

		random = new Random(seed);

		while (true)
		{
			bool? result = Observe();
			if (result != null) return (bool)result;
			while (Propagate()) ;
		}
	}
"
(defun clear ()
  (utility:dobox
      ((x 0 *fmx*)
       (y 0 *fmy*)
       (z 0 *fmz*))
    (dotimes (_t *t*)
      (fill (aref *wave* x y z) +true+)
      (setf (sbit *changes* x y z) +false+)))
  ;;Ground not implemented
  )
"
	void Clear()
	{
		for (int x = 0; x < FMX; x++) for (int y = 0; y < FMY; y++) for (int z = 0; z < FMZ; z++)
				{
					for (int t = 0; t < T; t++) wave[x][y][z][t] = true;
					changes[x][y][z] = false;
				}
		
		if (ground >= 0)
		{
			for (int x = 0; x < FMX; x++) for (int y = 0; y < FMY; y++)
				{
					for (int t = 0; t < T; t++) if (t != ground) wave[x][y][FMZ - 1][t] = false;
					changes[x][y][FMZ - 1] = true;

					for (int z = 0; z < FMZ - 1; z++)
					{
						wave[x][y][z][ground] = false;
						changes[x][y][z] = true;
					}
				}			
		}	
	}
"
