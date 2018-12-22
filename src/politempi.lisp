
(in-package :om)
 
(defclass! polytempo (simple-container)
  ((duration :initarg :duration :accessor duration :initform 6000)
   (tempocurves :initarg :tempocurves :accessor tempocurves :initform nil)
   (subdivisions :initarg :subdivisions :accessor subdivisions :initform '(1 2 3 4))
   (main-voice :initarg :main-voice :accessor main-voice :initform nil)
   (merge-lines :accessor merge-lines :initarg :merge-lines :initform t)
   (lines :accessor lines :initform nil)
   (mseq :accessor mseq :initform nil))
  (:icon '(225)))

(defmethod initialize-instance :after ((self polytempo) &rest args)
  (setf (lines self) (compute-lines self))
  (update-mseq self))

(defun make-cs-lines-split (lines)
  (loop for v in lines append
        (loop for line in v for i = 0 then (1+ i)  collect
              (let ((cs (make-instance 'chord-seq 
                                       :lmidic (make-list (length line) :initial-element 7100)
                                       :lonset line
                                       :ldur 100
                                       :lchan 1)))
                (add-extra cs (make-instance 'head-extra :thehead "i") nil)
                cs
                ))))

(defun get-line-pitches (n)
  (case n
    (1 '(7100))
    (2 '(7700 6400))
    (3 '(7700 7100 6400))
    (4 '(7600 7200 6900 6500))
    (5 '(7700 7400 7100  6700 6400))
    (otherwise (make-list n :initial-element 7100))
    ))

(defun make-cs-lines-merged (lines)
  (loop for v in lines collect
        (let* ((pitches (get-line-pitches (length v)))
               (cs (reduce #'merger
                           (loop for line in v 
                                 for i = 0 then (1+ i) collect
                                 (make-instance 'chord-seq 
                                                :lmidic (make-list (length line) :initial-element (nth i pitches))
                                                :lonset line
                                                :ldur 100
                                                :lchan 1)))
                     ))
          ;(setf (lmidic cs) (loop for c in (lmidic cs) collect (last (sort c #'<))))
          (add-extra cs (make-instance 'head-extra :thehead "i") nil)
          cs
          )))

(defun update-mseq (self)
  (when (lines self)
    (setf (mseq self)
          (make-instance 'multi-seq 
                         :chord-seqs (cons 
                                      (let ((v1 (or (main-voice self) (make-instance 'chord-seq :lmidic nil))))
                                        (add-extra v1 (make-instance 'head-extra :thehead "e") nil)
                                        v1)
                                      (if (merge-lines self)
                                          (make-cs-lines-merged (lines self))
                                        (make-cs-lines-split (lines self))))
                         ))
    (set-mus-color self *om-gray-color*)))
 

(defmethod compute-lines ((self polytempo))
  (loop for tc in (tempocurves self) collect
        (loop for sd in (subdivisions self) 
              for i = 0 then (1+ i) collect
              (compute-line tc sd i (duration self)))))

;; here subdiv is a real subdivision
(defmethod compute-line ((tempocurve bpf) subdiv n duration)
  (declare (ignore n))
  (let ((currtime 0))
    (cons currtime
          (loop while (<= currtime duration) collect
                (let* ((currtempo (x-transfer tempocurve currtime))
                       (curr-subdiv-duration (/ 60000 (* currtempo subdiv))))
                  (setf currtime (round (+ currtime curr-subdiv-duration))))))))

;; here we take n = index in the list
;; the onsets are already computed
(defmethod compute-line ((tempocurve textfile) subdiv n duration)
  (declare (ignore subdiv))
  (let* ((mylist (nth n (list-of-data (buffer-text tempocurve))))
         (onsets (sec->ms (car (mat-trans (group-list mylist 3 'circular))))))
    (subseq onsets 0 (position duration onsets :test '<))))




;;; EDITOR 

(defclass polytempo-editor (multiseqeditor) 
  ((nvpl :accessor nvpl :initform 1 :initarg :nvpl :documentation "number of voices per line" )))

(defclass polytempo-panel (multiseqpanel) ())
(defmethod get-score-class-panel ((self polytempo-editor)) 'polytempo-panel)

(defmethod class-has-editor-p ((self polytempo)) t)
(defmethod get-editor-class ((self polytempo)) 'polytempo-editor)

(defmethod make-editor-window ((class (eql 'polytempo-editor)) (object polytempo) name ref &key 
                               winsize winpos (close-p t) (winshow t) (resize t) (retain-scroll nil)
                               (wintype nil))
  (if (mseq object)
      (let ((editorwin (call-next-method class (mseq object) name ref :winsize winsize :winpos winpos :resize resize 
                        :close-p close-p :winshow winshow :resize resize
                        :retain-scroll retain-scroll :wintype wintype)))
        (set-obj-mode (editor editorwin) 0)
        (set-layout (editor editorwin) object)
        editorwin)
    (progn (om-beep)
      nil)))

(defmethod om-drag-start ((view polytempo-panel)) nil)

(defmethod update-editor-after-eval ((self polytempo-editor) (val polytempo))
  (if (mseq val)
      (progn
        (call-next-method self (mseq val))
        (set-layout self val))
    (om-close-window (window self))))

(defmethod get-default-score-params ((self polytempo))
  (pairlis  '(staff show-stems fontsize) 
            '(empty nil 16)
           (call-next-method)))

(defmethod set-layout ((self polytempo-editor) polytempo)
  (score-top-margin (panel self) 0) 
  (setf (nvpl self) (if (merge-lines polytempo) 1 (length (subdivisions polytempo))))
  (score-system-space (panel self) 
                      (cons 2 (loop for v in (tempocurves polytempo) append 
                                    (append 
                                     (if (merge-lines polytempo) 
                                         '(1)
                                       (append (make-list (1- (nvpl self)) :initial-element 1) '(2)))
                                     )))))

   
(defmethod select-note ((self polytempo-panel) note)
  (if (= 0 (click-in-which-voice? self (om-mouse-position self)))
      (setf (cursor-pos self) (offset->ms (reference note)))
    (call-next-method)))

(defmethod draw-view-contents ((self polytempo-panel))
   (call-next-method)
   (let ((cursor-pos-pix (time-to-pixels self (cursor-pos self))))
     (om-with-fg-color self *om-red2-color*
       (om-with-dashline 
           (om-with-line-size 1.5
             (om-draw-line cursor-pos-pix 0 cursor-pos-pix (h self)))))))
   

(defmethod draw-object ((self grap-multiseq) (view polytempo-panel) x y zoom minx maxx miny maxy slot size linear? staff grille-p chnote)
  (setf (car *internal-score-fonts*) (om-make-font-object (om-make-music-font *heads-font* 16)))
  (unless (= (nvpl (editor view)) 1) 
    (let ((posy y) (nvpl (nvpl (editor view))))
      (loop for item in (inside self)
            for i = 0 then (+ i 1)
            for system in staff do
            (when (= (mod (1- i) nvpl) 0)
              (let ((endxms (* (ceiling (pixel2ms (- maxx x)  (/ size 4) zoom) 1000) 1000))
                    (xms 0))
                (om-with-fg-color view *om-gray-color* 
                  (om-with-line '(2 2)
                    (loop for gchord in (inside item) while (<= xms endxms) do
                          (let ((posx (+ x (ms2pixel (offset->ms (reference gchord)) (/ size 4) zoom))))
                            (om-draw-line posx posy posx (+ posy -20 (* nvpl (get-delta-system system size view i)))))))))
              )
            (setf posy (+ posy (get-delta-system system size view i))))))
  (call-next-method))
  

;(defmethod scorepanel-draw-object ((self polytempo-panel) x0 y0 deltax deltay size)
;  (call-next-method))

;(defmethod draw-system-only ((self polytempo-panel) &optional printinfo)
;  (call-next-method))


(defmethod handle-key-event ((self polytempo-panel) char)
  (case char
    (#\q (when (selection? self) 
           (select-unselect-pulse (selection? self))
           (om-invalidate-view self t)))
    (otherwise (call-next-method))))

(defmethod pulse-selected ((self container)) 
  (find t (mapcar 'pulse-selected (inside self))))

(defmethod pulse-selected ((self note)) 
  (= (chan self) 2))
 
(defmethod select-unselect-pulse (selection)
  (loop for item in selection do
        (if (pulse-selected item)
            ;;; unselect
            (progn (set-channel item 1)
              (set-mus-color item nil))
          ;;; select
          (progn (set-channel item 2)
              (set-mus-color item *om-red-color*))
          )))



;;;=======================
;;; GET THE RHYTHM
;;;=======================
(defmethod create-cseqs-from-line ((self chord-seq) pitches)
  (loop for p in pitches collect 
        (let (mm oo cc)
          (loop for m in (lmidic self)
                for o in (lonset self)
                for c in (lchan self) do
                (let ((pos (position p m :test '=)))
                  (when pos
                    (push 7100 mm)
                    (push o oo)
                    (push (nth pos c) cc))))
          (make-instance 'chord-seq :lmidic (reverse mm) :lonset (reverse oo) :lchan (reverse cc)))
        ))
      
(defmethod! get-rhythmic-lines ((self polytempo))
   :icon '(225)
   (let ((groups (if (merge-lines self) 
                     (loop for cs in (cdr (chord-seqs (mseq self))) collect (create-cseqs-from-line cs (get-line-pitches (length (subdivisions self)))))
                   (group-list (cdr (chord-seqs (mseq self))) (length (subdivisions self)) 'circular))))
     (loop for g in groups collect
           (loop for cs in g 
                 for sd in (subdivisions self)
                 collect
                 (make-instance 'voice :tree (reducetree (mktree (loop for c in (inside cs) collect
                                                                       (if (pulse-selected c) (/ 1 (* 4 sd)) (- (/ 1 (* 4 sd)))))
                                                                 '(4 4)))
                                :chords (mapcar #'(lambda (chord) (set-mus-color chord *om-black-color*) chord) (remove-if-not 'pulse-selected (get-chords cs))))))
     ))



     
