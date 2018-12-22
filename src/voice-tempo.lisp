
;;; UTILS FOR DEaLING WITH TEMPO CHANGES IN VOICES

(in-package :om)

;;; VOICE DISPLAY PROPORTIONNAL

(defmethod get-view-object ((self miniview)) (value (object (om-view-container self))))
(defmethod get-view-object ((self scorepanel)) (object (editor self)))

(defmethod time-to-pixels ((self voicepanel) time-ms)
  (+ (get-key-space self) (staff-size self) (* 0.1 time-ms (staff-zoom self))))
(defmethod time-to-pixels ((self polypanel) time-ms)
  (+ (get-key-space self) (staff-size self) (* 0.1 time-ms (staff-zoom self))))

; => HERE
;(defmethod draw-object-ryth :before ((self simple-graph-container) view x y zoom minx maxx miny maxy slot size linear? staff chnote) 
;  (setf (main-point self)
;        (list (* 0.1 (offset->ms (reference self) (get-view-object view)))
;              (cadr (main-point self)))))

(defmethod offset->ms ((self chord) &optional grandparent)
  (+ (call-next-method)
     (or (and grandparent (getf (symb-info (car (inside self))) :offset))
         0)))

(defmethod offset->ms ((self note) &optional grandparent)
  (+ (call-next-method)
     (or (getf (symb-info self) :offset) 0))) 

;(defmethod get-chiffrage-space ((self grap-measure) size)
;   (if (or (show-chifrage self) (first-of-group? self)) size
;     (let ((previous-mes (nth (- (position self (inside (parent self)) :test 'equal) 1) (inside (parent self)))))
;      (mesure-space self size (not (equal (metric self) (metric previous-mes)))))))

;(defmethod mesure-space ((self grap-measure) size &optional metric) size)
; (if metric size (round size 1)))


(defmethod! format-tempo-list ((voice voice) beat-per-mesure beats-tempo-list)
  :initvals '(nil 4 nil)
  :doc "Formats the list of tempo as a suitable spec for voice 'tempo' input. Returns a voice"
  (flet ((1-beat-to-tempo-change (voice-ratios beatnum tempo beat-per-mesure)
           (multiple-value-bind (m b) (floor beatnum beat-per-mesure)
             (let* ((ratios (nth m voice-ratios))
                    (beat-pos (loop for pulse in ratios 
                                    for n = 0 then (1+ n)
                                    sum pulse into total
                                    while (<= total (/ b beat-per-mesure))
                                    finally return n)))
               (list (list m beat-pos) (list 1/4 tempo nil))))))
    (let* ((voice-ratios (tree-to-ratios (tree voice)))
           (tempolist (list (car (tempo voice))
                            (loop for tempo in beats-tempo-list
                                  for beat = 0 then (1+ beat)
                                  with current
                                  do (setf current (1-beat-to-tempo-change voice-ratios beat tempo beat-per-mesure))
                                  unless (equal (car (car (last res))) (car current))
                                  collect current into res
                                  finally return res
                                  ))))
      (make-instance 'voice :tree (tree voice) :chords (chords voice) :tempo tempolist))))


(defun beat-position (b ratios bpmes)
  (loop for pulse in ratios 
        for p = 0 then (1+ p)
        sum (abs pulse) into total
        while (<= total (/ b bpmes))
        finally return p))

(defmethod! set-tempo-changes ((voice voice) beat-tempos beat-onsets)
  :initvals '(nil 4 nil)
  :doc ""
  (let* ((beats-per-mesure (or (caaar (second (tree voice))) 4))  ;;; generally = 4
         (voice-ratios (tree-to-ratios (tree voice)))     ;;; ((1/4 -1/4 1/8 ...) (1/8 ... ) ...)
         ;(chords (loop for m in (inside voice) collect (get-chords m))))
         (onset-corrections nil)
         (tempochanges 
          (list (car (tempo voice))            
                (remove-duplicates 
                 (remove nil
                         (loop for tempo in beat-tempos
                               for realonset in beat-onsets
                               for beat = 0 then (1+ beat)
                               collect 
                               (multiple-value-bind (m b) 
                                   (floor beat beats-per-mesure)
                                 ;(print (list "mesure" m "beat" b))
                                 (when (< m (length voice-ratios))
                                   (let* ((mesure-ratios (nth m voice-ratios))
                                          (beat-pos (beat-position b mesure-ratios beats-per-mesure)))
                                     (when (plusp (nth beat-pos mesure-ratios))
                                       (let* ((ratios-so-far (first-n mesure-ratios (1+ beat-pos)))
                                              (pos2 (1- (length (remove-if 'minusp ratios-so-far)))))
                                         (push (list m pos2 realonset tempo) onset-corrections)
                                         ))
                                     (list (list m beat-pos) (list 1/4 tempo t))))
                                 )))
                 :test 'equal :key #'car :from-end t)))
                )
    (let* ((voice2 (make-instance 'voice :tree (tree voice)
                                  :chords (get-chords voice)
                                  :tempo tempochanges))
           (chords (loop for m in (inside voice2) collect 
                         (remove-if-not 'chord-p (collect-chords m)))))
      (loop for b in (reverse onset-corrections) do
            (let* ((chord (nth (cadr b) (nth (car b) chords)))
                   (diff (- (caddr b) (offset->ms chord voice2))))  
              ;(print (list chord (offset->ms chord voice2) (caddr b)))
              ;(setf (lchan chord) '(2))
              (setf (symb-info (car (inside chord))) (list :offset diff))
              (add-extra chord 
                         (make-instance 'text-extra :deltay 2 :thetext (number-to-string diff))
                         nil)
              
              ))
      (let ((last-offset 0))
        (loop for c in (collect-chords voice2) 
              when (chord-p c) do
              (if (getf (symb-info (car (inside c))) :offset)
                  (setf last-offset (getf (symb-info (car (inside c))) :offset))
                (setf (symb-info (car (inside c))) (list :offset last-offset))))
        )

      voice2
      )))
   
