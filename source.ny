;nyquist plug-in
;version 0
;type process
;name "delay"
;action "-*-.-working-.-*-"
;info "Basic Nyquest delay.\nMakes a number of echoes."
;control decay "decay" int "dB" 6 0 24
;control delay "delay" real "seconds" 0.5 0.0 5.0
;control count "echoes" int "times" 5 1 30

(defun delays (s decay delay count)
(if (= count 0) (cue s)
   (sim (cue s)
	(loud decay (at delay (delays s decay delay (- count 1)))))))
(stretch-abs 1 (delays s (- 0 decay) delay count))





;nyquist plug-in
;version 0
;type generate
;name "freq shift"
;action "-*-.-working-.-*-"
;info "Basic Nyquist sweep."
;control startf "start freq (Hz)" real "" 20 20 20000
;control endf "end freq (Hz)" real "" 20000 20 20000
;control duration "duration (secs)" real "" 30 1 300
;control level "level (dBFS)" real " " 0 -40 0
;control type "Sweep scale [1=Linear, 2=Exponential]" int "" 2 1 2
 
(if (= type 2)
(scale-db level (fmosc 0 (pwev startf duration endf))) 
(scale-db level (fmosc 0 (pwlv startf duration endf))) )





;nyquist plug-in
;version 0
;type process
;name "High Pass LFO"
;action "-*-.-working-.-*-"
;info "Basic High Pass LFO (Low Frequency Oscillator)"

;control f "LFO freq" real "Hz" 0.20 0.00 20.00 
;control lo-f "low cutoff freq" int "hz" 160 20 20000
;control hi-f "high cutoff freq" int "hz" 2560 20 20000
;control phase "LFO start phase" int "degrees" 0 -180 180

(setf factor (abs (- hi-f lo-f)))
(setf center (/ factor 2.0))

(defun get-lfo (f phase factor center)
(sum center (mult factor
	(sum 0.5 (mult 0.5 (lfo f 1.0 *sine-table* phase))))))

(defun normalize (signal)
(setf x (if (arrayp signal)
(max (peak (aref signal 0) ny:all) (peak (aref signal 1) ny:all))
(peak signal ny:all)))
(scale (/ 0.95 x) signal))

(normalize (hp s (get-lfo f phase factor center)))