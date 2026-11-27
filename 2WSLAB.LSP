;;; 2WSLAB.LSP - preliminary two-way slab reinforcement detailing helper
;;; Copyright (c) 2026 mrislamce97
;;; AutoCAD 2021-2026; Visual LISP / Windows AutoCAD.
;;;
;;; SAFETY: This is a drafting and reinforcement-area bookkeeping aid, NOT a
;;; structural analysis/design program. It does not determine loads, moments,
;;; shear, deflection, development/lap lengths, seismic requirements, or code
;;; compliance. A qualified structural engineer must design and approve the
;;; reinforcement and all details against the adopted BNBC 2020/ACI edition.
;;; Drawing uses the rectangular plan extents only. See README for limitations.

(vl-load-com)

(setq *2w-old-error* nil *2w-old-osmode* nil *2w-old-clayer* nil)

(defun 2w:restore-vars ()
  (if (and *2w-old-osmode* (numberp *2w-old-osmode*)) (setvar "OSMODE" *2w-old-osmode*))
  (if (and *2w-old-clayer* (tblsearch "LAYER" *2w-old-clayer*)) (setvar "CLAYER" *2w-old-clayer*))
  (princ)
)

(defun 2w:error (msg)
  (2w:restore-vars)
  (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*")))
    (princ (strcat "\n2WSLAB error: " msg)))
  (princ)
)

(defun 2w:positive (prompt default / val)
  (initget 6)
  (setq val (getreal (strcat "\n" prompt " <" (rtos default 2 3) ">: ")))
  (if val val default)
)

(defun 2w:yesno (prompt default / answer)
  (initget "Yes No")
  (setq answer (getkword (strcat "\n" prompt " [Yes/No] <" default ">: ")))
  (if answer answer default)
)

(defun 2w:layer (name color)
  (if (not (tblsearch "LAYER" name))
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord") (cons 2 name) '(70 . 0) (cons 62 color)
                   '(6 . "Continuous"))))
)

(defun 2w:line (p1 p2 layer)
  (entmake (list '(0 . "LINE") '(100 . "AcDbEntity") (cons 8 layer)
                 '(100 . "AcDbLine") (cons 10 p1) (cons 11 p2)))
)

(defun 2w:text (pt height value layer)
  (entmake (list '(0 . "TEXT") '(100 . "AcDbEntity") (cons 8 layer)
                 '(100 . "AcDbText") (cons 10 pt) (cons 40 height)
                 (cons 1 value) (cons 7 (getvar "TEXTSTYLE")) '(50 . 0.0)))
)

(defun 2w:closed-poly-p (ent / data)
  (setq data (entget ent))
  (and data (member (cdr (assoc 0 data)) '("LWPOLYLINE" "POLYLINE"))
       (= 1 (logand 1 (cdr (assoc 70 data)))))
)

(defun 2w:rect-info (ent / obj lo hi mn mx endp i p x y mask ok)
  ;; Accept only four-corner, axis-aligned, closed rectangular polylines.
  (if (not (2w:closed-poly-p ent))
    nil
    (progn
      (setq obj (vlax-ename->vla-object ent))
      (vla-getBoundingBox obj 'lo 'hi)
      (setq mn (vlax-safearray->list lo) mx (vlax-safearray->list hi))
      (setq endp (fix (vlax-curve-getEndParam ent)) i 0 mask 0 ok (= endp 4))
      (while (and ok (< i endp))
        (setq p (vlax-curve-getPointAtParam ent i) x (car p) y (cadr p))
        (cond
          ((and (equal x (car mn) 1e-6) (equal y (cadr mn) 1e-6)) (setq mask (logior mask 1)))
          ((and (equal x (car mx) 1e-6) (equal y (cadr mn) 1e-6)) (setq mask (logior mask 2)))
          ((and (equal x (car mx) 1e-6) (equal y (cadr mx) 1e-6)) (setq mask (logior mask 4)))
          ((and (equal x (car mn) 1e-6) (equal y (cadr mx) 1e-6)) (setq mask (logior mask 8)))
          (T (setq ok nil)))
        (setq i (1+ i)))
      (if (and ok (= mask 15)) (list (car mn) (cadr mn) (car mx) (cadr mx)) nil)))
)

(defun 2w:select-rect (prompt / ent info)
  (setq ent nil info nil)
  (while (not info)
    (setq ent (car (entsel prompt)))
    (cond
      ((null ent) (setq info 'CANCEL))
      ((setq info (2w:rect-info ent)))
      (T (princ "\nSelect a closed, axis-aligned rectangular polyline with four straight corners."))))
  (if (eq info 'CANCEL) nil (cons ent info))
)

(defun 2w:ss-bbox (ss / i en obj lo hi mn mx boxes)
  (setq i 0 boxes nil)
  (while (< i (sslength ss))
    (setq en (ssname ss i) obj (vlax-ename->vla-object en))
    (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-getBoundingBox (list obj 'lo 'hi)))
      (princ "\nSkipped an entity without a usable bounding box.")
      (progn
        (setq mn (vlax-safearray->list lo) mx (vlax-safearray->list hi))
        (setq boxes (cons (list (car mn) (cadr mn) (car mx) (cadr mx)) boxes))))
    (setq i (1+ i)))
  boxes
)

(defun 2w:edge-beam-p (edge x0 y0 x1 y1 boxes tol / b hit)
  ;; A selected support must lie on/near the named slab edge and span >= half it.
  (setq hit nil)
  (foreach b boxes
    (cond
      ((= edge "Bottom")
       (if (and (<= (abs (- (cadr b) y0)) tol) (<= (abs (- (cadddr b) y0)) tol)
                (> (min x1 (caddr b) (- (max x0 (car b)))) (* 0.5 (- x1 x0)))) (setq hit T)))
      ((= edge "Top")
       (if (and (<= (abs (- (cadddr b) y1)) tol) (<= (abs (- (cadr b) y1)) tol)
                (> (min x1 (caddr b) (- (max x0 (car b)))) (* 0.5 (- x1 x0)))) (setq hit T)))
      ((= edge "Left")
       (if (and (<= (abs (- (car b) x0)) tol) (<= (abs (- (caddr b) x0)) tol)
                (> (min y1 (cadddr b) (- (max y0 (cadr b)))) (* 0.5 (- y1 y0)))) (setq hit T)))
      ((= edge "Right")
       (if (and (<= (abs (- (caddr b) x1)) tol) (<= (abs (- (car b) x1)) tol)
                (> (min y1 (cadddr b) (- (max y0 (cadr b)))) (* 0.5 (- y1 y0)))) (setq hit T)))))
  hit
)

(defun 2w:edge-mode (edge / ans)
  (initget "Continuous Discontinuous")
  (setq ans (getkword (strcat "\nAt supported " edge " edge, does slab continue across the support? [Continuous/Discontinuous] <Discontinuous>: ")))
  (if ans ans "Discontinuous")
)

(defun 2w:cut-ratio (edge mode choice)
  (cond ((= choice "L/4") 0.25) ((= choice "L/5") 0.20)
        ((= mode "Continuous") 0.25) (T 0.20))
)

(defun 2w:dim (p1 p2 loc)
  (command "_.DIMLINEAR" p1 p2 loc)
)

(defun 2w:arrow-tick (pt dx dy size layer)
  (2w:line (list (- (car pt) (* dx size)) (- (cadr pt) (* dy size)) 0.0)
           (list (+ (car pt) (* dx size)) (+ (cadr pt) (* dy size)) 0.0) layer)
)

(defun 2w:get-bar-set (direction span-mm / dia mode value n spacing area req)
  (setq dia (2w:positive (strcat direction " bar diameter (mm)") 12.0))
  (initget "Spacing Count")
  (setq mode (getkword (strcat "\n" direction " input by [Spacing/Count] <Spacing>: ")))
  (if (null mode) (setq mode "Spacing"))
  (if (= mode "Count")
    (progn
      (initget 6)
      (setq n (getint (strcat "\nNumber of " direction " bars across " (rtos span-mm 2 0) " mm (minimum 2): ")))
      (while (or (null n) (< n 2))
        (princ "\nEnter an integer of at least 2.") (initget 6)
        (setq n (getint "\nBar count: ")))
      (setq spacing (/ span-mm (float (1- n)))))
    (progn (setq spacing (2w:positive (strcat direction " spacing c/c (mm)") 150.0))
           (setq n (1+ (fix (/ span-mm spacing)))))
  )
  (setq area (* (/ (* pi dia dia) 4.0) (/ 1000.0 spacing)))
  (setq req (getreal (strcat "\nEngineer-supplied required As for " direction " (mm2/m; Enter to skip): ")))
  (if (and req (> req 0.0) (< area req))
    (princ (strcat "\nWARNING: selected steel provides " (rtos area 2 1) " mm2/m, below entered required " (rtos req 2 1) " mm2/m.")))
  (if (or (null req) (<= req 0.0) (>= area req))
    (princ (strcat "\n" direction ": As,provided = " (rtos area 2 1) " mm2/m; " (itoa n) " indicative bars; spacing " (rtos spacing 2 1) " mm.")))
  (list dia spacing n area)
)

(defun 2w:draw-rep (x0 y0 x1 y1 mmfactor cover-dwg shortx bottomS bottomL crankS crankL layerbot layercr layertext / y x dx dy)
  ;; Clean representative line, with end range ticks and ratio ticks; not a bar schedule/grid.
  (if shortx
    (progn
      (setq y (/ (+ y0 y1) 2.0) dx (* 0.42 (- x1 x0)))
      (2w:line (list (- (/ (+ x0 x1) 2.0) dx) y 0.0) (list (+ (/ (+ x0 x1) 2.0) dx) y 0.0) layerbot)
      (2w:arrow-tick (list (- (/ (+ x0 x1) 2.0) dx) y 0.0) 0.707 0.707 (* 0.04 (- x1 x0)) layerbot)
      (2w:arrow-tick (list (+ (/ (+ x0 x1) 2.0) dx) y 0.0) 0.707 0.707 (* 0.04 (- x1 x0)) layerbot)
      (2w:text (list x0 (- y (* 0.04 (- y1 y0))) 0.0) (* 0.025 (- y1 y0))
        (strcat "BOT SHORT: %%c" (rtos (car bottomS) 2 0) " @ " (rtos (cadr bottomS) 2 0) " mm c/c") layertext)
      (setq dy (* 0.36 (- y1 y0)))
      (2w:line (list (/ (+ x0 x1) 2.0) (- (/ (+ y0 y1) 2.0) dy) 0.0)
               (list (/ (+ x0 x1) 2.0) (+ (/ (+ y0 y1) 2.0) dy) 0.0) layercr)
      (2w:text (list x0 (+ y0 (* 0.02 (- y1 y0))) 0.0) (* 0.025 (- y1 y0))
        (strcat "CRANK SHORT: %%c" (rtos (car crankS) 2 0) " @ " (rtos (cadr crankS) 2 0) " mm c/c") layertext))
    (progn
      (setq x (/ (+ x0 x1) 2.0) dy (* 0.42 (- y1 y0)))
      (2w:line (list x (- (/ (+ y0 y1) 2.0) dy) 0.0) (list x (+ (/ (+ y0 y1) 2.0) dy) 0.0) layerbot)
      (2w:arrow-tick (list x (- (/ (+ y0 y1) 2.0) dy) 0.0) 0.707 0.707 (* 0.04 (- x1 x0)) layerbot)
      (2w:arrow-tick (list x (+ (/ (+ y0 y1) 2.0) dy) 0.0) 0.707 0.707 (* 0.04 (- x1 x0)) layerbot)
      (2w:text (list (+ x0 (* 0.02 (- x1 x0))) y0 0.0) (* 0.025 (- x1 x0))
        (strcat "BOT LONG: %%c" (rtos (car bottomL) 2 0) " @ " (rtos (cadr bottomL) 2 0) " mm c/c") layertext)
      (setq dx (* 0.36 (- x1 x0)))
      (2w:line (list (- (/ (+ x0 x1) 2.0) dx) (/ (+ y0 y1) 2.0) 0.0)
               (list (+ (/ (+ x0 x1) 2.0) dx) (/ (+ y0 y1) 2.0) 0.0) layercr)
      (2w:text (list (+ x0 (* 0.02 (- x1 x0))) (+ y0 (* 0.05 (- y1 y0))) 0.0) (* 0.025 (- x1 x0))
        (strcat "CRANK LONG: %%c" (rtos (car crankL) 2 0) " @ " (rtos (cadr crankL) 2 0) " mm c/c") layertext)))
)

(defun c:2WSLAB (/ *error* olderr unit factor main x0 y0 x1 y1 sx sy shortx
                   supportss supportboxes openingss openingboxes cantss cantboxes
                   ask choice edge modes beam tol cutoffS cutoffL covermm cover
                   bottomS bottomL crankS crankL topbar cantmain cantdist
                   mmx mmy ht y x b dia sp cantRatio point)
  (setq olderr *error* *error* 2w:error)
  (setq *2w-old-osmode* (getvar "OSMODE") *2w-old-clayer* (getvar "CLAYER"))
  (setvar "OSMODE" 0)
  (princ "\n2WSLAB - preliminary slab reinforcement detailing helper; engineer review required.")
  (initget "Inches Feet")
  (setq unit (getkword "\nDrawing coordinate unit [Inches/Feet] <Feet>: "))
  (if (null unit) (setq unit "Feet"))
  (setq factor (if (= unit "Inches") 25.4 304.8))
  (setq main (2w:select-rect "\nSelect closed rectangular polyline for main slab boundary: "))
  (if (null main) (progn (princ "\nCancelled.") (2w:restore-vars) (setq *error* olderr) (princ) (exit)))
  (setq x0 (cadr main) y0 (caddr main) x1 (cadddr main) y1 (car (cddddr main)))
  (setq sx (- x1 x0) sy (- y1 y0) shortx (<= sx sy))
  (if (or (<= sx 0.0) (<= sy 0.0)) (progn (princ "\nInvalid slab extents.") (exit)))
  (princ "\nSelect supporting beam LINE/POLYLINE entities, then press Enter.")
  (setq supportss (ssget ":L" '((0 . "LINE,LWPOLYLINE,POLYLINE"))))
  (setq supportboxes (if supportss (2w:ss-bbox supportss) nil))
  (setq ask (2w:yesno "Are there cantilever slab arms?" "No"))
  (setq cantboxes nil)
  (if (= ask "Yes")
    (progn (princ "\nSelect closed rectangular cantilever projection polylines, then Enter.")
           (setq cantss (ssget ":L" '((0 . "LWPOLYLINE,POLYLINE"))))
           (if cantss (setq cantboxes (2w:ss-bbox cantss)))))
  (setq ask (2w:yesno "Are there slab openings?" "No"))
  (setq openingboxes nil)
  (if (= ask "Yes")
    (progn (princ "\nSelect closed rectangular opening polylines, then Enter.")
           (setq openingss (ssget ":L" '((0 . "LWPOLYLINE,POLYLINE"))))
           (if openingss (setq openingboxes (2w:ss-bbox openingss)))))
  (initget "L/4 L/5 Auto")
  (setq choice (getkword "\nCrank cutoff ratio [L/4/L/5/Auto] <Auto>: "))
  (if (null choice) (setq choice "Auto"))
  (setq modes nil tol (* 0.02 (min sx sy)))
  (foreach edge '("Bottom" "Top" "Left" "Right")
    (if (2w:edge-beam-p edge x0 y0 x1 y1 supportboxes tol)
      (setq modes (cons (cons edge (2w:edge-mode edge)) modes))))
  (setq covermm (2w:positive "Clear cover (inches)" 0.75) cover (/ (* covermm 25.4) factor))
  (setq bottomS (2w:get-bar-set "Bottom short-direction" (* (min sx sy) factor)))
  (setq bottomL (2w:get-bar-set "Bottom long-direction" (* (max sx sy) factor)))
  (setq crankS (2w:get-bar-set "Cranked short-direction" (* (min sx sy) factor)))
  (setq crankL (2w:get-bar-set "Cranked long-direction" (* (max sx sy) factor)))
  (setq topbar (2w:get-bar-set "Double-top extra" (* (max sx sy) factor)))
  (if (and cantboxes (initget "1.5L 2.0L") (setq cantRatio (getkword "\nCantilever back-span anchorage [1.5L/2.0L] <1.5L>: ")))
    (if (= cantRatio "2.0L") (setq cantRatio 2.0) (setq cantRatio 1.5))
    (setq cantRatio 1.5))
  (if cantboxes
    (progn (setq cantmain (2w:get-bar-set "Cantilever top main" (* (max sx sy) factor)))
           (setq cantdist (2w:get-bar-set "Cantilever distribution" (* (min sx sy) factor)))))
  (setq ask (2w:yesno "Place corner torsion mat callouts at discontinuous corners?" "No"))

  ;; Create requested ACI color-index layers (ACI colors only; drawing units remain unchanged).
  (2w:layer "REBAR-BOT" 4) (2w:layer "REBAR-CRANK" 3)
  (2w:layer "REBAR-TOP-EXTRA" 1) (2w:layer "REBAR-CANTILEVER" 5)
  (2w:layer "REBAR-CORNER" 6) (2w:layer "REBAR-TRIMMER" 30)
  (2w:layer "REBAR-DIM" 8) (2w:layer "REBAR-TEXT" 2)

  ;; Representative ranges rather than full bar grids. Openings and projection geometry
  ;; are retained as selections for review; they are not automatically clipped/trimmed.
  (2w:draw-rep x0 y0 x1 y1 factor cover shortx bottomS bottomL crankS crankL "REBAR-BOT" "REBAR-CRANK" "REBAR-TEXT")
  (setq ht (* 0.025 (min sx sy)))
  (2w:text (list x0 (+ y0 (* 0.10 sy)) 0.0) ht
    (strcat "PRELIMINARY ONLY - verify design/development/spacing. Cover " (rtos covermm 2 1) " mm (" (rtos cover 2 3) " drawing units).") "REBAR-TEXT")
  (foreach edge '("Bottom" "Top" "Left" "Right")
    (setq beam (2w:edge-beam-p edge x0 y0 x1 y1 supportboxes tol))
    (if beam
      (progn
        (setq modes (assoc edge modes))
        (setq cutoffS (2w:cut-ratio edge (if modes (cdr modes) "Discontinuous") choice))
        (if (member edge '("Bottom" "Top"))
          (progn
            (setq x (if (= edge "Bottom") (+ x0 (* cutoffS sx)) (- x1 (* cutoffS sx))))
            (2w:line (list x y0 0.0) (list x y1 0.0) "REBAR-CRANK")
            (2w:arrow-tick (list x (/ (+ y0 y1) 2.0) 0.0) 0.707 0.707 (* 0.04 sy) "REBAR-CRANK")
            (2w:dim (list (if (= edge "Bottom") x0 x1) y0 0.0) (list x y0 0.0) (list x (- y0 (* 0.12 sy)) 0.0)))
          (progn
            (setq y (if (= edge "Left") (+ y0 (* cutoffS sy)) (- y1 (* cutoffS sy))))
            (2w:line (list x0 y 0.0) (list x1 y 0.0) "REBAR-CRANK")
            (2w:arrow-tick (list (/ (+ x0 x1) 2.0) y 0.0) 0.707 0.707 (* 0.04 sx) "REBAR-CRANK")
            (2w:dim (list x0 (if (= edge "Left") y0 y1) 0.0) (list x0 y 0.0) (list (- x0 (* 0.12 sx)) y 0.0))))))
  (if openingboxes
    (foreach b openingboxes
      (2w:text (list (car b) (cadddr b) 0.0) ht "OPENING SELECTED - bars/trimmers require manual detailing" "REBAR-TEXT")))
  (if cantboxes
    (foreach b cantboxes
      (2w:text (list (car b) (cadddr b) 0.0) ht
        (strcat "CANTILEVER SELECTED - verify development length; requested multiplier " (rtos cantRatio 2 1) "L") "REBAR-TEXT")))
  (if (= ask "Yes")
    (2w:text (list x0 (+ y0 (* 0.06 sy)) 0.0) ht "CORNER REINF. %%c10 @ 150mm c/c TOP & BOT - ENGINEER TO VERIFY" "REBAR-CORNER"))
  (princ "\n2WSLAB finished. Review all geometry and design with a qualified structural engineer.")
  (2w:restore-vars)
  (setq *error* olderr)
  (princ)
)

(princ "\n2WSLAB loaded. Type 2WSLAB to run. Use ASCII-compatible .LSP source; %%c creates diameter symbol.")
(princ)
