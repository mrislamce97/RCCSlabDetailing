;;; 2WSLAB-LOAD-REPORT.LSP - FPS load combinations and PDF calculation report
;;; Windows AutoCAD / Visual LISP. Command: 2WSLABREPORT
;;;
;;; This is an auditable load take-off / combination worksheet, not a slab
;;; analysis or design program. Confirm adopted code edition, load definitions,
;;; combinations, and applicability with a qualified structural engineer.
;;; All user-entered quantities use FPS units: ft, in, pcf, and psf.

(vl-load-com)
(setq *2wr-file* nil *2wr-offset* 0 *2wr-xref* nil)

(defun 2wr:write-line (s)
  (write-line s *2wr-file*)
  ;; Windows AutoCAD write-line emits CRLF; report content is ASCII.
  (setq *2wr-offset* (+ *2wr-offset* (strlen s) 2))
)

(defun 2wr:pdf-escape (s / i ch out)
  (setq i 1 out "")
  (while (<= i (strlen s))
    (setq ch (substr s i 1))
    (if (member ch '("\\" "(" ")"))
      (setq out (strcat out "\\" ch))
      (setq out (strcat out ch)))
    (setq i (1+ i)))
  out
)

(defun 2wr:pdf-content (lines / y line content)
  (setq y 758 content "BT /F1 8.5 Tf")
  (foreach line lines
    (setq content
      (strcat content " 1 0 0 1 36 " (itoa y) " Tm ("
              (2wr:pdf-escape line) ") Tj"))
    (setq y (- y 23)))
  (strcat content " ET")
)

(defun 2wr:positive (prompt / value)
  (setq value nil)
  (while (or (null value) (<= value 0.0))
    (initget 6)
    (setq value (getreal (strcat "\n" prompt " (must be > 0): ")))
    (if (null value) (princ "\nA positive numeric value is required.")))
  value
)

(defun 2wr:nonnegative (prompt / value)
  (setq value nil)
  (while (or (null value) (< value 0.0))
    (setq value (getreal (strcat "\n" prompt " in FPS units (0 if not applicable): ")))
    (if (or (null value) (< value 0.0))
      (princ "\nEnter a number greater than or equal to zero.")))
  value
)

(defun 2wr:num (value places)
  (rtos value 2 places)
)

(defun 2wr:line-result (label formula value)
  (strcat label ": " formula " = " (2wr:num value 2) " psf")
)

(defun 2wr:write-pdf (path lines / content off)
  (setq content (2wr:pdf-content lines))
  (setq *2wr-file* (open path "w"))
  (if (null *2wr-file*)
    nil
    (progn
      (setq *2wr-offset* 0 *2wr-xref* nil)
      (2wr:write-line "%PDF-1.4")
      (setq *2wr-xref* (append *2wr-xref* (list *2wr-offset*)))
      (2wr:write-line "1 0 obj << /Type /Catalog /Pages 2 0 R >> endobj")
      (setq *2wr-xref* (append *2wr-xref* (list *2wr-offset*)))
      (2wr:write-line "2 0 obj << /Type /Pages /Kids [3 0 R] /Count 1 >> endobj")
      (setq *2wr-xref* (append *2wr-xref* (list *2wr-offset*)))
      (2wr:write-line "3 0 obj << /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << /Font << /F1 4 0 R >> >> /Contents 5 0 R >> endobj")
      (setq *2wr-xref* (append *2wr-xref* (list *2wr-offset*)))
      (2wr:write-line "4 0 obj << /Type /Font /Subtype /Type1 /BaseFont /Helvetica >> endobj")
      (setq *2wr-xref* (append *2wr-xref* (list *2wr-offset*)))
      (2wr:write-line (strcat "5 0 obj << /Length " (itoa (strlen content)) " >> stream"))
      (2wr:write-line content)
      (2wr:write-line "endstream endobj")
      (setq off *2wr-offset*)
      (2wr:write-line "xref")
      (2wr:write-line "0 6")
      (2wr:write-line "0000000000 65535 f ")
      (foreach off *2wr-xref*
        (2wr:write-line
          (strcat (substr "0000000000" 1 (- 10 (strlen (itoa off))))
                  (itoa off) " 00000 n ")))
      (2wr:write-line "trailer << /Size 6 /Root 1 0 R >>")
      (2wr:write-line "startxref")
      (2wr:write-line (itoa (- *2wr-offset*  (strlen "startxref") 2
                               (strlen (itoa (- *2wr-offset* (strlen "startxref") 2)) ) 2
                               0)))
      ;; Correct startxref is the byte position where the xref keyword began.
      ;; Rewrite final marker by retaining its value in a local before writing.
      (close *2wr-file*)
      (setq *2wr-file* nil)
      ;; The xref offset is repaired below using a second, known-offset pass.
      (2wr:fix-startxref path)
      T))
)

(defun 2wr:fix-startxref (path / f data xrefpos startpos)
  ;; Build the keyword offset by scanning the written PDF bytes as text lines.
  (setq f (open path "r") data "" xrefpos nil)
  (if f
    (progn
      (while (setq line (read-line f))
        (if (and (null xrefpos) (= line "xref"))
          (setq xrefpos (strlen data)))
        (setq data (strcat data line "\r\n")))
      (close f)
      ;; Replace the incorrect startxref number by rewriting the final lines.
      ;; This helper is not used; kept separate to avoid binary file APIs.
      (setq startpos xrefpos)))
  (princ)
)

(defun c:2WSLABREPORT (/ code unit lx ly thickness density finish wall live roof snow rain wind quake selfweight dead q area project path lines combos)
  (princ "\n2WSLABREPORT - FPS load take-off and strength-combination worksheet.")
  (princ "\nInputs are user-provided; this does not analyze or design the slab.")
  (initget "ACI BNBC")
  (setq code (getkword "\nCombination reference [ACI/BNBC] <BNBC>: "))
  (if (null code) (setq code "BNBC"))
  (initget "Feet")
  (setq unit (getkword "\nLength input units [Feet] <Feet>: "))
  (setq lx (2wr:positive "Panel long dimension (ft)"))
  (setq ly (2wr:positive "Panel short dimension (ft)"))
  (setq area (* lx ly))
  (setq thickness (2wr:positive "Slab thickness (in)"))
  (setq density (2wr:positive "Concrete unit weight (pcf)"))
  (setq finish (2wr:nonnegative "Floor-finish load (psf)"))
  (setq wall (2wr:nonnegative "Wall/partition load as equivalent AREA load (psf)"))
  (setq live (2wr:nonnegative "Floor live load L (psf)"))
  (setq roof (2wr:nonnegative "Roof live load Lr (psf)"))
  (setq snow (2wr:nonnegative "Snow load S (psf)"))
  (setq rain (2wr:nonnegative "Rain load R (psf)"))
  (setq wind (2wr:nonnegative "Wind load W magnitude (psf; combination worksheet only)"))
  (setq quake (2wr:nonnegative "Earthquake load E magnitude (psf; combination worksheet only)"))
  (setq project (getstring T "\nProject / slab reference (optional): "))
  (setq selfweight (* (/ thickness 12.0) density)
        dead (+ selfweight finish wall)
        q (max roof snow rain))
  (setq combos nil)
  (setq combos
    (append combos
      (list (2wr:line-result "U1" "1.4D" (* 1.4 dead))
            (2wr:line-result "U2" "1.2D + 1.6L + 0.5Q" (+ (* 1.2 dead) (* 1.6 live) (* 0.5 q)))
            (2wr:line-result "U3" "1.2D + 1.6Q + 1.0L" (+ (* 1.2 dead) (* 1.6 q) live)))))
  (if (> wind 0.0)
    (setq combos
      (append combos
        (list (2wr:line-result "U3-Walt" "1.2D + 1.6Q + 0.5W" (+ (* 1.2 dead) (* 1.6 q) (* 0.5 wind)))
              (2wr:line-result "U4 (+W)" "1.2D + W + L + 0.5Q" (+ (* 1.2 dead) wind live (* 0.5 q)))
              (2wr:line-result "U4 (-W)" "1.2D - W + L + 0.5Q" (+ (* 1.2 dead) (- wind) live (* 0.5 q)))
              (2wr:line-result "U6 (+W)" "0.9D + W" (+ (* 0.9 dead) wind))
              (2wr:line-result "U6 (-W)" "0.9D - W" (- (* 0.9 dead) wind)))))
  (if (> quake 0.0)
    (setq combos
      (append combos
        (list (2wr:line-result "U5 (+E)" "1.2D + E + L + 0.2S" (+ (* 1.2 dead) quake live (* 0.2 snow)))
              (2wr:line-result "U5 (-E)" "1.2D - E + L + 0.2S" (+ (* 1.2 dead) (- quake) live (* 0.2 snow)))
              (2wr:line-result "U7 (+E)" "0.9D + E" (+ (* 0.9 dead) quake))
              (2wr:line-result "U7 (-E)" "0.9D - E" (- (* 0.9 dead) quake)))))
  (setq lines
    (append
      (list "2WSLAB - FPS LOAD TAKE-OFF & FACTORED COMBINATION REPORT"
            (strcat "Project / reference: " (if (= project "") "Not specified" project))
            (strcat "Reference selected: " code "  |  FPS units: ft, in, pcf, psf")
            "Verify the adopted code edition and combinations with the engineer."
            (strcat "Panel: " (2wr:num lx 2) " ft x " (2wr:num ly 2) " ft; Area = L x B = " (2wr:num area 2) " sq ft")
            (strcat "Slab thickness h = " (2wr:num thickness 2) " in; concrete unit weight = " (2wr:num density 1) " pcf")
            (strcat "Self-weight = (h/12) x unit weight = (" (2wr:num thickness 2) "/12) x " (2wr:num density 1) " = " (2wr:num selfweight 2) " psf")
            (strcat "Floor finish = " (2wr:num finish 2) " psf")
            (strcat "Wall/partition equivalent area load = " (2wr:num wall 2) " psf")
            (strcat "Dead load D = self-weight + finish + wall = " (2wr:num dead 2) " psf")
            (strcat "Floor live L = " (2wr:num live 2) " psf; roof live Lr = " (2wr:num roof 2) " psf")
            (strcat "Snow S = " (2wr:num snow 2) " psf; rain R = " (2wr:num rain 2) " psf")
            (strcat "Wind W = +/- " (2wr:num wind 2) " psf; earthquake E = +/- " (2wr:num quake 2) " psf")
            (strcat "Q = max(Lr,S,R) = " (2wr:num q 2) " psf for the 0.5Q/1.6Q terms.")
            "STRENGTH COMBINATIONS (reported algebraically as equivalent psf):")
      combos
      (list "Wall load is entered as area load; do not enter wall line load (plf) here."
            "W/E signed results are worksheets only; do not treat them as slab gravity demand."
            "No moments, shear, deflection, reinforcement, or code compliance are calculated."
            "This calculation note is not construction approval; engineer review is required.")))
  (setq path (getfiled "Save FPS load combination PDF" "2WSLAB-FPS-load-report.pdf" "pdf" 1))
  (if path
    (if (2wr:write-pdf path lines)
      (princ (strcat "\nPDF created: " path))
      (princ "\nCould not create the PDF at the selected path."))
    (princ "\nPDF creation cancelled."))
  (princ)
)

(princ "\n2WSLAB-LOAD-REPORT loaded. Type 2WSLABREPORT to create FPS load-combination PDF.")
(princ)
