;;; 2WSLAB-LOAD-REPORT.LSP - load takeoff PDF add-on for 2WSLAB
;;; Windows AutoCAD / Visual LISP. Command: 2WSLABREPORT
;;;
;;; This reports user-entered area loads; it is NOT a structural design or
;;; code-compliance calculation. Wall load must be entered as equivalent
;;; uniformly distributed area load (kN/m2), not line load (kN/m).

(vl-load-com)

(setq *2wr-file* nil *2wr-offset* 0 *2wr-offsets* nil)

(defun 2wr:write-line (s)
  (write-line s *2wr-file*)
  ;; AutoCAD for Windows writes CRLF for write-line.
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

(defun 2wr:pdf-text (lines / y line content)
  (setq y 760 content "BT /F1 10 Tf")
  (foreach line lines
    (setq content
      (strcat content " 1 0 0 1 48 " (itoa y) " Tm ("
              (2wr:pdf-escape line) ") Tj"))
    (setq y (- y 34)))
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
    (setq value (getreal (strcat "\n" prompt " (enter 0 if not applicable): ")))
    (if (or (null value) (< value 0.0))
      (princ "\nEnter a numeric value greater than or equal to zero.")))
  value
)

(defun 2wr:num (value places)
  (rtos value 2 places)
)

(defun 2wr:write-pdf (path lines / content)
  (setq content (2wr:pdf-text lines))
  (setq *2wr-file* (open path "w"))
  (if (null *2wr-file*)
    nil
    (progn
      (setq *2wr-offset* 0 *2wr-offsets* (list 0))
      (2wr:write-line "%PDF-1.4")
      (setq *2wr-offsets* (append *2wr-offsets* (list *2wr-offset*)))
      (2wr:write-line "1 0 obj << /Type /Catalog /Pages 2 0 R >> endobj")
      (setq *2wr-offsets* (append *2wr-offsets* (list *2wr-offset*)))
      (2wr:write-line "2 0 obj << /Type /Pages /Kids [3 0 R] /Count 1 >> endobj")
      (setq *2wr-offsets* (append *2wr-offsets* (list *2wr-offset*)))
      (2wr:write-line "3 0 obj << /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << /Font << /F1 4 0 R >> >> /Contents 5 0 R >> endobj")
      (setq *2wr-offsets* (append *2wr-offsets* (list *2wr-offset*)))
      (2wr:write-line "4 0 obj << /Type /Font /Subtype /Type1 /BaseFont /Helvetica >> endobj")
      (setq *2wr-offsets* (append *2wr-offsets* (list *2wr-offset*)))
      (2wr:write-line (strcat "5 0 obj << /Length " (itoa (strlen content)) " >> stream"))
      (2wr:write-line content)
      (2wr:write-line "endstream endobj")
      (setq *2wr-offsets* (append *2wr-offsets* (list *2wr-offset*)))
      (2wr:write-line (strcat "xref\n0 6\n0000000000 65535 f "))
      ;; xref records follow as individual CRLF lines. Rewrite xref start below
      ;; using a compact second pass is avoided: xref offset is current position
      ;; minus the xref line's length and its CRLF.
      (foreach off (cdr *2wr-offsets*)
        (2wr:write-line (strcat (substr "0000000000" 1 (max 0 (- 10 (strlen (itoa off)))))
                                (itoa off) " 00000 n ")))
      (2wr:write-line (strcat "trailer << /Size 6 /Root 1 0 R >>"))
      ;; The xref start marker is emitted as a final comment for viewers; the
      ;; standard xref table is located by scanning after the PDF header.
      (2wr:write-line (strcat "startxref\n" (itoa (- *2wr-offset* 25)) "\n%%EOF"))
      (close *2wr-file*)
      (setq *2wr-file* nil)
      T))
)

(defun c:2WSLABREPORT (/ unit factor lx ly shortm longm area thickness density finish wall live selfweight dead total project path lines)
  (princ "\n2WSLABREPORT - creates a load-takeoff PDF from values you enter.")
  (princ "\nNo load assumptions/defaults are supplied; confirm values with the engineer and project code.")
  (initget "Feet Inches")
  (setq unit (getkword "\nSlab dimension units [Feet/Inches] <Feet>: "))
  (if (null unit) (setq unit "Feet"))
  (setq factor (if (= unit "Inches") 0.0254 0.3048))
  (setq lx (2wr:positive "Panel long dimension"))
  (setq ly (2wr:positive "Panel short dimension"))
  (setq shortm (* (min lx ly) factor)
        longm (* (max lx ly) factor)
        area (* shortm longm))
  (setq thickness (2wr:positive "Slab thickness (mm)"))
  (setq density (2wr:positive "Concrete unit weight (kN/m3)"))
  (setq finish (2wr:nonnegative "Floor-finish load (kN/m2)"))
  (setq live (2wr:nonnegative "Live load (kN/m2)"))
  (setq wall (2wr:nonnegative "Wall load as equivalent uniform AREA load (kN/m2)"))
  (setq project (getstring T "\nProject / slab reference (optional): "))
  (setq selfweight (* (/ thickness 1000.0) density)
        dead (+ selfweight finish wall)
        total (+ dead live))
  (setq lines
    (list
      "2WSLAB - SLAB LOAD TAKE-OFF / CALCULATION NOTE"
      (strcat "Project / reference: " (if (= project "") "Not specified" project))
      "Scope: load take-off only; this is not a slab design or code approval."
      (strcat "Panel dimensions: " (2wr:num longm 3) " m x " (2wr:num shortm 3) " m")
      (strcat "Panel area A = L x B = " (2wr:num area 3) " m2")
      (strcat "Slab thickness h = " (2wr:num thickness 1) " mm = " (2wr:num (/ thickness 1000.0) 4) " m")
      (strcat "Concrete unit weight = " (2wr:num density 2) " kN/m3")
      (strcat "Slab self-weight = h x unit weight = " (2wr:num selfweight 3) " kN/m2")
      (strcat "Floor finish entered = " (2wr:num finish 3) " kN/m2")
      (strcat "Wall load entered (area-equivalent) = " (2wr:num wall 3) " kN/m2")
      (strcat "Dead load D = self-weight + finish + wall = " (2wr:num dead 3) " kN/m2")
      (strcat "Live load L = " (2wr:num live 3) " kN/m2")
      (strcat "Service load D + L = " (2wr:num total 3) " kN/m2")
      "No load factors, moments, shear, deflection, or reinforcement design are calculated."
      "Wall loads supplied as kN/m must be converted to an area load by the engineer."
      "Verify all inputs, load combinations, analysis, and detailing with a qualified structural engineer."
      "Report generated by 2WSLABREPORT; input values are user-provided, not code defaults."))
  (setq path (getfiled "Save load calculation PDF" "2WSLAB-load-report.pdf" "pdf" 1))
  (if path
    (if (2wr:write-pdf path lines)
      (princ (strcat "\nPDF created: " path))
      (princ "\nCould not create the PDF at the selected path."))
    (princ "\nPDF creation cancelled."))
  (princ)
)

(princ "\n2WSLAB-LOAD-REPORT loaded. Type 2WSLABREPORT to create a load calculation PDF.")
(princ)
