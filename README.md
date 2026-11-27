# RCCSlabDetailing — 2WSLAB AutoLISP helper

`2WSLAB.LSP` provides preliminary reinforcement annotation only. It does not calculate slab actions, load combinations, or design reinforcement.

## FPS load calculation PDF

Load `2WSLAB-LOAD-REPORT.LSP` in Windows AutoCAD with `APPLOAD`, then run `2WSLABREPORT`. The add-on asks for panel dimensions (ft), slab thickness (in), concrete unit weight (pcf), and loads (psf), then offers a PDF load-takeoff and factored-combination worksheet. Inputs include floor finish, wall/partition equivalent area load, floor live load, roof live load, snow, rain, wind, and earthquake. Slab self-weight is calculated as `(thickness in / 12) x concrete unit weight pcf`.

No load assumptions/defaults are embedded. Enter the project loads, and verify the adopted code edition and combination factors with the engineer. Wall loads must be converted to an equivalent area load in psf before entry; do not enter plf in that prompt. Wind and earthquake combinations are displayed as signed worksheet terms, not slab gravity design results.

The report is not a structural analysis/design or code-compliance calculation. It does not calculate moments, shear, deflection, or reinforcement. It is not for construction approval. A qualified structural engineer must verify the load inputs, applicable ACI/ASCE or BNBC provisions, analysis, and detailing.

## Detailing helper

`2WSLAB.LSP` adds the AutoCAD command `2WSLAB` for preliminary reinforcement annotation on axis-aligned rectangular slab panels. Use a copy of the drawing and have all generated details independently reviewed. See code comments for limitations and workflow.

## Compatibility

Windows AutoCAD / Visual LISP. Test in a disposable drawing before using project files.
