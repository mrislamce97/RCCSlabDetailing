# RCCSlabDetailing — 2WSLAB AutoLISP helper

`2WSLAB.LSP` adds the AutoCAD command `2WSLAB` for preliminary reinforcement annotation on **axis-aligned rectangular slab panels**. It is intended as a drafting/area-bookkeeping starting point for AutoCAD 2021–2026, not as a structural design or code-compliance program.

## Load and run

1. Open a copy of the drawing and confirm its coordinate units are inches or feet.
2. In AutoCAD, run `APPLOAD` and load `2WSLAB.LSP`.
3. Type `2WSLAB` and follow the prompts. Bar diameters, spacings, and optional required steel areas are entered in mm and mm²/m; slab geometry is interpreted in the selected inches/feet drawing unit.
4. Review the generated annotations and dimensions, then save only after engineering review.

The script creates the requested AutoCAD layers and restores the prior `OSMODE` and `CLAYER` when the command exits or errors.

## What the current implementation does

- Selects a four-corner, axis-aligned, closed polyline for the main slab outline.
- Collects support LINE/POLYLINE extents and asks whether supported edges continue across the support. `Auto` uses L/4 for edges answered Continuous and L/5 otherwise; explicit L/4 and L/5 override that choice.
- Prompts for clear cover in inches and for bar diameter/spacing or bar count for bottom and crank directions. For bottom bars, an optional engineer-supplied required As allows a unit-based provided-versus-required comparison.
- Collects rectangular cantilever and opening selections and creates representative reinforcement annotations, support cutoff marks, and DIMLINEAR dimensions using the active dimension style.
- Lets the user enter an optional cantilever anchorage multiplier and adds torsion-reinforcement callout text when requested.

## Important limitations

This version intentionally **does not** calculate slab actions or determine design reinforcement. An engineer must determine and enter required reinforcement from a valid analysis. The area comparison is only a unit-based arithmetic check and does not establish structural adequacy or BNBC/ACI compliance.

The plan geometry is reduced to rectangular extents. Representative lines are not bar-by-bar placement plans. Selected openings are **not** clipped or given generated trimmers; cantilevers do **not** receive complete hooked-bar or anchorage geometry; top-extra, corner, and distribution reinforcement are not fully laid out. The script does not check minimum/maximum reinforcement, spacing limits, flexure, shear, deflection, development/lap, hooks, seismic detailing, or support conditions. The corner callout is a reminder only.

Verify all detailing against BNBC 2020, the adopted ACI edition, project drawings, material properties, and an engineer's design. Have a qualified engineer independently review construction drawings.

## Source and compatibility

The source uses ASCII text and AutoCAD `%%c` diameter notation. Use Windows AutoCAD 2021–2026 and test in a disposable drawing first. The active DIMSTYLE controls dimensions; drawing unit-format settings control feet/inches display.
