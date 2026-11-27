# RCCSlabDetailing — 2WSLAB AutoLISP helper

`2WSLAB.LSP` adds the AutoCAD command `2WSLAB` for preliminary reinforcement annotation on **axis-aligned rectangular slab panels**. It is intended as a drafting/area-bookkeeping starting point for AutoCAD 2021–2026, not as a structural design or code-compliance program.

## Load and run

1. Open a copy of the drawing and confirm its coordinate units are inches or feet.
2. In AutoCAD, run `APPLOAD` and load `2WSLAB.LSP`.
3. Type `2WSLAB` and follow the prompts. Bar diameters, spacings, and optional required steel areas are entered in mm and mm²/m; slab geometry is interpreted in the selected inches/feet drawing unit.
4. Review the generated annotations and dimensions, then save only after engineering review.

The script creates layers using ACI colors: REBAR-BOT (4 cyan), REBAR-CRANK (3 green), REBAR-TOP-EXTRA (1 red), REBAR-CANTILEVER (5 blue), REBAR-CORNER (6 magenta), REBAR-TRIMMER (30 orange), REBAR-DIM (8 gray), and REBAR-TEXT (2 yellow). It restores the prior `OSMODE` and `CLAYER` when the command exits or errors.

## What the current implementation does

- Selects a four-corner, axis-aligned, closed polyline for the main slab outline.
- Collects support LINE/POLYLINE extents and asks whether supported edges continue across the support. `Auto` uses L/4 for edges answered Continuous and L/5 otherwise; explicit L/4 and L/5 override that choice.
- Prompts for cover in inches, bar diameter/spacing or bar count for the two bottom directions and two crank directions, and a user-supplied required As (optional). It reports the selected As,provided = πd²/4 × 1000/s and warns if that is below the entered As,required.
- Collects rectangular cantilever and opening selections and creates representative reinforcement annotations, support cutoff marks, and DIMLINEAR dimensions using the active dimension style.
- Lets the user enter an optional cantilever anchorage multiplier and adds torsion-reinforcement callout text when requested.

## Important limitations — do not use as construction documents

This version intentionally **does not** calculate slab actions or determine design reinforcement. An engineer must determine and enter required reinforcement from a valid analysis. The area comparison is only a unit-based arithmetic check; it does not establish adequacy or BNBC/ACI compliance.

The selected plan geometry is reduced to its rectangular extents. Representative lines are not bar-by-bar placement plans. Selected openings are **not** used to clip bars or generate trimmers; cantilevers do **not** get hooked bars or anchorage geometry; top-extra, corner, and distribution reinforcement are not fully laid out. The script does not check minimum/maximum reinforcement, spacing limits, flexure, one-way/punching shear, deflection, crack control, bar development/lap, hooks, cover rules, seismic detailing, or support/continuity conditions. The fixed corner callout is a reminder only, not a code determination. Not every requested drafting feature is implemented in this initial version.

L/4, L/5, and cantilever multipliers are user-directed drafting ratios, not universal code requirements. Verify all detailing against the project-specific adopted BNBC 2020 provisions, referenced ACI edition, drawings, material properties, and structural engineer's design. Have a qualified engineer independently review the complete drawing before construction.

## Compatibility note

The source uses ASCII characters and AutoCAD `%%c` diameter notation to avoid relying on a particular editor encoding. Visual LISP/ActiveX functions are used for polyline bounds; use Windows AutoCAD 2021–2026 and test in a disposable drawing first. The active DIMSTYLE controls dimensions; drawing unit-format settings control displayed feet/inches formatting.
