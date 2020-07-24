# =============================================================================
# *Please read all comments before running Makefile*
#
# First ensure that Common Lisp, Quicklisp, Shop3, etc are properly installed
# (instructions for this can be found at
# https://ml4ai.github.io/tomcat/developer/lisp.html)
#
# The parser requires Python 3.6 or higher.
#
# Any edits to the files in tomcat/src/pddl/plan_trace_generation may cause
# unexpected output or cause the make command to fail.
# =============================================================================


all: sar-individual-plan-traces.json

# =============================================================================
# Parse plan output
# =============================================================================

sar-individual-plan-traces.json: sar-individual-plan-trace-parser.py sar-individual-plan-output.txt
	python $^ $@


# =============================================================================
# Get plan output
# =============================================================================

sar-individual-plan-output.txt: sar-individual-plan-trace-version.lisp
	sbcl --load $^ --quit > $@
