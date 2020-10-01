# =============================================================================
# *Please read all comments before running Makefile*
#
# First ensure that Common Lisp, Quicklisp, Shop3, etc are properly installed
# (instructions for this can be found at
# https://ml4ai.github.io/tomcat/developer/lisp.html)
#
# The parser requires Python 3.6 or higher.
#
# Any edits to the files in tomcat/src/lisp/plan_trace_generation or
# tomcat/src/lisp/plan_recognition_toy  may cause
# unexpected output or cause the make command to fail.
# =============================================================================


all: simple-schedule-plan-traces.json

# =============================================================================
# Parse plan output
# =============================================================================

simple-schedule-plan-traces.json: ../plan_trace_generation/plan_trace_parser.py simple-schedule-plans.txt
	python $^ $@


# =============================================================================
# Get plan output
# =============================================================================

simple-schedule-plans.txt: simple-schedule-test-prob.lisp
	sbcl --load $^ --quit > $@

