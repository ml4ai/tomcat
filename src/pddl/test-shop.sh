#!/bin/sh

sbcl --eval '(progn (ql:quickload "shop3") (in-package :shop-user) (load "sar-individual-domain.pddl") (load "sar-individual-problem.pddl"))'

