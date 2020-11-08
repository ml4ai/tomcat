PSDG/Python files
===========================
The folder contains all files pertaining to our probablistic state-dependent grammars planning domains as well as 
our efforts to create a plan recognition algorithm. The main files required for running the domains 
and problem definitions are in python (using the .py extension).

Although earlier version of python may work, the files here assume that you are
running python 3.8 or higher. The only current package requirement is nltk (the
natural language processing package). With properly set-up python and pip
installations you can get nltk by doing,

`pip install nltk`

on your terminal. The package's documentation can be found here http://www.nltk.org/ 

PSDG and PSDProductions
-------------------------
Included in this directory are the Probablistic State-Dependent Grammars (PSDG) class
and the associated Probablistic State-Dependent Productions (PSDProduction) class. Although
these two classes are called by main PSDG\_Domain class which is enough to
create an instance of the planner, advanced users may find a use for these
outside of this main class. 

The PSDProduction class is a subclass of the nltk package's Production and ProbabilisticMixIn classes. 
Like the Production class, this class takes as arugments a left hand side nonterminal symbol and right hand side sequence of 
terminal and nonterminal symbols. Additionally it takes a function representing a set of preconditions, 
a current state, and the probability assigned to the production if the state meets the preconditions. 
The class has a function for potential subsequent updating of its proability
given a new state. 

The PSDG class is a subclass of the nltk package's PCFG class. It takes the
same arguments as the PCFG, however you must ensure that the list of
productions consists of PSDProductions and not any type of Productions defined
in the nltk package. If you use an instance of as a grammar argument to any of
the nltk package functions and classes, they will behave as if given a PCFG. 

PSDG Domain
---------------------
The PSDG\_Domain class is the main class in this directory and should be used
to construct an instance of a planning domain. It takes a list of methods and a
list of actions

**Actions** are represented as a function that takes a single argument representing
the domain state in the form of a dictionary. The function must return a
dictionary. 

**Methods** are represented as a dictionary. Each Method must have the *task*,
*preconditions*, and *subtasks* keys with an optional key of *t\_prob*. Here I
will detail what the value is for each key:

* *task* must a string and NOT start with an !
* *preconditions* must be a function that takes the domain state (in the form
  of a dictionary) and returns a boolean value.
* *subtasks* must be a list of strings. These strings can be action headers or
  other method headers. Action headers must start with ! here. 
* *t\_prob* must be a value between 0 and 1. If not given, the method's
  probability for satisfied preconditions is set to 1 by default. 

The methods must be constructed such that for any given state and task, the
probabilities of the methods for that task must sum to 1. In most cases if the
planner is meant to be determinstic, this means that for a given state and
task, there will be one method with probability 1 for that task and all other
methods assigned probability 0 for that task. 

The list of methods must include at least one method with *task* P. P
represents the nonterminal starting symbol for the PSDG. Ideally there should
be a method for P for each top level *task*, in which the top level *task* is
a *subtask* for P.  

You can find an example of a "domain definition" (i.e, lists of actions and
methods) in the python script `simple_schedule_domain.py`

Once an instance of a domain is constructed, you can use the
`initialize_planning` function which takes a initial state (as a dictionary)
and then the `sample_plans` function to sample/generate a plan from that initial
state. For probablistic domains, `sample_plans` can be given an integer n, the
number of desired samples. It can also be given a "start" symbol which
can be any *task* defined in the list of methods. If no "start" symbol is
given, it defaults to P. The output type can also be changed, with the default
being a plan trace (i.e, a state, action, state sequence). 

The `psdg_demo` jupyter notebook shows example of a plan generated for the
simple schedule domain starting at the task MONDAY. 
