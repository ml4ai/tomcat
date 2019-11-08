.. ToMCAT: Theory of Mind-based Cognitive Architecture for Teams documentation master file, created by
   sphinx-quickstart on Sun Nov 3 08:50:49 2019.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Welcome to ToMCAT's documentation!
==================================

Current state-of-the art approaches to designing artificially intelligent (AI)
teammates address some of the capabilities that they must have, including
inferring the internal states of other agents 
solving problems collaboratively with them, and communicating
with them in a socially-aware manner. 
To be a truly effective teammate, an AI agent must possess *all*
of these capabilities, but no current approach combines them.  To address
this, we propose a *Theory of Mind-based Cognitive Architecture for Teams
(ToMCAT)* with these capabilities.

ToMCAT will be tested in the virtual environment of Minecraft alongside human
players. ToMCAT is comprised of a set of *local* agents (one for each human
teammate) equipped with cameras and microphones to capture facial expressions
and speech, as well as *virtual* sensors that record the local environment, the
actions performed by human teammates, and chat exchanges between them. The
local agents communicate with their respective humans, as well as with a
*global* agent that performs coordination and global team optimization.

This site contains documentation and resources on the overall project, as well
as the C++ API and links to the Java API and other ToMAT components.


.. toctree::
   :maxdepth: 2
   :caption: Contents:

   team
   installation
   lisp
   CONTRIBUTING
   Client_Server_Communication
   prebuilt_world_loading
   cpp_api/index

We are also building some Java components to extend Minecraft and Project Malmo
functionality - The API for those components can be found here: `Java API <java_api/index.html>`_

The Github repository for ToMCAT can be found `here <https://github.com/ml4ai/tomcat>`_.

Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
