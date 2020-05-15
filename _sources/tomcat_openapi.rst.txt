Data models and message bus topics
==================================

Overview
--------
We document here the message bus topics and the data models associated with
ToMCAT. An implicit design decision that we make is that each 'leaf' in the
topic hierarchy corresponds to a unique schema. So if you subscribe to all the
messages at a non-leaf level in the hierarchy, you will get messages
corresponding to all the leaves under that level.

We also provide the data models and example JSON output for data that is not
necessarily published to the message bus - for example, the ``SessionMetadata``
model describes the format of the file ``metadata.json`` that is written to 
each session data directory produced by the ``run_session`` script. 

The structured documentation below was auto-generated from `this YAML file`_.

Topics and models
-----------------

.. raw:: html
  :file: spec.html

Colophon
--------
The documentation for the topics and the data models above was generated using
a version of the `Swagger UI generator`_ that was
modified to align better with the notion of publishing/subscribing using a
message bus.

.. _Swagger UI generator: https://generator.swagger.io
.. _this YAML file`: https://github.com/ml4ai/tomcat/blob/master/docs/spec.yml
