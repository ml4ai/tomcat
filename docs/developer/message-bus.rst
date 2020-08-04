Working with the MQTT message bus
=================================

In this section, we will briefly cover the basics of message buses and how to
work with them, in the context of the ToMCAT project.

A message bus is a system that allows communication between software
components. There are `other ways`_ to achieve this kind of communication (e.g.
production and consumption of files, a shared database, remote procedure
invocations) - however, an architecture built around a message bus provides a
great deal of flexibility and enables asynchronous communication between the
components.

It might be useful to visualize the bus as a hierarchical collection of
'streams' of data, called 'topics'. The data on the bus is in the form of
'messages', which are usually in a lightweight machine-readable plain text
format (in our case, we use JSON messages). Client applications can then
'subscribe' and 'publish' to specific topics, dealing with only a subset of the
information. 

- What is a message broker?
- How do we use one in ToMCAT?
- How do I publish a message?
  - Using a client executable (mosquitto_pub)
  - Using a client library (Eclipse Paho)
- How do I subscribe to a topic?
  - Using a client executable (mosquitto_pub)
  - Using a client library (Eclipse Paho)

.. _other ways: https://www.enterpriseintegrationpatterns.com/patterns/messaging/IntegrationStylesIntro.html
