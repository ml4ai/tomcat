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
'subscribe' and 'publish' to specific topics, thus having to deal with only a
subset of the information on the bus rather than all of it.

A message *broker* is a program that serves to handle the flow of messages,
routing them as necessary between the publishers and subscribers. It enables
asynchronicity by storing messages on a topic until all the subscribers who
have subscribed to the topic have received them.

There are a number different types of messaging protocols. In ToMCAT, we use
MQTT, since it is the protocol being used in the ASIST cloud testbed being
developed by the SIM2 collaboration (Aptima, ASU, CoS). MQTT is a lightweight
messaging protocol suitable for embedded computers and IoT (internet of things)
systems.

The message broker that we use in ToMCAT is `Mosquitto`_. Communication with
the message broker can be done using an executable or a client library. We will
go over both these methods.

Starting up the Mosquitto message broker
----------------------------------------

The invocation to start up the Mosquitto message broker depends on the
operating system and the package manager you're using (keep in mind that
running the ``tools/install`` script will automatically install Mosquitto for
you).

MacOS
^^^^^

If you installed Mosquitto using MacPorts, you can start it up in the
background with the default settings by invoking:::

    mosquitto &


If you used Homebrew, you can do:::

    $(brew --prefix)/sbin/mosquitto &


Ubuntu
^^^^^^

If you're on Ubuntu, Mosquitto will automatically start up as a background
service after installation, so you don't need to start it up manually.


Publishing/subscribing
----------------------

Using client executables
^^^^^^^^^^^^^^^^^^^^^^^^

Using a client library
^^^^^^^^^^^^^^^^^^^^^^

.. _other ways: https://www.enterpriseintegrationpatterns.com/patterns/messaging/IntegrationStylesIntro.html
.. _mosquitto: https://mosquitto.org
