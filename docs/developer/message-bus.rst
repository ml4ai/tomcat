Working with the MQTT message bus
=================================


.. toctree::
   :maxdepth: 1
   :caption: Contents:

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
developed by the SIM2 collaboration (Aptima + ASU + CoS). MQTT is a lightweight
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


Message topics and topic hierarchies
------------------------------------

Message brokers route messages to/from client applications based on the topics
that the applications published/subscribed to. Topics follow a hierarchical
pattern, with levels in the hierarchy separated by slashes (``/``). For
example, consider the topics shown below (a subset of the `topics`_
implemented in ToMCAT).

- ``observations/events/entity_death``
- ``observations/events/mob_attacked``

If a client application subscribed to ``observations/events/#``, they would
receive the messages from both the ``observations/events/entity_death`` and
``observations/events/mob_attacked`` topics.

Publishing/subscribing
----------------------

There are two ways to publish/subscribe to topics on a message bus - using
client executables, or using a client library in your own program.

Using client executables
^^^^^^^^^^^^^^^^^^^^^^^^

Using a client executable is a simple, no-frills method to work with a message
bus.

Let's go through a small exercise in publishing/subscribing using client
executables.

Once you've started up the broker, you can subscribe to a topic called
``example_topic`` using the ``mosquitto_sub`` client:::

    mosquitto_sub -t example_topic

Then, in a separate terminal window, use the ``mosquitto_pub`` client to
publish a message containing the string ``Hello world`` to ``example_topic``.::

    mosquitto_pub -t example_topic -m "Hello world"

You should see ``Hello world`` printed to standard output in the terminal
window in which ``mosquitto_sub`` was run.

.. image:: http://vanga.sista.arizona.edu/tomcat/data/screenshots/mosquitto_client_executable_usage.png 

Using client executables provides a few advantages over using a client library.

- It makes it easier to switch message brokers
- It can potentially make dependency management simpler.

For when a client application does not need to publish to more than one topic,
this is an elegant way to work with a message bus.

Using a client library
^^^^^^^^^^^^^^^^^^^^^^

.. _other ways: https://www.enterpriseintegrationpatterns.com/patterns/messaging/IntegrationStylesIntro.html
.. _mosquitto: https://mosquitto.org
.. _topics: ../tomcat_openapi.html
