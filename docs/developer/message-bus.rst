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
bus. Let's go through a small exercise in publishing/subscribing using client
executables.

Once you've started up the broker, you can subscribe to a topic called
``example_topic`` using the ``mosquitto_sub`` client:::

    mosquitto_sub -t example_topic

Then, in a separate terminal window, use the ``mosquitto_pub`` client to
publish a message containing the string ``Hello world`` to ``example_topic``.::

    mosquitto_pub -t example_topic -m "Hello world"

You should see ``Hello world`` printed to standard output in the terminal
window in which ``mosquitto_sub`` was run (see screenshot below for example)

.. image:: http://vanga.sista.arizona.edu/tomcat/data/screenshots/mosquitto_client_executable_usage.png

Now, let's try an example where the message published is not supplied using the
``-m`` command line flag, but using a pipe instead. Since the messages we will
be producing and consuming are in JSON format, we will use JSON for this
example.

In the same window where you ran ``mosquitto_pub`` last time, run the following
invocation:::

    echo '{"key": "value"}' | mosquitto_pub -t example_topic -l

You should see the JSON object output to standard output in the window in which
``mosquitto_sub`` was run.

.. image:: http://vanga.sista.arizona.edu/tomcat/data/screenshots/mosquitto_client_executable_usage_pipe_1.png

The string ``{"key": "value"}`` represents a JSON object. It is output to
standard output using ``echo``, and subsequently piped into ``mosquitto_pub``
using ``|``. The ``-l`` command line flag tells ``mosquitto_pub`` to treat each
incoming line (separated by newline characters) as separate messages.

Let us now put all these components together, as well as learn a bit more about
another useful tool called ``jq`` (which can be obtained using your package
manager: ``port/brew/apt-get install jq``), which is a program for working with
JSON at the command line. ``jq`` can be a helpful tool when debugging JSON
message publishing. When used as a filter with no arguments, any JSON piped
into jq will be pretty-printed to the standard output stream.::

    $ echo '{"key": "value"}' | jq
    {
      "key": "value"
    }

We will publish the following two JSON messages to ``topic1``.::

    {"key": "value1"}
    {"key": "value2"}

For each one, we will extract the value stored in the key '``key``' using
``jq``, and publish that value to ``topic2``.

In one terminal window, run the following invocation to set up the ``jq``
processing 'stream'.::

    mosquitto_sub -t topic1 | jq --unbuffered .key | mosquitto_pub -t topic2 -l

In another window, subscribe to ``topic2``:::

    mosquitto_sub -t topic2

Then, in a third window, publish two JSON messages to topic1:::

    echo '{"key": "value1"}' '{"key": "value2"}' | mosquitto_pub -t topic1 -l

In the second window, you'll see ``value1`` and ``value2`` being printed to
standard output.

.. image:: http://vanga.sista.arizona.edu/tomcat/data/screenshots/mosquitto_client_executable_usage_pipe_2.png

This example illustrates an elegant stream processing pipeline
setup with MQTT client executables and pipes. Hopefully, this simple exercise
has helped you get an intuitive sense of how a message-based communication
system works.

Using a client library
^^^^^^^^^^^^^^^^^^^^^^

For more complex scenarios, it may be necessary to use a client library instead
of a client executable. One example of such a scenario would be when a client
application needs to publish a variety of messages to different topics. While
``mosquitto_sub`` is capable of subscribing to multiple topics,
``mosquitto_pub`` is designed to publish to a single topic. =

Using a client library is also not a bad idea when your client program needs to
process messages from different topics differently. While you could technically
design your client to inspect each incoming message and route them based on
their contents, it is probably simpler to create multiple client objects to
subscribe to different topics with different callbacks (it's okay if this
doesn't mean anything to you just yet - we'll get to it soon).

This is the case with the ToMCAT Java mod (which extends the `Malmo`_ mod). For
when a client application does not need to publish to more than one topic, this
is an elegant way to work with a message bus.

Using client executables provides a couple of advantages over
using a client library. For one, it makes it easier to switch message brokers
in the future - say, if you wanted to use `Apache Kafka`_ for your streaming
architecture rather than Mosquitto.

.. _other ways: https://www.enterpriseintegrationpatterns.com/patterns/messaging/IntegrationStylesIntro.html
.. _mosquitto: https://mosquitto.org
.. _topics: ../tomcat_openapi.html
.. _Apache Kafka: https://kafka.apache.org
.. _Malmo: https://github.com/microsoft/malmo
