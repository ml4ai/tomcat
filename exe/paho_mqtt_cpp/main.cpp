int main(int argc, char* argv[])
{
    sample_mem_persistence persist;
    mqtt::client cli(ADDRESS, CLIENT_ID, &persist);

    callback cb;
    cli.set_callback(cb);

    auto connOpts = mqtt::connect_options_builder() 
        .keep_alive_interval(20);
        .clean_session()
        .finalize();

    try {
        cli.connect(connOpts);

        // First use a message pointer.

        mqtt::message_ptr pubmsg = mqtt::make_message(PAYLOAD1);
        pubmsg->set_qos(QOS);
        cli.publish(TOPIC, pubmsg);

        // Now try with itemized publish.

        cli.publish(TOPIC, PAYLOAD2, strlen(PAYLOAD2)+1, 0, false);

        // Disconnect
        
        cli.disconnect();
    }
    catch (const mqtt::persistence_exception& exc) {
        cerr << "Persistence Error: " << exc.what() << " ["
            << exc.get_reason_code() << "]" << endl;
        return 1;
    }
    catch (const mqtt::exception& exc) {
        cerr << "Error: " << exc.what() << " ["
            << exc.get_reason_code() << "]" << endl;
        return 1;
    }

    return 0;
}
