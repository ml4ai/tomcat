# ToMCAT Baseline Tasks

Install the necessary packages using `pip`
```bash
pip install -r requirements.txt
```
**or** using `conda`
```bash
conda install --file requirements.txt
```

## Start server-side application

The server-side application must run **before** launching client-side application. Launch the server application with
```bash
python3 run_server.py -a <ip-address> -p <port>
```
where `<ip-address>` is the local IPv4 address of the host computer, and `<port>` can be any available port on the host computer. Optionally, `-h` flag can be included to show the help menu.

**IMPORTANT**: ensure that both port and port + 1 (e.g., 6060 and 6061) are both available on the computer that hosts the server-side application. The server application uses two channels to communicate with each client.

The `-a <ip-address>` option can be omitted. The application will use the default IP address specified by `DEFAULT_SERVER_ADDR` in `common/config.py`.

## Start client-side application

Launch the client-side application **after** launching the server-side application. Launch the client application with
```bash
python3 run_client.py -a <ip-address> -p <port> -n <name>
```
where `<ip-address>` is the local IPv4 address of the *host computer* that runs the server-side application (not to be mistakened for the IPv4 address of the computer that runs the client-side application), and `<port>` is the port that the server-side application uses on the host computer. Optionally, `-h` flag can be included to show the help menu.

Similarly, the `-a <ip-address>` option can be omitted. The application will use the default IP address specified by `DEFAULT_SERVER_ADDR` in `common/config.py`.

*Note: the same `<ip-address>` and `<port>` parameters must be used when launching the server-side application and the client-side application, because they are IPv4 address and ports of the computer that hosts the server-side application.*
