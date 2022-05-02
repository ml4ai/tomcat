#include "WebsocketClient.h"

namespace beast = boost::beast;         // from <boost/beast.hpp>
namespace http = beast::http;           // from <boost/beast/http.hpp>
namespace websocket = beast::websocket; // from <boost/beast/websocket.hpp>
namespace net = boost::asio;            // from <boost/asio.hpp>
using tcp = boost::asio::ip::tcp;       // from <boost/asio/ip/tcp.hpp>
using namespace std;

WebsocketClient::WebsocketClient(string ws_host, string ws_port,
                                 string player_name, int sample_rate) {
    this->ws_host = ws_host;
    this->ws_port = ws_port;

    this->player_name = player_name;
    this->sample_rate = sample_rate;
}

WebsocketClient::~WebsocketClient() {
    if (running) {
        Shutdown();
    }
}

void WebsocketClient::Connect() {
    // Look up the domain name
    auto const results = resolver.resolve(ws_host, ws_port);

    // Make the connection on the IP address we get from a lookup
    net::connect(ws.next_layer(), results.begin(), results.end());

    // Set a decorator to change the User-Agent of the handshake
    ws.set_option(
        websocket::stream_base::decorator([](websocket::request_type& req) {
            req.set(http::field::user_agent,
                    string(BOOST_BEAST_VERSION_STRING) +
                        " websocket-client-coro");
        }));

    // Perform the websocket handshake
    ws.handshake(ws_host,
                 "/?sampleRate=" + to_string(sample_rate) +
                     "&id=" + player_name);
    ws.binary(true);

    running = true;
}

void WebsocketClient::SendChunk(vector<int16_t> chunk) {
    ws.write(net::buffer(chunk));
}

void WebsocketClient::Shutdown() {
    ws.close(websocket::close_code::normal);
    running = false;
}
