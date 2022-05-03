#pragma once
#include <string>
#include <vector>

#include <boost/asio/connect.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/beast/core.hpp>
#include <boost/beast/websocket.hpp>
#include <boost/log/trivial.hpp>

namespace beast = boost::beast;         // from <boost/beast.hpp>
namespace http = beast::http;           // from <boost/beast/http.hpp>
namespace websocket = beast::websocket; // from <boost/beast/websocket.hpp>
namespace net = boost::asio;            // from <boost/asio.hpp>
using tcp = boost::asio::ip::tcp;       // from <boost/asio/ip/tcp.hpp>

class WebsocketClient {
  public:
    WebsocketClient(std::string ws_host, std::string ws_port,
                    std::string player_name, int sample_rate);
    ~WebsocketClient();

    void Connect();
    void Shutdown();
    void SendChunk(std::vector<int16_t> chunk);

  private:
    bool running = false;

    // Query parameters
    std::string player_name;
    int sample_rate;

    // Websocket options
    std::string ws_host, ws_port;

    // Context for websocket connection
    net::io_context ioc;
    tcp::resolver resolver{ioc};
    websocket::stream<tcp::socket> ws{ioc};
};
