// Code adapted from
// https://github.com/vin-ni/Google-Cloud-Speech-Node-Socket-Playground

'use strict'

let connectedAtLeastOnce = false;

var processWebSocketMessage = function(event) {
    var data = JSON.parse(event.data);
    if ("participantId" in data) {
        document.getElementById("participantId").innerHTML = data["participantId"];
        document.getElementById("exp").style.visibility = "visible";
    }

    if ("transcript" in data) {
        let transcriptDiv = document.getElementById("transcript")
        transcriptDiv.innerHTML = data["transcript"];
        if (data["is_final"]) {
            transcriptDiv.style="color:black;";
        }
        else {
            transcriptDiv.style="color:gray;"
        }
    }
};
let destination="ws://localhost:8888"
function makeSocket(destination) {
    var ws = new WebSocket(destination);
    const StopButton = document.getElementById('stop');
    // Listen for messages
    ws.onopen = function(event) {
        connectedAtLeastOnce=true;

        document.getElementById("connectedIndicator").innerHTML = "Yes";
        // if (document.getElementById("connectedIndicator").innerHTML == "Yes") {
        //     document.getElementById("exp").style.visibility = "visible";
        // }


        var connectButton = document.getElementById("connectButton");
        connectButton.disabled = true;
        console.log("Websocket connected!")
    }

    ws.onmessage = processWebSocketMessage;

    ws.onerror = (error) => {
        console.error(
            "Error in creating websocket. Trying again in 5 seconds.");
        ws.close();
    };

    StopButton.addEventListener('click', () => {
        ws.close(1000);
        connectButton.disabled = false;
              });

    ws.onclose = function(event) {
        document.getElementById("connectedIndicator").innerHTML =
            "No";

        if (!connectedAtLeastOnce) {
            setTimeout(function() { ws = makeSocket(destination); }, 5000);
        }
    };

    return ws;
}

// A wrapper class to create a websocket that tries repeatedly to connect to
// a given URL. It does not try to reconnect if the connection is terminated
// after successfully connecting at least once.
class PersistentSocket {
    constructor(destination) {
        this.destination = destination;
        this.ws = makeSocket(this.destination);
    }

    send(data) {
        if (this.ws.readyState == 1) {
            this.ws.send(data);
        }
    }
}

let socket,
    sampleRate;

document.getElementById("connectButton").onclick = function() {
    // Get parameters from URL query string
    const params = new URLSearchParams(window.location.search);
    const participantId = params.get("id");
    let ID = document.getElementById('participantId').value.toString();

    var context = getAudioContext();
    var destination = "{{ ws_url }}" +
                      "?id=" + ID +
                      "&sampleRate=" + context.sampleRate;
    socket = new PersistentSocket(destination);
    initRecording(context);

};


//================= CONFIG =================
// Stream Audio
let AudioContext, context, processor, input, globalStream;

// vars
let audioElement = document.querySelector('audio'), finalWord = false,
    resultText = document.getElementById('ResultText'),
    removeLastSentence = true, streamStreaming = false;

// audioStream constraints
const constraints = {audio : true, video : false};

//================= RECORDING =================

function getAudioContext() {
    AudioContext = window.AudioContext || window.webkitAudioContext;
    context = new AudioContext({
        // if Non-interactive, use 'playback' or 'balanced'
        // https://developer.mozilla.org/en-US/docs/Web/API/AudioContextLatencyCategory
        latencyHint : 'interactive',
    });
    return context;
}

function initRecording(context) {
    streamStreaming = true;
    processor = context.createScriptProcessor(0, 1, 1);
    processor.connect(context.destination);
    context.resume();

    var handleSuccess = function(stream) {
        globalStream = stream;
        input = context.createMediaStreamSource(stream);
        input.connect(processor);

        processor.onaudioprocess = function(audioProcessingEvent) {
            var channelData = audioProcessingEvent.inputBuffer.getChannelData(0);
            socket.send(channelData);
        };
    };

    navigator.mediaDevices.getUserMedia(constraints).then(handleSuccess);
}


