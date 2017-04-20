function makeButton (msg, onClick) {
    var btn = document.createElement("button");
    btn.onclick = onClick;
    var content = document.createTextNode(msg);
    btn.appendChild(content);
    return btn;
}

function videoWidget() {
    var ourDiv = document.createElement("div");
    var stopped = makeButton("Stop video!", null);
    ourDiv.appendChild(stopped);
    function handleStream(stream) {
        var videoTag = document.createElement("video");
        ourDiv.appendChild(videoTag);
        videoTag.srcObject = stream;
        videoTag.play();
        stopped.addEventListener("click", function() {
                                            var i =0;
                                            var tracks = stream.getTracks();
                                            for(; i< tracks.length; ++i) {
                                                tracks[i].stop();
                                            }
                                          }
                                );
        var tracks = stream.getTracks();
        var i = 0;
        for(; i< tracks.length; ++i) {
            tracks[i].addEventListener('ended', function() { videoTag.srcObject = null;});
        }
    }
    navigator.mediaDevices.getUserMedia({audio:true, video:true}).then(handleStream);
    document.body.appendChild(ourDiv);
    stopped.addEventListener("click", function () {document.body.removeChild(ourDiv);});
}

function main () {
    document.body.innerHTML = '';
    document.body.appendChild(makeButton("Load video!", videoWidget));

}

main();
