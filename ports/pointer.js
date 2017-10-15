window.pointerModulePort = function(app) {
  app.ports.lockPointerPort.subscribe(function() {
    let canvas = document.getElementsByTagName('canvas')[0];

    if(canvas)  {
      canvas.requestPointerLock();
      canvas.webkitRequestFullscreen();
    }
  });
};