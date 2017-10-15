window.pointerModulePort = function(app) {
  const getCanvas = () => document.getElementsByTagName('canvas')[0];

  app.ports.lockPointerPort.subscribe(function() {
    let canvas = getCanvas();

    if(canvas)  {
      canvas.requestPointerLock();
      canvas.webkitRequestFullscreen();
    }
  });

  document.addEventListener('pointerlockchange', e => {
    let canvas = getCanvas(),
        element = document.pointerLockElement || document.mozPointerLockElement;

    app.ports.pointerLockChangePort.send(canvas === element);
  }, false);
};