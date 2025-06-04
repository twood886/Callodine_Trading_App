// preload.js
const { contextBridge, ipcRenderer } = require('electron');

// Expose a small API named “electronAPI” to the renderer (Shiny window).
contextBridge.exposeInMainWorld('electronAPI', {
  openPlotWindow: () => {
    // Send an IPC message to the main process
    ipcRenderer.send('open-plot-window');
  }
});