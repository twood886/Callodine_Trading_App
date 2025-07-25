// preload.js
const { contextBridge, ipcRenderer } = require('electron');

// Expose a small API named “electronAPI” to the renderer (Shiny window).
contextBridge.exposeInMainWorld('electronAPI', {
  openPlotWindow: () => {ipcRenderer.send('open-plot-window');},
  openRebalWindow: () => {ipcRenderer.send('open-rebal-window');},
});