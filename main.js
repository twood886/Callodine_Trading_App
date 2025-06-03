// main.js for Callodine Trading Electron App
const { app, BrowserWindow } = require('electron');
const { spawn }              = require('child_process');
const path                   = require('path');
const fs                     = require('fs');

// Detect platform
const isWin       = process.platform === 'win32';
// __dirname is project root
const projectRoot = __dirname;

let resourcesPath;
let rproc;  // Make R process reference global so we can kill it on exit

app.once('ready', () => {
  resourcesPath = app.isPackaged
    ? process.resourcesPath      // Packaged: <install_dir>/resources
    : projectRoot;               // Dev: project root
  console.log('◉ resourcesPath =', resourcesPath);
});

function startApp() {
  // Locate R-Portable folder
  const rPortableDir = app.isPackaged
    ? path.join(resourcesPath, 'R-Portable')
    : path.join(projectRoot, 'R-Portable');
  const rHome = path.join(rPortableDir, 'App', 'R-Portable');
  const rscriptPath = isWin
    ? path.join(rHome, 'bin', 'Rscript.exe')
    : path.join(rHome, 'bin', 'Rscript');

  console.log('◉ Looking for Rscript at:', rscriptPath);
  console.log('◉ Rscript exists?', fs.existsSync(rscriptPath));

  // Build R expression: setwd into resources/app then launch Rhino
  const appDir = path.join(resourcesPath, 'app').replace(/\\/g, '/');
  const expr = [
    `setwd("${appDir}")`,
    "options(shiny.port=8000,shiny.host='0.0.0.0',shiny.launch.browser=FALSE)",
    "rhino::app()"
  ].join(';');

  const args = ['-e', expr];
  const childEnv = {
    ...process.env,
    R_HOME: rHome,
    // Ensure R's DLLs and runtimes are found
    PATH: `${path.join(rHome, 'bin')};${process.env.PATH}`
  };

  // Spawn Rscript from its own home dir so DLLs load properly
  rproc = spawn(rscriptPath, args, {
    cwd: rHome,
    env: childEnv,
    stdio: ['ignore', 'pipe', 'pipe'],
    shell: false
  });

  rproc.stdout.on('data', d => console.log(`[R stdout] ${d.toString().trim()}`));
  rproc.stderr.on('data', d => console.error(`[R stderr] ${d.toString().trim()}`));
  rproc.on('close', code => console.log(`[R exited] code ${code}`));

  return rproc;
}

let mainWin;
function createWindow() {
  if (mainWin) return;
  mainWin = new BrowserWindow({
    width: 800,
    height: 400,
    resizable: false,
    webPreferences: { nodeIntegration: false, contextIsolation: true }
  });
  mainWin.loadURL('http://localhost:8000/');
    // Hide scrollbar via CSS injection
  mainWin.webContents.on('did-finish-load', () => {
    mainWin.webContents.insertCSS(`
      ::-webkit-scrollbar { display: none; }
      body { overflow: hidden !important; }
    `);
  });
  mainWin.on('closed', () => { mainWin = null; });
}

function watchAndLaunch() {
  startApp();
  let launched = false;
  // Wait for Rhino/Shiny to signal readiness
  rproc.stdout.on('data', data => {
    const msg = data.toString();
    if (!launched && msg.includes('Listening on')) {
      createWindow();
      launched = true;
    }
  });
  // Fallback open after 10s
  setTimeout(() => { if (!launched) createWindow(); }, 10000);
}

// Always quit the app and kill R, even on Windows
app.on('window-all-closed', () => {
  if (rproc) rproc.kill();
  app.quit();
});

app.whenReady().then(watchAndLaunch);
app.on('activate', () => { if (BrowserWindow.getAllWindows().length === 0) watchAndLaunch(); });
