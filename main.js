// main.js for Callodine Trading Electron App
const { app, BrowserWindow } = require('electron');
const { spawn }              = require('child_process');
const path                   = require('path');
const fs                     = require('fs');

// Detect platform
const isWin       = process.platform === 'win32';
// __dirname is the folder where this file lives (your project root during dev)
const projectRoot = __dirname;

let resourcesPath;
let rproc;  // global reference to the R process so we can kill it on exit

app.once('ready', () => {
  // When packaged, `resourcesPath` = <install_dir>/resources
  // In dev, `resourcesPath` = projectRoot
  resourcesPath = app.isPackaged
    ? process.resourcesPath
    : projectRoot;
  console.log('◉ resourcesPath =', resourcesPath);
});

function startApp() {
  // ─── 1. Locate R-Portable and Rscript.exe ─────────────────────────────────────
  const rPortableDir = app.isPackaged
    ? path.join(resourcesPath, 'R-Portable')
    : path.join(projectRoot, 'R-Portable');

  const rHome = path.join(rPortableDir, 'App', 'R-Portable');
  const rscriptPath = isWin
    ? path.join(rHome, 'bin', 'Rscript.exe')
    : path.join(rHome, 'bin', 'Rscript');

  console.log('◉ Looking for Rscript at:', rscriptPath);
  console.log('◉ Rscript exists?', fs.existsSync(rscriptPath));

  // ─── 2. Decide where rhino.yml actually lives ─────────────────────────────────
  //
  // In dev mode:         rhino.yml is at projectRoot/rhino.yml
  // Once packaged:       rhino.yml is at <resources>/app/rhino.yml
  let configDir;
  if (app.isPackaged) {
    // packaged: resourcesPath points to <install_dir>/resources,
    // and electron-builder copies your top-level files into <resources>/app
    configDir = path.join(resourcesPath, 'app');
  } else {
    // dev: configDir is just projectRoot
    configDir = projectRoot;
  }
  // Replace backslashes with forward slashes for R on Windows:
  const appDir = configDir.replace(/\\/g, '/');

  // Build the R expression:
  //  1) setwd to configDir (so Rhino can find rhino.yml)
  //  2) set Shiny to listen on 0.0.0.0:8000
  //  3) run rhino::app()
  const expr = [
    `setwd("${appDir}")`,
    "options(shiny.port=8000,shiny.host='0.0.0.0',shiny.launch.browser=FALSE)",
    "rhino::app()"
  ].join(';');

  const args = ['-e', expr];

  // Ensure R_HOME is the portable R, and prepend R's bin folder to PATH:
  const childEnv = {
    ...process.env,
    R_HOME: rHome,
    PATH:   `${path.join(rHome, 'bin')};${process.env.PATH}`
  };

  // ─── 3. Spawn Rscript with cwd=rHome so it loads from R-Portable ─────────────────
  rproc = spawn(rscriptPath, args, {
    cwd:   rHome,
    env:   childEnv,
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
    width:  800,
    height: 400,
    resizable: false,
    webPreferences: {
      nodeIntegration:  false,
      contextIsolation: true
    }
  });

  // Once Rhino/Shiny starts, it will be listening on port 8000
  mainWin.loadURL('http://localhost:8000/');
  
  // Optionally hide scrollbars via injected CSS
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
  rproc.stdout.on('data', data => {
    const msg = data.toString();
    if (!launched && msg.includes('Listening on')) {
      createWindow();
      launched = true;
    }
  });

  // Fallback: if Rhino didn’t print “Listening on …” within 10s, open the window anyway
  setTimeout(() => {
    if (!launched) createWindow();
  }, 10_000);
}

// Always kill the R process and quit when all windows are closed
app.on('window-all-closed', () => {
  if (rproc) rproc.kill();
  app.quit();
});

app.whenReady().then(watchAndLaunch);
app.on('activate', () => {
  if (BrowserWindow.getAllWindows().length === 0) {
    watchAndLaunch();
  }
});
