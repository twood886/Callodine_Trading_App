// main.js for Callodine Trading Electron App
// ────────────────────────────────────────────────────────────────────────────────
// This version includes `electron-updater` so that a packaged app (e.g. v0.1.1)
// will auto-check against your GitHub Releases and install v0.1.2 when available.
// ────────────────────────────────────────────────────────────────────────────────

const { app, BrowserWindow, dialog } = require('electron');
const { spawn }                      = require('child_process');
const { autoUpdater }                = require('electron-updater');
const path                           = require('path');
const fs                             = require('fs');

// Detect platform
const isWin     = process.platform === 'win32';
// __dirname is the folder where this file lives (project root during dev)
const projectRoot = __dirname;

let resourcesPath;
let rproc;    // reference to the R process so we can kill it on exit
let mainWin;  // reference to the BrowserWindow

// ────────────────────────────────────────────────────────────────────────────────
// 1) When Electron is ready, determine resourcesPath (dev vs. packaged)
// ────────────────────────────────────────────────────────────────────────────────
app.once('ready', () => {
  resourcesPath = app.isPackaged
    ? process.resourcesPath    // when packaged: <install_dir>/resources
    : projectRoot;             // in dev: the repo root
  console.log('◉ resourcesPath =', resourcesPath);
});

// ────────────────────────────────────────────────────────────────────────────────
// 2) Start R/ Rhino (Shiny) via R-Portable
// ────────────────────────────────────────────────────────────────────────────────
function startApp() {
  // 2.1) Locate R-Portable and Rscript.exe
  const rPortableDir = app.isPackaged
    ? path.join(resourcesPath, 'R-Portable')
    : path.join(projectRoot, 'R-Portable');

  const rHome = path.join(rPortableDir, 'App', 'R-Portable');
  const rscriptPath = isWin
    ? path.join(rHome, 'bin', 'Rscript.exe')
    : path.join(rHome, 'bin', 'Rscript');

  console.log('◉ Looking for Rscript at:', rscriptPath);
  console.log('◉ Rscript exists?', fs.existsSync(rscriptPath));

  // 2.2) Decide where rhino.yml lives (projectRoot in dev, resources/app when packaged)
  let configDir;
  if (app.isPackaged) {
    // packaged: electron-builder copies top-level files (including rhino.yml) into <resources>/app
    configDir = path.join(resourcesPath, 'app');
  } else {
    // dev: rhino.yml is at projectRoot/rhino.yml
    configDir = projectRoot;
  }
  // Convert Windows backslashes to forward slashes for R
  const appDir = configDir.replace(/\\/g, '/');

  // 2.3) Build the R expression: setwd(configDir); set shiny options; run rhino::app()
  const expr = [
    `setwd("${appDir}")`,
    "options(shiny.port=8000, shiny.host='0.0.0.0', shiny.launch.browser=FALSE)",
    "rhino::app()"
  ].join(';');
  const args = ['-e', expr];

  // 2.4) Ensure R_HOME points to the portable R and prepend its bin to PATH
  const childEnv = {
    ...process.env,
    R_HOME: rHome,
    PATH:   `${path.join(rHome, 'bin')};${process.env.PATH}`
  };

  // 2.5) Spawn Rscript so Rhino can start Shiny
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

// ────────────────────────────────────────────────────────────────────────────────
// 3) Create the Electron BrowserWindow (once Shiny is listening on port 8000)
// ────────────────────────────────────────────────────────────────────────────────
function createWindow() {
  if (mainWin) return;

  mainWin = new BrowserWindow({
    width:      800,
    height:     400,
    resizable: false,
    webPreferences: {
      nodeIntegration:  false,
      contextIsolation: true
    }
  });

  mainWin.loadURL('http://localhost:8000/');

  // Optional: hide scrollbars via injected CSS
  mainWin.webContents.on('did-finish-load', () => {
    mainWin.webContents.insertCSS(`
      ::-webkit-scrollbar { display: none; }
      body { overflow: hidden !important; }
    `);
  });

  mainWin.on('closed', () => {
    mainWin = null;
  });
}

// ────────────────────────────────────────────────────────────────────────────────
// 4) Watch for Rhino’s “Listening on” message, then open the window.
//    Also set up auto‐update immediately after the window opens.
// ────────────────────────────────────────────────────────────────────────────────
function watchAndLaunch() {
  startApp();

  let launched = false;
  rproc.stdout.on('data', data => {
    const msg = data.toString();
    if (!launched && msg.includes('Listening on')) {
      createWindow();
      launched = true;

      // ──────────────────────────────────────────────────────────────────────────
      // 4.1) Once the window is created, tell electron-updater to check GitHub
      // ──────────────────────────────────────────────────────────────────────────
      autoUpdater.checkForUpdatesAndNotify();
    }
  });

  // Fallback: if we don’t see “Listening on” within 10s, open the window anyway
  setTimeout(() => {
    if (!launched) {
      createWindow();
      autoUpdater.checkForUpdatesAndNotify();
    }
  }, 10000);
}

// ────────────────────────────────────────────────────────────────────────────────
// 5) Register autoUpdater event listeners (optional but recommended for logging)
// ────────────────────────────────────────────────────────────────────────────────
autoUpdater.on('checking-for-update', () => {
  console.log('🔍 Checking for updates…');
});
autoUpdater.on('update-available', info => {
  console.log(`⬆️ Update available: v${info.version}`);
});
autoUpdater.on('update-not-available', () => {
  console.log('✅ No update available');
});
autoUpdater.on('error', err => {
  console.error('❌ Auto-update error:', err);
});
autoUpdater.on('download-progress', progress => {
  console.log(`⬇️ Downloaded ${Math.round(progress.percent)}%`);
});
autoUpdater.on('update-downloaded', info => {
  console.log('✅ Update downloaded:', info.version);
  // Automatically quit & install. If you prefer to prompt the user first, you can
  // show a dialog instead and call quitAndInstall() only when they click “Restart.”
  autoUpdater.quitAndInstall(/* isSilent */ false, /* isForceRunAfter */ true);
});

// ────────────────────────────────────────────────────────────────────────────────
// 6) Electron app lifecycle (quit & clean up R when windows close)
// ────────────────────────────────────────────────────────────────────────────────
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
