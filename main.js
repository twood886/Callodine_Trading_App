// main.js for Callodine Trading Electron App
// ────────────────────────────────────────────────────────────────────────────────
// Includes `electron-updater` so a packaged app auto-checks against GitHub Releases
// ────────────────────────────────────────────────────────────────────────────────

const { app, BrowserWindow, dialog } = require('electron');
const { spawn }                      = require('child_process');
const { autoUpdater }                = require('electron-updater');
const path                           = require('path');
const fs                             = require('fs');

// Detect platform
const isWin      = process.platform === 'win32';
const projectRoot = __dirname;

let resourcesPath;
let rproc;    // reference to the R process to kill on exit
let mainWin;  // reference to the BrowserWindow

// ════════════════════════════════════════════════════════════════════════════════
// 1) When Electron is ready, set resourcesPath (dev vs. packaged), then launch
// ════════════════════════════════════════════════════════════════════════════════
app.once('ready', () => {
  resourcesPath = app.isPackaged
    ? process.resourcesPath      // when packaged: <install_dir>/resources
    : projectRoot;               // in dev: project root
  console.log('◉ resourcesPath =', resourcesPath);
});

// ════════════════════════════════════════════════════════════════════════════════
// 2) R/ Rhino (Shiny) via R-Portable
// ════════════════════════════════════════════════════════════════════════════════
function startApp() {
  const rPortableDir = app.isPackaged
    ? path.join(resourcesPath, 'R-Portable')
    : path.join(projectRoot, 'R-Portable');

  const rHome = path.join(rPortableDir, 'App', 'R-Portable');
  const rscriptPath = isWin
    ? path.join(rHome, 'bin', 'Rscript.exe')
    : path.join(rHome, 'bin', 'Rscript');

  console.log('◉ Looking for Rscript at:', rscriptPath);
  console.log('◉ Rscript exists?', fs.existsSync(rscriptPath));

  let configDir;
  if (app.isPackaged) {
    // packaged: top-level files (rhino.yml) copied into <resources>/app
    configDir = path.join(resourcesPath, 'app');
  } else {
    // dev: configDir is projectRoot where rhino.yml lives
    configDir = projectRoot;
  }
  const appDir = configDir.replace(/\\/g, '/');

  const expr = [
    `setwd("${appDir}")`,
    "options(shiny.port=8000, shiny.host='0.0.0.0', shiny.launch.browser=FALSE)",
    "rhino::app()"
  ].join(';');
  const args = ['-e', expr];

  const childEnv = {
    ...process.env,
    R_HOME: rHome,
    PATH:   `${path.join(rHome, 'bin')};${process.env.PATH}`
  };

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

// ════════════════════════════════════════════════════════════════════════════════
// 3) Create the BrowserWindow after Shiny is listening on port 8000
// ════════════════════════════════════════════════════════════════════════════════
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

// ════════════════════════════════════════════════════════════════════════════════
// 4) Start Rhino, wait for “Listening on”, then open Window & check for updates
// ════════════════════════════════════════════════════════════════════════════════
function watchAndLaunch() {
  startApp();

  let launched = false;
  rproc.stdout.on('data', data => {
    const msg = data.toString();
    if (!launched && msg.includes('Listening on')) {
      createWindow();
      launched = true;

      // ──────────────────────────────────────────────────────────────────────────
      // Log the app’s version immediately after the window opens:
      console.log('🔰 App version (from package.json):', app.getVersion());
      // Then check for updates:
      autoUpdater.checkForUpdatesAndNotify();
    }
  });

  // Fallback: after 10s, if still not launched, show window & check for updates
  setTimeout(() => {
    if (!launched) {
      createWindow();
      console.log('🔰 App version (from package.json):', app.getVersion());
      autoUpdater.checkForUpdatesAndNotify();
    }
  }, 10000);
}

// ════════════════════════════════════════════════════════════════════════════════
// 5) Auto‐Updater event handlers (for logging & install on download)
// ════════════════════════════════════════════════════════════════════════════════
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
  autoUpdater.quitAndInstall(false, true);
});

// ════════════════════════════════════════════════════════════════════════════════
// 6) App lifecycle: kill R when windows close, launch on activate
// ══════════════════════════════════════════════════════════════════════════════════
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
