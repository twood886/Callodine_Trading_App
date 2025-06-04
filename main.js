// main.js for Callodine Trading Electron App
// ────────────────────────────────────────────────────────────────────────────────
// Includes `electron-updater` so a packaged app auto-checks against GitHub Releases
// ────────────────────────────────────────────────────────────────────────────────

const { app, BrowserWindow, dialog } = require('electron');
const { spawn }                     = require('child_process');
const { autoUpdater }               = require('electron-updater');
const path                          = require('path');
const fs                            = require('fs');

// Detect platform
const isWin     = process.platform === 'win32';
const projectRoot = __dirname;

let resourcesPath;
let rproc;   // reference to the R process to kill on exit
let mainWin; // reference to the BrowserWindow

// Configure logging for electron-updater (optional, but good for debugging)
// You can also set this up to write to a file if needed
autoUpdater.logger = require('electron-log');
autoUpdater.logger.transports.file.level = 'info';
autoUpdater.logger.transports.console.level = 'info'; // Also log to console

// ════════════════════════════════════════════════════════════════════════════════
// 1) When Electron is ready, set resourcesPath (dev vs. packaged), then launch
// ════════════════════════════════════════════════════════════════════════════════
app.once('ready', () => {
  resourcesPath = app.isPackaged
    ? process.resourcesPath   // when packaged: <install_dir>/resources
    : projectRoot;            // in dev: project root
  console.log('◉ resourcesPath =', resourcesPath);
  // Note: watchAndLaunch is now called via app.whenReady().then(watchAndLaunch)
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
    width:       800,
    height:      400,
    resizable: false, // Consider if you want this true for better UX
    webPreferences: {
      nodeIntegration:  false,
      contextIsolation: true
      // preload: path.join(__dirname, 'preload.js') // Recommended for security
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
  const launchAndCheckForUpdates = () => {
    if (!launched) {
      createWindow();
      launched = true;

      console.log('🔰 App version (from package.json):', app.getVersion());
      
      // Only check for updates in a packaged app
      if (app.isPackaged) {
        console.log('🚀 Production mode: Checking for updates...');
        autoUpdater.checkForUpdatesAndNotify(); // This is the key call
      } else {
        console.log('🔧 Development mode: Auto-update check skipped.');
      }
    }
  };

  rproc.stdout.on('data', data => {
    const msg = data.toString();
    if (msg.includes('Listening on')) {
      launchAndCheckForUpdates();
    }
  });

  // Fallback: after 10s, if still not launched, show window & check for updates
  setTimeout(() => {
    if (!launched) { // Check !launched again in case it launched just before timeout
      console.log('⏳ Timeout reached, attempting to launch window and check updates.');
      launchAndCheckForUpdates();
    }
  }, 10000);
}

// ════════════════════════════════════════════════════════════════════════════════
// 5) Auto‐Updater event handlers (for logging & install on download)
//    These are crucial for debugging your update process!
// ════════════════════════════════════════════════════════════════════════════════
autoUpdater.on('checking-for-update', () => {
  console.log('🔍 Checking for updates…');
  if (autoUpdater.logger) autoUpdater.logger.info('🔍 Checking for updates…');
});
autoUpdater.on('update-available', info => {
  console.log(`⬆️ Update available: v${info.version}`);
  if (autoUpdater.logger) autoUpdater.logger.info(`⬆️ Update available: v${info.version}`);
});
autoUpdater.on('update-not-available', () => {
  console.log('✅ No update available for this version.');
  if (autoUpdater.logger) autoUpdater.logger.info('✅ No update available for this version.');
});
autoUpdater.on('error', err => {
  console.error('❌ Auto-update error:', err.message);
  if (autoUpdater.logger) autoUpdater.logger.error('❌ Auto-update error:', err);
});
autoUpdater.on('download-progress', progress => {
  const msg = `⬇️ Downloaded ${Math.round(progress.percent)}% (${progress.bytesPerSecond} B/s)`;
  console.log(msg);
  if (autoUpdater.logger) autoUpdater.logger.info(msg);
  // Example: mainWin.webContents.send('download-progress', progress.percent);
});
autoUpdater.on('update-downloaded', info => {
  console.log(`✅ Update v${info.version} downloaded. Application will quit and install.`);
  if (autoUpdater.logger) autoUpdater.logger.info(`✅ Update v${info.version} downloaded. Application will quit and install.`);
  // The update will automatically be installed when the app quits.
  // autoUpdater.quitAndInstall(isSilent, isForceRunAfterInstall)
  // isSilent: false will ask user if they want to install now.
  // isForceRunAfterInstall: true will restart the app after install.
  autoUpdater.quitAndInstall(false, true); 
});

// ════════════════════════════════════════════════════════════════════════════════
// 6) App lifecycle: kill R when windows close, launch on activate
// ══════════════════════════════════════════════════════════════════════════════════
app.on('window-all-closed', () => {
  console.log('🚪 All windows closed.');
  if (rproc) {
    console.log('🔪 Killing R process.');
    rproc.kill();
  }
  // On macOS, it's common for applications to stay active until the user quits explicitly
  if (process.platform !== 'darwin') {
    app.quit();
  }
});

app.on('before-quit', () => {
  console.log('👋 Application is about to quit.');
  if (rproc) {
    console.log('🔪 Ensuring R process is killed before quit.');
    rproc.kill(); // Ensure R process is killed if not already
  }
});

app.whenReady().then(() => {
  console.log('🎉 App is ready.');
  watchAndLaunch(); // Initial launch

  app.on('activate', () => {
    console.log('▶️ App activated.');
    // On macOS it's common to re-create a window in the app when the
    // dock icon is clicked and there are no other windows open.
    if (BrowserWindow.getAllWindows().length === 0) {
      console.log('💨 No windows open, re-launching.');
      watchAndLaunch(); // This will restart the R process and open a new window
    } else if (mainWin) {
        mainWin.show(); // If mainWin still exists but is hidden, show it
    }
  });
});