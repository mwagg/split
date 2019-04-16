import { Elm } from './Main.elm';

const storage = window.localStorage;
const serviceWorkerPath = "service-worker.js";

navigator.serviceWorker.register(serviceWorkerPath);

const persistedSettings = storage.getItem('settings');
let settings = null;
try {
  settings = JSON.parse(persistedSettings);
} catch (e) {
}

const app = Elm.Main.init({ flags: settings });

app.ports.settingsUpdated.subscribe((settings) => {
  const persistedSettings = JSON.stringify(Object.assign({ version: "v1" }, settings));
  storage.setItem('settings', persistedSettings);
});
