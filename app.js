import { Elm } from "./src/Main.elm";

// Extract the stored data from previous sessions.
var storedData = localStorage.getItem('menage-data');
var flags = storedData ? JSON.parse(storedData) : null;

// Load the Elm app, passing in the stored data.
var app = Elm.Main.init({
  node: document.getElementById('app'),
  flags: flags
});

// Listen for commands from the `setStorage` port.
// Turn the data to a string and put it in localStorage.
app.ports.setStorage.subscribe(function(state) {
  localStorage.setItem('menage-data', JSON.stringify(state));
});
