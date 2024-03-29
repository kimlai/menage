import { Elm } from "./src/Main.elm";
import { gsap } from "gsap";
import { Flip } from "gsap/Flip";

gsap.registerPlugin(Flip);

const LOCAL_STORAGE_KEY = "menage-data-v0";

// Extract the stored data from previous sessions.
var storedData = localStorage.getItem(LOCAL_STORAGE_KEY);
var flags = storedData ? JSON.parse(storedData) : null;

// Load the Elm app, passing in the stored data.
var app = Elm.Main.init({
  node: document.getElementById('app'),
  flags: flags
});

// Listen for commands from the `setStorage` port.
// Turn the data to a string and put it in localStorage.
app.ports.setStorage.subscribe(function(state) {
  localStorage.setItem(LOCAL_STORAGE_KEY, JSON.stringify(state));
});

customElements.define("gsap-flip", class extends HTMLElement {
  constructor() {
    super();
    this._flipsState = null;
  }

  set status(value) {
    switch (value) {
      case "inert":
        this._flipsState = null;
        break;
      case "saveState":
        // create a "ghost" element to be animated while the original one is removed from the DOM by Elm,
        // making it possible to run the FLIP animation at the same time.
        const element = document.querySelector("[data-animate]");
        if (element) {
          const boundingRect = element.getBoundingClientRect();
          const ghost = element.cloneNode(true);
          ghost.removeAttribute("data-flip-id");
          ghost.style.position = "absolute";
          ghost.style.top = `${Math.floor(boundingRect.top)}px`;
          ghost.style.left = `${Math.floor(boundingRect.left)}px`;
          ghost.style.width = `${Math.ceil(boundingRect.width) + 1}px`; // +1px to prevent unwanted flexbox wrapping
          ghost.querySelector("label").style.letterSpacing = "1px"; // no idea why this is needed but it is
          ghost.addEventListener("animationend", event => {
            if (event.animationName == "fade-out") {
              document.body.removeChild(ghost);
            }
          });
          document.body.appendChild(ghost);
        }

        requestAnimationFrame(() => {
          this._flipState = Flip.getState("[data-flip-id]");
          this.dispatchEvent(new CustomEvent('flip-state-saved'));
        });
        break;
      case "run":
        Flip.from(this._flipState, {
          targets: "[data-flip-id]",
          duration: 0.5,
          absolute: true,
          ease: "power4.inOut",
          onComplete: () => this.dispatchEvent(new CustomEvent('flip-done'))
        });
        break;
    }
  }
});

if ('serviceWorker' in navigator) {
  navigator.serviceWorker.register(new URL('service-worker.js', import.meta.url), { type: 'module' });
}

// https://web.dev/push-notifications-subscribing-a-user/
function askForNotificationPermission() {
  return new Promise(function(resolve, reject) {
    const permissionResult = Notification.requestPermission(function(result) {
      resolve(result);
    });

    if (permissionResult) {
      permissionResult.then(resolve, reject);
    }
  }).then(function(permissionResult) {
    if (permissionResult === 'granted') {
      subscribeUserToPush();
    }
  });
}

function subscribeUserToPush() {
  document.querySelector(".notification-button").dataset.activating = true;
  document.querySelector('.notification-button').dataset.active = true;
  navigator.serviceWorker.getRegistration().then(function(registration) {
    const subscribeOptions = {
      userVisibleOnly: true,
      applicationServerKey: urlBase64ToUint8Array(
        'BPj113A6FPAs0TsDY5xyJTxL_c7XqiCCBWoo_gXfLnju_lBmMuWoKrgP6RG0COAx27DNDOZR6u8e-fodnlUl2qA',
      ),
    };

    return registration.pushManager.subscribe(subscribeOptions);
  }).then(sendSubscriptionToBackEnd);
}

// https://github.com/screendriver/convert-vapid-public-key/blob/main/src/toUint8Array.ts
function urlBase64ToUint8Array(base64String) {
  const padding = '='.repeat((4 - (base64String.length % 4)) % 4);
  const base64 = (base64String + padding).replace(/-/g, '+').replace(/_/g, '/');

  const rawData = window.atob(base64);
  const outputArray = new Uint8Array(rawData.length);

  for (let i = 0; i < rawData.length; ++i) {
    outputArray[i] = rawData.charCodeAt(i);
  }
  return outputArray;
}

function sendSubscriptionToBackEnd(subscription) {
  return fetch('/.netlify/functions/subscriptions', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify(subscription),
  }).then(function(response) {
    document.querySelector(".notification-button").dataset.activating = false;
    if (!response.ok) {
      document.querySelector('.notification-button').dataset.active = false;
      throw new Error('Bad status code from server.');
    }
  });
}

const initNotifications = () => {
}

customElements.define("menage-notifications", class extends HTMLElement {
  constructor() {
    super();
    if (!('serviceWorker' in navigator) || !('PushManager' in window)) {
      return;
    }
    const template = document.createElement('div');
    template.innerHTML =
      `<button class="notification-button" data-active=false>
        <img alt="notifications inactives" src = "/img/bell-slash.svg" />
        <img alt="notifications activées" src = "/img/bell.svg" />
      </button >`;
    const button = template.firstChild;
    if (Notification.permission === "granted") {
      button.dataset.active = true;
    } else {
      button.addEventListener('click', askForNotificationPermission, { once: true });
    }
    this.appendChild(button);
  }
});
// initNotifications();
