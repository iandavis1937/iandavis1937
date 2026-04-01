/* global firebase */

// Config is loaded from firebase-config.js (gitignored).
// That file sets window.FIREBASE_CONFIG from your project's values.
// See .env.example for the required fields and README.md for setup steps.
if (!window.FIREBASE_CONFIG) {
  throw new Error(
    'Missing Firebase config. Copy public/js/firebase-config.js from .env.example and fill in your project values.'
  );
}

firebase.initializeApp(window.FIREBASE_CONFIG);

const db   = firebase.firestore();
const auth = firebase.auth();
