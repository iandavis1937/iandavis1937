/* global firebase, db, auth */
'use strict';

// ── DOM refs ──────────────────────────────────────────────────────────────────
const $ = id => document.getElementById(id);

// ── State ─────────────────────────────────────────────────────────────────────
let unsubGame = null;

// ── Boot ──────────────────────────────────────────────────────────────────────
document.addEventListener('DOMContentLoaded', async () => {
  showView('name-view');

  $('name-form').addEventListener('submit', async e => {
    e.preventDefault();
    const name = $('display-name').value.trim();
    if (!name) return;
    await signInAnonymously(name);
  });

  $('join-btn').addEventListener('click', joinGame);
});

// ── Anonymous Auth ────────────────────────────────────────────────────────────
async function signInAnonymously(displayName) {
  try {
    setStatus('name-status', '');
    await auth.setPersistence(firebase.auth.Auth.Persistence.LOCAL);
    const cred = await auth.signInAnonymously();
    await cred.user.updateProfile({ displayName });
    showView('lobby-view');
  } catch (err) {
    setStatus('name-status', 'Sign-in failed: ' + err.message, true);
  }
}

// ── Join Game ─────────────────────────────────────────────────────────────────
// Uses a transaction to safely claim whichever player slot is open.
// First student to join becomes player1 (mean); second becomes player2 (median).
async function joinGame() {
  const user = auth.currentUser;
  if (!user) return;

  const gameId = $('game-id-input').value.trim();
  if (!gameId) {
    setStatus('lobby-status', 'Please enter a Game ID.', true);
    return;
  }

  $('join-btn').disabled = true;
  setStatus('lobby-status', 'Joining…');

  const gameRef = db.collection('games').doc(gameId);
  let assignedSlot = null;

  try {
    await db.runTransaction(async tx => {
      const snap = await tx.get(gameRef);

      if (!snap.exists) throw new Error('not_found');

      const data = snap.data();
      if (data.status !== 'waiting') throw new Error('not_open');

      if (!data.player1) {
        // Claim player1 slot (mean)
        tx.update(gameRef, {
          player1: {
            uid:         user.uid,
            displayName: user.displayName || 'Player 1',
            role:        'mean',
            score:       0,
            ready:       true,
          },
        });
        assignedSlot = 1;
      } else if (!data.player2) {
        // Claim player2 slot (median) and activate the game
        tx.update(gameRef, {
          player2: {
            uid:         user.uid,
            displayName: user.displayName || 'Player 2',
            role:        'median',
            score:       0,
            ready:       true,
          },
          status: 'active',
        });
        assignedSlot = 2;
      } else {
        throw new Error('full');
      }
    });
  } catch (err) {
    const messages = {
      not_found: 'Game not found.',
      not_open:  'That game is no longer accepting players.',
      full:      'This game is already full.',
    };
    setStatus('lobby-status', messages[err.message] || 'Failed to join: ' + err.message, true);
    $('join-btn').disabled = false;
    return;
  }

  sessionStorage.setItem('playerSlot', assignedSlot === 1 ? 'player1' : 'player2');

  if (assignedSlot === 2) {
    // Player 2 — game is now active, go straight in
    location.href = `/game.html?gameId=${gameId}`;
  } else {
    // Player 1 — wait for player 2
    setStatus('lobby-status', '');
    $('lobby-actions').style.display = 'none';
    $('waiting-text').textContent    = 'You\'re the Mean player — waiting for the Median player to join…';
    $('waiting-msg').style.display   = '';
    listenForOpponent(gameId);
  }
}

// ── Wait for Opponent (player 1) ──────────────────────────────────────────────
function listenForOpponent(gameId) {
  if (unsubGame) unsubGame();
  unsubGame = db.collection('games').doc(gameId).onSnapshot(snap => {
    if (!snap.exists) return;
    const data = snap.data();
    if (data.status === 'active' && data.player2) {
      if (unsubGame) { unsubGame(); unsubGame = null; }
      location.href = `/game.html?gameId=${gameId}`;
    }
  });
}

// ── UI helpers ─────────────────────────────────────────────────────────────────
function showView(id) {
  ['name-view', 'lobby-view'].forEach(v => {
    const el = $(v);
    if (el) el.style.display = v === id ? '' : 'none';
  });
}

function setStatus(id, msg, isError = false) {
  const el = $(id);
  if (!el) return;
  el.textContent = msg;
  el.className   = 'status-msg' + (isError ? ' error' : '');
}
