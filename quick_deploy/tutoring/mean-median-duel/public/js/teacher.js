/* global firebase, db, auth */
'use strict';

// ── Passcode ──────────────────────────────────────────────────────────────────
// Change this value (and update README.md) to set a new teacher passcode.
const TEACHER_PASSCODE = 'classroom1';

// ── State ─────────────────────────────────────────────────────────────────────
let currentGameId  = null;
let unsubGame      = null;
let latestGameData = null;

// ── DOM ───────────────────────────────────────────────────────────────────────
const $ = id => document.getElementById(id);

// ── Boot ──────────────────────────────────────────────────────────────────────
document.addEventListener('DOMContentLoaded', () => {
  showView('login-view');

  $('login-form').addEventListener('submit', handleLogin);
  $('logout-btn').addEventListener('click', handleLogout);
  $('teacher-create-btn').addEventListener('click', createGame);
  $('observe-btn').addEventListener('click', observeGame);

  // Controls
  $('pause-btn').addEventListener('click', togglePause);
  $('lock-p1-btn').addEventListener('click', () => togglePlayerLock('player1InputLocked'));
  $('lock-p2-btn').addEventListener('click', () => togglePlayerLock('player2InputLocked'));
  $('buffer-apply-btn').addEventListener('click', applyBuffer);
  $('force-round-btn').addEventListener('click', forceNextRound);
  $('end-game-btn').addEventListener('click', endGame);
});

// ── Auth ──────────────────────────────────────────────────────────────────────
async function handleLogin(e) {
  e.preventDefault();
  const entered = $('teacher-passcode').value;
  setLoginError('');

  if (entered !== TEACHER_PASSCODE) {
    setLoginError('Incorrect passcode.');
    return;
  }

  try {
    // Sign in anonymously so Firestore write rules are satisfied
    await auth.setPersistence(firebase.auth.Auth.Persistence.SESSION);
    await auth.signInAnonymously();
    showView('dashboard-view');
  } catch (err) {
    setLoginError('Sign-in error: ' + err.message);
  }
}

function handleLogout() {
  auth.signOut();
  if (unsubGame) { unsubGame(); unsubGame = null; }
  $('teacher-passcode').value = '';
  showView('login-view');
}

// ── Create game ───────────────────────────────────────────────────────────────
async function pickGameId() {
  for (let i = 0; i < 10; i++) {
    const id = String(Math.floor(1000 + Math.random() * 9000));
    const snap = await db.collection('games').doc(id).get();
    if (!snap.exists) return id;
  }
  throw new Error('Could not find a free game ID — try again.');
}

async function createGame() {
  const maxRounds = parseInt($('max-rounds-input').value, 10);
  if (isNaN(maxRounds) || maxRounds < 1) {
    $('create-game-status').textContent = 'Rounds must be a positive number.';
    return;
  }

  $('teacher-create-btn').disabled = true;
  $('create-game-status').textContent = 'Creating…';
  $('created-game-id-box').style.display = 'none';

  try {
    const gameId = await pickGameId();
    await db.collection('games').doc(gameId).set({
      player1: null,
      player2: null,
      status:       'waiting',
      currentRound: 1,
      maxRounds,
      teacherControls: {
        paused:             false,
        player1InputLocked: false,
        player2InputLocked: false,
        meanBufferSeconds:  1,
      },
      round: {
        cards:            [],
        mean:             null,
        median:           null,
        meanRevealedAt:   null,
        medianRevealedAt: null,
        winner:           null,
        winnerChoice:     null,
        pointsEarned:     null,
        revealed:         false,
      },
      createdAt: firebase.firestore.FieldValue.serverTimestamp(),
    });

    $('created-game-id').textContent       = gameId;
    $('created-game-id-box').style.display = '';
    $('create-game-status').textContent    = '';

    // Auto-populate the observe field for convenience
    $('game-id-observe').value = gameId;
  } catch (err) {
    $('create-game-status').textContent = 'Error: ' + err.message;
  } finally {
    $('teacher-create-btn').disabled = false;
  }
}

// ── Observe game ──────────────────────────────────────────────────────────────
function observeGame() {
  const id = $('game-id-observe').value.trim().toUpperCase();
  if (!id) return;

  if (unsubGame) { unsubGame(); unsubGame = null; }
  currentGameId = id;
  $('observe-status').textContent = 'Connecting…';
  disableControls(true);

  unsubGame = db.collection('games').doc(id).onSnapshot(snap => {
    if (!snap.exists) {
      $('observe-status').textContent = 'Game not found.';
      disableControls(true);
      return;
    }
    latestGameData = snap.data();
    $('observe-status').textContent = '';
    renderDashboard(latestGameData);
    disableControls(false);
  }, err => {
    $('observe-status').textContent = 'Error: ' + err.message;
  });
}

// ── Render dashboard ──────────────────────────────────────────────────────────
function renderDashboard(data) {
  // Status bar
  $('dash-status').textContent  = data.status;
  $('dash-round').textContent   = `${data.currentRound} / ${data.maxRounds}`;
  $('dash-p1-name').textContent = data.player1?.displayName || '—';
  $('dash-p1-score').textContent = data.player1?.score ?? 0;
  $('dash-p2-name').textContent = data.player2?.displayName || '—';
  $('dash-p2-score').textContent = data.player2?.score ?? 0;

  // Teacher always sees cards + mean + median
  const round = data.round || {};
  renderTeacherCards(round.cards || []);
  $('teacher-mean').textContent   = round.mean   ?? '—';
  $('teacher-median').textContent = round.median ?? '—';

  // Control states
  const tc = data.teacherControls || {};
  setToggleState('pause-btn', tc.paused, 'Resume Game', 'Pause Game');
  setToggleState('lock-p1-btn', tc.player1InputLocked,
    `🔒 Unlock ${data.player1?.displayName || 'P1'}`,
    `🔓 Lock ${data.player1?.displayName || 'P1'}`);
  setToggleState('lock-p2-btn', tc.player2InputLocked,
    `🔒 Unlock ${data.player2?.displayName || 'P2'}`,
    `🔓 Lock ${data.player2?.displayName || 'P2'}`);

  // Buffer
  $('buffer-active').textContent = tc.meanBufferSeconds ?? 1;
  if (document.activeElement !== $('buffer-input')) {
    $('buffer-input').value = tc.meanBufferSeconds ?? 1;
  }

  // Force/end buttons
  const canForce = data.status === 'active' || data.status === 'round_over';
  $('force-round-btn').disabled = !canForce;
  $('end-game-btn').disabled    = data.status === 'finished';
}

function renderTeacherCards(cards) {
  const container = $('teacher-cards');
  container.innerHTML = '';
  if (!cards.length) {
    container.innerHTML = '<span class="no-cards">No cards yet</span>';
    return;
  }
  cards.forEach(num => {
    const card = document.createElement('div');
    card.className   = 'teacher-card';
    card.textContent = num;
    container.appendChild(card);
  });
}

// ── Controls ──────────────────────────────────────────────────────────────────
async function togglePause() {
  if (!currentGameId || !latestGameData) return;
  const current = latestGameData.teacherControls?.paused ?? false;
  await writeControl({ 'teacherControls.paused': !current });
}

async function togglePlayerLock(field) {
  if (!currentGameId || !latestGameData) return;
  const current = latestGameData.teacherControls?.[field] ?? false;
  await writeControl({ [`teacherControls.${field}`]: !current });
}

async function applyBuffer() {
  if (!currentGameId) return;
  const val = parseFloat($('buffer-input').value);
  if (isNaN(val) || val < 0) {
    $('buffer-error').textContent = 'Must be a non-negative number (e.g. 0.5, 2).';
    return;
  }
  $('buffer-error').textContent = '';
  await writeControl({ 'teacherControls.meanBufferSeconds': val });
}

async function forceNextRound() {
  if (!currentGameId) return;
  if (!confirm('Force next round? The current round will end immediately.')) return;
  await db.collection('games').doc(currentGameId).update({ status: 'round_over' });
}

async function endGame() {
  if (!currentGameId) return;
  if (!confirm('End the game now?')) return;
  await db.collection('games').doc(currentGameId).update({ status: 'finished' });
}

async function writeControl(fields) {
  try {
    await db.collection('games').doc(currentGameId).update(fields);
  } catch (err) {
    console.error('Control write failed:', err);
  }
}

// ── UI helpers ─────────────────────────────────────────────────────────────────
function showView(id) {
  ['login-view', 'dashboard-view'].forEach(v => {
    const el = $(v);
    if (el) el.style.display = v === id ? '' : 'none';
  });
}

function setLoginError(msg) {
  $('login-error').textContent = msg;
}

function setToggleState(btnId, isActive, activeLabel, inactiveLabel) {
  const btn = $(btnId);
  btn.textContent = isActive ? activeLabel : inactiveLabel;
  btn.classList.toggle('active-control', isActive);
}

function disableControls(disable) {
  ['pause-btn','lock-p1-btn','lock-p2-btn','buffer-apply-btn',
   'force-round-btn','end-game-btn'].forEach(id => {
    $(id).disabled = disable;
  });
}
