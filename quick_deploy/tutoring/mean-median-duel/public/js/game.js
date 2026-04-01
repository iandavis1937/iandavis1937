/* global firebase, db, auth */
'use strict';

// ── State ─────────────────────────────────────────────────────────────────────
let gameId, myUid, myPlayerKey, myRole;
let latestData         = null;
let unsubscribe        = null;
let roundTimerInterval = null;
let medianRevealTimer  = null;
let roundAdvanceTimer  = null;
let initializingRound  = false;
let lastInitRound      = -1; // currentRound value when we last kicked off init

// ── DOM ───────────────────────────────────────────────────────────────────────
const $ = id => document.getElementById(id);

// ── Boot ──────────────────────────────────────────────────────────────────────
document.addEventListener('DOMContentLoaded', () => {
  const params = new URLSearchParams(location.search);
  gameId = params.get('gameId');
  if (!gameId) { location.href = '/'; return; }

  auth.onAuthStateChanged(user => {
    if (!user) { location.href = '/'; return; }
    myUid = user.uid;
    startListener();
  });

  // Event wiring
  $('submit-btn').addEventListener('click',    () => latestData && submitAnswer());
  $('answer-input').addEventListener('keydown', e => {
    if (e.key === 'Enter') latestData && submitAnswer();
  });
  $('btn-take-mean').addEventListener('click',   () => latestData && makeChoice('mean'));
  $('btn-take-median').addEventListener('click', () => latestData && makeChoice('median'));
  $('play-again-btn').addEventListener('click',  () => latestData && playAgain());
});

// ── Firestore listener ────────────────────────────────────────────────────────
function startListener() {
  unsubscribe = db.collection('games').doc(gameId).onSnapshot(snap => {
    if (!snap.exists) { location.href = '/'; return; }
    latestData = snap.data();

    const storedSlot = sessionStorage.getItem('playerSlot');
    if (storedSlot) {
      myPlayerKey = storedSlot;
    } else if (latestData.player1?.uid === myUid) {
      myPlayerKey = 'player1';
    } else if (latestData.player2?.uid === myUid) {
      myPlayerKey = 'player2';
    } else {
      return; // Not a player in this game
    }
    myRole = roleForRound(myPlayerKey, latestData.currentRound);

    render(latestData);
  }, err => console.error('Snapshot error:', err));
}

// ── Render dispatch ───────────────────────────────────────────────────────────
function render(data) {
  updateHeader(data);
  applyTeacherControls(data);

  switch (data.status) {
    case 'active':     return renderActive(data);
    case 'choosing':   return renderChoosing(data);
    case 'round_over': return renderRoundOver(data);
    case 'finished':   return renderFinished(data);
  }
}

// ── Role helper ───────────────────────────────────────────────────────────────
// P1 is mean on odd rounds, median on even rounds. P2 is the opposite.
function roleForRound(playerKey, roundNumber) {
  const p1IsMean = roundNumber % 2 === 1;
  if (playerKey === 'player1') return p1IsMean ? 'mean' : 'median';
  return p1IsMean ? 'median' : 'mean';
}

// ── Header ────────────────────────────────────────────────────────────────────
function updateHeader(data) {
  $('p1-name').textContent    = data.player1?.displayName || 'Player 1';
  $('p1-score').textContent   = data.player1?.score ?? 0;
  $('p2-name').textContent    = data.player2?.displayName || 'Player 2';
  $('p2-score').textContent   = data.player2?.score ?? 0;
  $('round-info').textContent = `Round ${data.currentRound} of ${data.maxRounds}`;

  $('p1-header').classList.toggle('mine', myPlayerKey === 'player1');
  $('p2-header').classList.toggle('mine', myPlayerKey === 'player2');

  const p1Role = roleForRound('player1', data.currentRound);
  const p2Role = roleForRound('player2', data.currentRound);

  $('p1-role-pill').textContent = p1Role.toUpperCase();
  $('p1-role-pill').className   = `role-pill ${p1Role}-pill`;
  $('p2-role-pill').textContent = p2Role.toUpperCase();
  $('p2-role-pill').className   = `role-pill ${p2Role}-pill`;

  $('my-role-badge').textContent = myRole === 'mean' ? 'MEAN' : 'MEDIAN';
  $('my-role-badge').className   = 'role-badge ' + myRole;
}

// ── Teacher controls ──────────────────────────────────────────────────────────
function applyTeacherControls(data) {
  const tc = data.teacherControls || {};
  $('pause-banner').style.display  = tc.paused ? 'flex' : 'none';
  $('lock-indicator').style.display = tc[myPlayerKey + 'InputLocked'] ? 'inline' : 'none';
}

// ── Active round ──────────────────────────────────────────────────────────────
async function renderActive(data) {
  const round = data.round || {};

  // Player 1 (mean role) is responsible for initializing each round
  if (myPlayerKey === 'player1') {
    const needsInit = !round.cards || round.cards.length === 0;
    const freshRound = data.currentRound !== lastInitRound;
    if (needsInit && freshRound && !initializingRound) {
      lastInitRound = data.currentRound;
      initializingRound = true;
      try { await initRound(data); }
      finally { initializingRound = false; }
      return; // snapshot will re-fire with fresh cards
    }
  }

  if (!round.cards || round.cards.length === 0) {
    // Waiting for player1 to init
    showSection('active-section');
    $('cards-container').innerHTML = '<p class="waiting-init">Dealing cards…</p>';
    return;
  }

  showSection('active-section');
  hideChoosing();

  // Reveal state
  const myRevealField = myRole === 'mean' ? 'meanRevealedAt' : 'medianRevealedAt';
  const revealTs = round[myRevealField];
  const revealed = !!revealTs;

  renderCards(round.cards, revealed);

  // Timer
  if (revealed) {
    if (!roundTimerInterval) startTimer(revealTs);
  } else {
    clearTimer();
    $('timer').textContent = '00:00';
  }

  // Input locked?
  const tc         = data.teacherControls || {};
  const paused     = tc.paused;
  const myLocked   = tc[myPlayerKey + 'InputLocked'];
  const alreadyWon = !!round.winner;
  const enabled    = revealed && !paused && !myLocked && !alreadyWon;

  $('answer-input').disabled = !enabled;
  $('submit-btn').disabled   = !enabled;

  // Status message
  if (!revealed) {
    $('status-msg').textContent = myRole === 'median'
      ? 'Waiting for your cards…'
      : 'Cards revealed — calculate the mean!';
  } else if (alreadyWon) {
    $('status-msg').textContent = round.winner === myPlayerKey
      ? 'You answered correctly!'
      : 'Opponent answered first!';
    $('answer-input').disabled = true;
    $('submit-btn').disabled   = true;
  } else {
    $('status-msg').textContent = '';
  }
}

// ── Round initialization (player 1 only) ──────────────────────────────────────
async function initRound(data) {
  clearAllTimers();

  const cards  = Array.from({ length: 4 }, () => Math.floor(Math.random() * 10) + 1);
  const mean   = computeMean(cards);
  const median = computeMedian(cards);
  const buffer = data.teacherControls?.meanBufferSeconds ?? 1;

  await db.collection('games').doc(gameId).update({
    'round.cards':            cards,
    'round.mean':             mean,
    'round.median':           median,
    'round.meanRevealedAt':   firebase.firestore.FieldValue.serverTimestamp(),
    'round.medianRevealedAt': null,
    'round.winner':           null,
    'round.winnerChoice':     null,
    'round.pointsEarned':     null,
    'round.revealed':         false,
  });

  // Reveal median player's cards after buffer
  medianRevealTimer = setTimeout(async () => {
    await db.collection('games').doc(gameId).update({
      'round.medianRevealedAt': firebase.firestore.FieldValue.serverTimestamp(),
    });
  }, buffer * 1000);
}

// ── Answer submission ─────────────────────────────────────────────────────────
async function submitAnswer() {
  const raw    = $('answer-input').value.trim();
  const answer = parseFloat(raw);

  if (isNaN(answer)) {
    showFeedback('Please enter a valid number.', true);
    return;
  }

  const round   = latestData.round;
  const correct = myRole === 'mean'
    ? Math.abs(answer - round.mean) <= 0.01
    : Math.abs(answer - round.median) < 0.001;

  if (!correct) {
    showFeedback('Incorrect — try again.', true);
    return;
  }

  // Race-safe winner claim
  const gameRef = db.collection('games').doc(gameId);
  try {
    await db.runTransaction(async tx => {
      const snap = await tx.get(gameRef);
      if (snap.data().round.winner !== null) throw new Error('already_won');
      tx.update(gameRef, {
        'round.winner': myPlayerKey,
        status:         'choosing',
      });
    });
    showFeedback('', false);
    $('answer-input').disabled = true;
    $('submit-btn').disabled   = true;
  } catch (err) {
    if (err.message === 'already_won') {
      showFeedback('Opponent was faster!', true);
    } else {
      console.error(err);
    }
  }
}

// ── Choosing phase ────────────────────────────────────────────────────────────
function renderChoosing(data) {
  const round = data.round || {};
  clearTimer();
  showSection('active-section');
  renderCards(round.cards || [], true);

  $('answer-input').disabled = true;
  $('submit-btn').disabled   = true;
  showFeedback('', false);

  if (round.winner === myPlayerKey) {
    // I won — show choice buttons
    $('choosing-section').style.display    = '';
    $('opponent-choosing').style.display   = 'none';
    $('btn-take-mean').textContent   = 'Mean';
    $('btn-take-median').textContent = 'Median';
    $('status-msg').textContent      = '';
  } else {
    $('choosing-section').style.display    = 'none';
    $('opponent-choosing').style.display   = '';
    $('status-msg').textContent            = '';
  }
}

async function makeChoice(choice) {
  const round   = latestData.round;
  const points  = choice === 'mean' ? round.mean : round.median;
  const current = latestData[myPlayerKey].score;

  await db.collection('games').doc(gameId).update({
    'round.winnerChoice': choice,
    'round.pointsEarned': points,
    'round.revealed':     true,
    [`${myPlayerKey}.score`]: current + points,
    status: 'round_over',
  });
}

// ── Round over / reveal ───────────────────────────────────────────────────────
function renderRoundOver(data) {
  const round = data.round || {};
  clearAllTimers();
  showSection('reveal-section');
  hideChoosing();
  renderCards(round.cards || [], true);

  $('reveal-mean').textContent   = round.mean ?? '—';
  $('reveal-median').textContent = round.median ?? '—';

  if (round.winner && round.winnerChoice) {
    const winnerName = data[round.winner]?.displayName || round.winner;
    $('reveal-winner').textContent =
      `${winnerName} chose the ${round.winnerChoice} (${round.pointsEarned} pts)`;
  } else {
    $('reveal-winner').textContent = 'Round ended early by teacher.';
  }

  // Only player 1 drives the countdown + advance
  if (myPlayerKey === 'player1') {
    let seconds = 3;
    $('reveal-countdown').textContent = seconds;
    roundAdvanceTimer = setInterval(async () => {
      seconds--;
      $('reveal-countdown').textContent = seconds;
      if (seconds <= 0) {
        clearInterval(roundAdvanceTimer);
        roundAdvanceTimer = null;
        await advanceRound(data);
      }
    }, 1000);
  } else {
    $('reveal-countdown').textContent = '…';
  }
}

async function advanceRound(data) {
  const next = data.currentRound + 1;
  if (next > data.maxRounds) {
    await db.collection('games').doc(gameId).update({ status: 'finished' });
  } else {
    lastInitRound = -1; // allow re-init
    await db.collection('games').doc(gameId).update({
      currentRound: next,
      status:       'active',
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
    });
  }
}

// ── Finished ──────────────────────────────────────────────────────────────────
function renderFinished(data) {
  clearAllTimers();
  showSection('finished-section');

  const p1 = data.player1?.score ?? 0;
  const p2 = data.player2?.score ?? 0;

  $('final-p1-name').textContent  = data.player1?.displayName || 'Player 1';
  $('final-p1-score').textContent = p1;
  $('final-p2-name').textContent  = data.player2?.displayName || 'Player 2';
  $('final-p2-score').textContent = p2;

  let banner;
  if (p1 > p2)      banner = `🏆 ${data.player1?.displayName || 'Player 1'} wins!`;
  else if (p2 > p1) banner = `🏆 ${data.player2?.displayName || 'Player 2'} wins!`;
  else              banner = "It's a tie!";
  $('game-winner').textContent = banner;

  $('play-again-btn').style.display = myPlayerKey === 'player1' ? '' : 'none';
}

async function playAgain() {
  lastInitRound = -1;
  await db.collection('games').doc(gameId).update({
    status:          'active',
    currentRound:    1,
    'player1.score': 0,
    'player2.score': 0,
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
  });
}

// ── Cards ─────────────────────────────────────────────────────────────────────
function renderCards(cards, revealed) {
  const container = $('cards-container');
  container.innerHTML = '';

  if (!cards || cards.length === 0) {
    container.innerHTML = '<p class="waiting-init">Dealing cards…</p>';
    return;
  }

  cards.forEach((num, i) => {
    const card = document.createElement('div');
    card.className = 'card' + (revealed ? ' revealed' : '');
    if (revealed) card.style.animationDelay = `${i * 0.12}s`;
    card.innerHTML = `
      <div class="card-inner">
        <div class="card-front">?</div>
        <div class="card-back">${num}</div>
      </div>`;
    container.appendChild(card);
  });
}

// ── Timer ─────────────────────────────────────────────────────────────────────
function startTimer(ts) {
  clearTimer();
  const startMs = ts?.toMillis?.() || Date.now();
  roundTimerInterval = setInterval(() => {
    const elapsed = Math.max(0, Math.floor((Date.now() - startMs) / 1000));
    const m = Math.floor(elapsed / 60).toString().padStart(2, '0');
    const s = (elapsed % 60).toString().padStart(2, '0');
    $('timer').textContent = `${m}:${s}`;
  }, 500);
}

function clearTimer() {
  if (roundTimerInterval) { clearInterval(roundTimerInterval); roundTimerInterval = null; }
}

function clearAllTimers() {
  clearTimer();
  if (medianRevealTimer)  { clearTimeout(medianRevealTimer);  medianRevealTimer  = null; }
  if (roundAdvanceTimer)  { clearInterval(roundAdvanceTimer); roundAdvanceTimer  = null; }
}

// ── Math helpers ──────────────────────────────────────────────────────────────
function computeMean(cards) {
  return Math.round((cards.reduce((a, b) => a + b, 0) / cards.length) * 100) / 100;
}

function computeMedian(cards) {
  const s = [...cards].sort((a, b) => a - b);
  return (s[1] + s[2]) / 2; // always 4 cards
}

// ── UI helpers ─────────────────────────────────────────────────────────────────
const SECTIONS = ['active-section', 'reveal-section', 'finished-section'];

function showSection(id) {
  SECTIONS.forEach(s => {
    const el = $(s);
    if (el) el.style.display = s === id ? '' : 'none';
  });
}

function hideChoosing() {
  $('choosing-section').style.display  = 'none';
  $('opponent-choosing').style.display = 'none';
}

function showFeedback(msg, isError) {
  $('answer-feedback').textContent = msg;
  $('answer-feedback').className   = 'feedback' + (isError ? ' error' : '');
}
