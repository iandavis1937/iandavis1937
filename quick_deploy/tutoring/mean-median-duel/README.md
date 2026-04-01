# Mean & Median Duel

A real-time, 2-player browser game built on Firebase. Two students race to calculate the mean or median of a drawn card set. A teacher role provides live override controls.

---

## Game Rules

- **Players:** 2, matched by a shared Game ID
- **Roles:** Fixed — Player 1 calculates the **mean**; Player 2 calculates the **median**
- **Cards per round:** 4 integers drawn randomly from 1–10 (with replacement)
- **Head-start:** The mean player sees their cards `meanBufferSeconds` seconds before the median player (default: 1 second, adjustable by the teacher)
- **Winning a round:** First player to submit the correct answer
  - Mean answers accepted within ±0.01 of the true value
  - Median must be exact (always a multiple of 0.5 for 4-card draws)
- **Reward choice:** The round winner privately sees both the mean and median and chooses one as their point total
- **Reveal:** Both players see the full values only after the winner has chosen
- **Game length:** First to accumulate the highest score after `maxRounds` (default: 5) rounds wins

---

## Project Structure

```
mean-median-duel/
├── public/
│   ├── index.html          # Lobby & matchmaking (students)
│   ├── game.html           # Active game screen (students)
│   ├── teacher.html        # Teacher login & dashboard
│   ├── style.css           # Global styles (shared by all pages)
│   └── js/
│       ├── firebase-init.js  # Firebase app init; exports db, auth
│       ├── lobby.js          # Lobby & matchmaking logic
│       ├── game.js           # Round logic, timer, submission
│       └── teacher.js        # Teacher dashboard logic & controls
├── firestore.rules         # Field-level security rules
├── firestore.indexes.json  # Firestore composite indexes (empty)
├── firebase.json           # Hosting config (public dir: public/)
├── .env.example            # Template for Firebase config vars
└── README.md
```

---

## Prerequisites

- [Node.js](https://nodejs.org/) (for `firebase-tools`)
- Firebase CLI: `npm install -g firebase-tools`
- A Firebase project with **Firestore** and **Authentication** (Anonymous sign-in) enabled

---

## Local Development

```bash
# 1. Authenticate with Firebase
firebase login

# 2. Link to your Firebase project
firebase use --add

# 3. Start the local dev server
firebase serve
```

Open `http://localhost:5000` in your browser.

> **Why `firebase serve`?** The app loads its Firebase config automatically via the reserved URL `/__/firebase/init.js`, which is only available when served through Firebase Hosting (local or deployed). Opening the HTML files directly in a browser will not work.

---

## Deployment

```bash
firebase deploy
```

This deploys both Firestore security rules and the Hosting files.

---

## Teacher Access

The teacher dashboard is protected by a passcode. The default passcode is:

```
classroom1
```

To change it, edit the `TEACHER_PASSCODE` constant at the top of `public/js/teacher.js` and update this README.

---

## Teacher Dashboard Usage

1. Navigate to `/teacher.html` (a small, unobtrusive link appears at the bottom of the lobby page).
2. Enter the teacher passcode (default: `classroom1`).
3. Enter the active **Game ID** (shared by Player 1) and click **Observe**. The dashboard populates in real time.

### Controls

| Control | Effect |
|---|---|
| **Pause / Resume** | Disables all player inputs and shows a full-screen pause banner. Auto-resumes when toggled off. |
| **Lock Player 1 / 2** | Disables a specific player's input independently of the global pause. |
| **Mean buffer (Apply)** | Sets how many seconds Player 1 (mean) sees cards before Player 2 (median). Takes effect at the **start of the next round**. |
| **Force Next Round** | Immediately ends the current round (even if no one answered) and starts the next. |
| **End Game** | Immediately sets the game to "finished" and shows final scores. |

### Mean Buffer

- Default: **1 second**
- Accepts any non-negative number, including decimals (e.g. `0`, `0.5`, `2`, `10`)
- Changes take effect at the **start of the next round** — the current round always uses the buffer value that was active when that round began
- The "Currently active" readout beneath the input reflects the live Firestore value

---

## Changing `maxRounds`

`maxRounds` is written to the Firestore game document when a new game is created (default: `5`). To change it, either:

- Edit the default in `public/js/lobby.js` (`maxRounds: 5` in the `createGame` function) before deployment, or
- Update the `maxRounds` field directly on an existing game document in the Firestore console.

---

## Known Limitations

### Firestore field-hiding constraint

Firestore security rules cannot hide individual fields within a document. As a result, `round.mean` and `round.median` are technically readable by any authenticated game participant at any time — even before `round.revealed` is set to `true`. A motivated player could inspect the raw Firestore payload and see the values before the reveal phase.

**In a production deployment**, these values should be:
- Stored in a **subcollection** with more restrictive rules, or
- Computed and written to the main document **only after** `revealed = true` via a **Cloud Function**

For this prototype (classroom use under teacher supervision), the limitation is acceptable and is documented here.

### Anonymous Auth persistence

Players use Firebase Anonymous Authentication. Auth state persists across page refreshes (using `LOCAL` persistence) but is tied to the browser. If a player clears browser storage or uses a different device, they will receive a new anonymous UID and cannot rejoin an in-progress game.

---

## Firebase Config Variables

See `.env.example` for the required variables. These are injected automatically when using Firebase Hosting — they are never hard-coded in the source files.
