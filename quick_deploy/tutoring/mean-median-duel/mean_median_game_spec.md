# Mean & Median Duel — Firebase 2-Player Game
## Requirements Specification for Claude Code

---

## Overview

Build a real-time, 2-player browser game hosted on Firebase. Each round, both players see a set of 4 randomly drawn cards (numbers 1–10, drawn with replacement). One player is assigned to calculate the **mean**; the other is assigned to calculate the **median**. The player who answers correctly *first* wins the round and chooses their point reward (either the mean or the median of that round's card set). The player who reaches a target score first wins the game.

A **teacher role** exists alongside the two students. The teacher logs in with a dedicated password-protected account, has a separate dashboard view, and holds real-time override controls over all aspects of gameplay: they can pause/resume the game, force-advance rounds, lock or unlock individual player inputs, and adjust the head-start buffer (in seconds) given to the mean-calculating student.

---

## Tools to Load

```
- firebase (Firestore, Authentication [Email/Password + Anonymous], Hosting)
- Node.js / npm (local dev and deployment)
```

Claude Code should initialize a Firebase project (or connect to an existing one using environment variables for project config), enable **Firestore** for real-time game state sync, and configure **Firebase Hosting** for deployment.

> **Do not install any packages beyond those listed in each subagent's section without confirming with the user first.**

---

## Repository Structure

```
mean-median-duel/
├── public/
│   ├── index.html          # Entry point / lobby (students)
│   ├── teacher.html        # Teacher login & dashboard
│   ├── game.html           # Game screen (students)
│   ├── style.css           # Global styles
│   └── js/
│       ├── lobby.js        # Lobby & matchmaking logic
│       ├── game.js         # Round logic, timer, submission
│       ├── teacher.js      # Teacher dashboard logic & controls
│       └── firebase-init.js # Firebase config & initialization
├── firestore.rules         # Firestore security rules
├── firebase.json           # Firebase Hosting config
├── .env.example            # Template for Firebase config vars
└── README.md
```

---

## Subagents & Task Assignments

---

### Subagent 1 — Firebase Setup & Data Modeling

**Packages:** `firebase-tools` (CLI, global install)

**Tasks:**

1. Scaffold the Firebase project structure (`firebase init`), enabling Firestore and Hosting.
2. Write `firebase-init.js` to initialize the Firebase app using environment variables (never hard-coded keys). Export `db` (Firestore instance).
3. Design and document the Firestore data model:

**Collection: `games/`**

```
games/{gameId}
  ├── player1: { uid, displayName, role: "mean" | "median", score: 0, ready: bool }
  ├── player2: { uid, displayName, role: "mean" | "median", score: 0, ready: bool }
  ├── status: "waiting" | "active" | "paused" | "round_over" | "choosing" | "finished"
  ├── currentRound: number
  ├── maxRounds: number (default: 5)
  ├── teacherControls: {
  │     paused: bool,                        // if true, all player inputs are locked
  │     player1InputLocked: bool,            // teacher can lock individual players
  │     player2InputLocked: bool,
  │     meanBufferSeconds: number            // head-start delay for mean player (default: 1)
  │   }
  ├── round: {
  │     cards: [n1, n2, n3, n4],
  │     mean: number,
  │     median: number,
  │     meanRevealedAt: timestamp | null,
  │     medianRevealedAt: timestamp | null,
  │     winner: "player1" | "player2" | null,
  │     winnerChoice: "mean" | "median" | null,
  │     pointsEarned: number | null,
  │     revealed: bool
  │   }
  └── createdAt: timestamp
```

**Collection: `teachers/`**

```
teachers/{uid}
  └── email: string    // used to identify teacher accounts in Firestore rules
```

Teachers are provisioned by the developer: create a Firebase Auth Email/Password account for the teacher, then write a document to `teachers/{uid}` in Firestore. No self-registration UI is needed.

4. Write `firestore.rules` to enforce:
   - Only authenticated users may read/write their own game documents.
   - `round.mean` and `round.median` fields are **write-once** (set by a server-side function or the game host on round start) and are **not readable** by clients until `round.revealed === true`.
   - The `teacherControls` map and all fields within it are **writable only by authenticated users whose UID exists as a document in the `teachers/` collection**. Students may read `teacherControls` (so their UI can respond to pauses and locks) but never write to it.
   - The `teachers/` collection is readable and writable only by users whose UID matches the document ID (i.e., the teacher themselves), or by a pre-seeded admin. No student can enumerate or read this collection.

> **Security note:** Because Firestore rules cannot fully hide fields from clients who know how to read raw Firestore payloads, the spec should note that in a production setting, `mean` and `median` should be stored in a subcollection or computed via a Cloud Function and only written to the main document after `revealed` is set to `true`. For this prototype, instruct Claude Code to clearly document this limitation.

---

### Subagent 2 — Lobby & Matchmaking

**Packages:** None beyond Firebase SDK (loaded via CDN in HTML)

**Tasks:**

1. Build `index.html` and `lobby.js`:
   - On load, prompt for a display name (no full auth required; use Firebase Anonymous Auth).
   - **Create Game** button: writes a new `games/{gameId}` document with `status: "waiting"` and assigns the creator as `player1` with `role: "mean"`, and initializes `teacherControls: { paused: false, player1InputLocked: false, player2InputLocked: false, meanBufferSeconds: 1 }`.
   - **Join Game** button: accepts a Game ID input, validates the game exists and has `status: "waiting"`, writes the joiner as `player2` with `role: "median"`, and sets `status: "active"`.
   - Once both players are present, both clients are redirected to `game.html?gameId={gameId}`.
   - Display the Game ID prominently so Player 1 can share it with Player 2.
   - Include a small, unobtrusive **"Teacher Login →"** link at the bottom of the lobby page that navigates to `teacher.html`. Do not style this as a primary action.
2. Roles are **fixed** for the entire game session: Player 1 always calculates mean; Player 2 always calculates median.
3. Display a "Waiting for opponent…" spinner until Player 2 joins.

---

### Subagent 3 — Core Game Logic

**Packages:** None beyond Firebase SDK

**Tasks:**

1. Build `game.js` with a Firestore real-time listener (`onSnapshot`) on the active game document.

2. **Round initialization** (triggered when `status` becomes `"active"` or advances to the next round):
   - Draw 4 integers from 1–10 with replacement using `Math.random()`.
   - Compute and store `mean` (rounded to 2 decimal places) and `median`.
   - Write these plus `cards`, `meanRevealedAt: null`, `medianRevealedAt: null`, `winner: null`, `revealed: false` to Firestore.
   - Immediately set `meanRevealedAt` to `serverTimestamp()`.
   - After a delay of `teacherControls.meanBufferSeconds` seconds (read from the game document), set `medianRevealedAt` to `serverTimestamp()`. This delay must be re-read from Firestore at the start of each round — it is not cached from the previous round.

3. **Card reveal timing:**
   - The mean-player's UI shows the cards as soon as `meanRevealedAt` is set.
   - The median-player's UI shows the cards as soon as `medianRevealedAt` is set.
   - Use the Firestore `onSnapshot` listener to watch for these fields and render accordingly.

4. **Teacher control enforcement** (checked continuously via `onSnapshot`):
   - If `teacherControls.paused === true`: disable all player inputs and display a **"⏸ Game paused by teacher"** banner across the full game area. Re-enable automatically when `paused` returns to `false`.
   - If `teacherControls.player1InputLocked === true`: disable Player 1's answer input and submit button, showing a small lock icon. Same logic for `player2InputLocked`.
   - Individual locks are independent of the global pause — a player can be locked while the game is otherwise running.
   - Locking a player who has already submitted a correct answer in the current round has no effect on the already-recorded winner.

5. **Answer submission:**
   - Each player sees a numeric input and a Submit button, enabled only after their reveal timestamp is set AND they are not locked AND the game is not paused.
   - On submission, validate: is the answer correct (within `±0.01` tolerance for mean; exact for median)?
   - If correct and `round.winner` is still `null`, write `winner: thisPlayer` to Firestore.
   - If incorrect, display "Incorrect — try again" and allow resubmission.
   - If the other player already has `winner` set, lock out further submissions.

6. **Post-round choice phase** (`status: "choosing"`):
   - Only the round winner sees two buttons: **"Take the Mean: X"** and **"Take the Median: Y"** — where X and Y are the actual values (revealed to the winner only at this moment).
   - The non-winning player sees: *"Opponent is choosing their reward…"*
   - On choice, write `winnerChoice`, `pointsEarned`, and `revealed: true` to Firestore, and increment the winner's score.

7. **Reveal phase** (after `revealed === true`):
   - Both players see the full card set, the mean, and the median.
   - Display the round result: who won, what they chose, and how many points they earned.
   - After a 3-second countdown, advance to the next round (or end the game).

8. **Game end:**
   - After `maxRounds` rounds, display final scores and declare the winner.
   - Offer a "Play Again" button that resets the game document (same players, same roles).

---

### Subagent 4 — Teacher Dashboard

**Packages:** None beyond Firebase SDK

**Tasks:**

1. Build `teacher.html` and `teacher.js`:

   **Login screen:**
   - Simple centered form: email + password fields and a "Log in as Teacher" button.
   - Use Firebase Auth `signInWithEmailAndPassword`.
   - After login, verify the user's UID exists in the `teachers/` Firestore collection. If not, sign them out immediately and show "Unauthorized account."
   - On success, show the dashboard (same page, toggle visibility).

   **Dashboard layout:**
   - A **Game ID input** at the top: teacher types the active Game ID and clicks "Observe" to attach a Firestore `onSnapshot` listener to that game document. The dashboard populates in real time once connected.
   - A **read-only game status bar** showing: current round, score for each player, and game status string.
   - A **live card display**: shows the current round's cards, mean, and median — always visible to the teacher regardless of `round.revealed`. (Teacher is never subject to the reveal gate.)

2. **Control panel** — all controls write to `games/{gameId}/teacherControls` in Firestore:

   | Control | UI element | Firestore field written |
   |---|---|---|
   | Pause / Resume game | Toggle button (shows current state) | `teacherControls.paused` |
   | Lock Player 1 input | Toggle button labeled with Player 1's display name | `teacherControls.player1InputLocked` |
   | Lock Player 2 input | Toggle button labeled with Player 2's display name | `teacherControls.player2InputLocked` |
   | Mean buffer (seconds) | Numeric text input + "Apply" button | `teacherControls.meanBufferSeconds` |
   | Force next round | Button (disabled unless status is `"active"` or `"round_over"`) | Sets `status: "round_over"` and triggers next round via the game logic |
   | End game | Button with confirmation dialog | Sets `status: "finished"` |

3. **Mean buffer input** — special handling:
   - Render as a labeled text box: `Head-start for Mean player (seconds): [____] [Apply]`.
   - Validate on Apply: must be a non-negative number (decimals allowed, e.g. `0.5`, `2`). Show an inline error if invalid.
   - Write the validated value to `teacherControls.meanBufferSeconds` in Firestore.
   - The new value takes effect at the **start of the next round** (Subagent 3 reads this field fresh each round).
   - Display the currently active buffer value beneath the input as: *"Currently active: X seconds"*, updated in real time from the snapshot.

4. **Logout button** — calls `signOut()` and returns to the login screen.

5. The teacher dashboard must **never** be accessible to unauthenticated users or to users not in the `teachers/` collection. Redirect to the login screen if the auth state is missing or unauthorized.

---

### Subagent 5 — UI & Styling

**Packages:** None (vanilla CSS; no frameworks)

**Design Direction — "Classroom Arcade":**

Use a playful but legible aesthetic suitable for students. Think bold card visuals, clear typography, and satisfying micro-animations — energetic without being chaotic.

- **Typography:** Use [Fredoka One](https://fonts.google.com/specimen/Fredoka+One) (display/headings) + [Nunito](https://fonts.google.com/specimen/Nunito) (body). Load via Google Fonts CDN.
- **Color palette:**
  - Background: `#1a1a2e` (deep navy)
  - Card surface: `#f5f0e8` (warm off-white)
  - Mean accent: `#f7b731` (golden yellow)
  - Median accent: `#26de81` (mint green)
  - Danger/incorrect: `#fc5c65` (coral red)
  - Text on dark: `#e8e8f0`
- **Cards:** Render each number as a large playing-card-style tile (CSS `box-shadow`, rounded corners, bold centered numeral). Cards should flip in with a CSS 3D flip animation on reveal.
- **Role badge:** Each player's assigned role (MEAN / MEDIAN) should be displayed as a prominent pill badge in their accent color at all times.
- **Score display:** Show both players' scores as large numerals in the header, updating in real time via Firestore listener.
- **Countdown timer:** Display a visible per-round stopwatch (counting up from 0) that starts when the player's cards are revealed.
- **Waiting state:** Animate a pulsing ellipsis ("Opponent is choosing…") during the choosing phase.
- **Responsive layout:** Must be usable on tablets (min-width 768px). Two-column layout for scores at top; card grid centered; input + submit below.

**Specific UI states to implement (student game screen):**

| State | Mean Player sees | Median Player sees |
|---|---|---|
| Round start (0 – buffer) | Cards revealed, input enabled | Cards hidden, input locked |
| Round start (buffer elapsed) | Cards revealed, input enabled | Cards revealed, input enabled |
| Correct answer submitted first | "You won! Choose your reward." + choice buttons showing actual values | "Opponent choosing…" |
| Correct answer submitted second | "Opponent won this round." | "Opponent choosing…" (or "You won!" if median player was faster) |
| Reveal phase | Full mean + median shown | Full mean + median shown |
| Game over | Final scores + winner banner | Final scores + winner banner |
| Teacher paused | All inputs disabled; full-width **"⏸ Game paused by teacher"** banner | Same |
| Teacher locked (this player) | Input and submit disabled; small 🔒 icon on input | Same |

**Teacher dashboard styling:**
- Use the same global CSS file but give `teacher.html` a distinctly different feel: a clean, functional **"control room"** aesthetic with a light background (`#f7f8fc`), dark text, and the same accent colors repurposed as status indicators.
- Toggle buttons for pause and lock states should visually reflect their active state (e.g., red tint when paused/locked, green when active/unlocked).
- The mean buffer text input should be clearly labeled and visually grouped with its Apply button and the "Currently active" readout beneath it.
- The teacher's live card display should show all four cards plus the mean and median values at all times, styled similarly to the student card tiles but smaller.

---

### Subagent 6 — Deployment & README

**Packages:** `firebase-tools` (already installed by Subagent 1)

**Tasks:**

1. Configure `firebase.json` for Hosting (public directory: `public/`, rewrites to `index.html`).
2. Write `.env.example` documenting required Firebase config variables:
   ```
   FIREBASE_API_KEY=
   FIREBASE_AUTH_DOMAIN=
   FIREBASE_PROJECT_ID=
   FIREBASE_STORAGE_BUCKET=
   FIREBASE_MESSAGING_SENDER_ID=
   FIREBASE_APP_ID=
   ```
3. Write `README.md` covering:
   - Project overview and game rules
   - Local development setup (`firebase serve`)
   - Deployment steps (`firebase deploy`)
   - **Teacher account provisioning:** explain that a teacher account must be created manually — (1) create an Email/Password user in the Firebase Auth console, (2) copy the UID, (3) create a document at `teachers/{uid}` in Firestore with `{ email: "teacher@example.com" }`. No self-registration is possible by design.
   - **Teacher dashboard usage:** how to navigate to `teacher.html`, log in, enter a Game ID to observe, and use each control
   - **Mean buffer:** what it controls, how to change it mid-session, and that changes take effect at the next round
   - Known limitations (Firestore field-hiding constraint noted by Subagent 1)
   - How to change `maxRounds` (currently a Firestore field defaulting to 5)

---

## Game Rules Summary (for reference in all subagents)

- **Players:** 2, matched by shared Game ID
- **Roles:** Fixed — Player 1 calculates mean; Player 2 calculates median
- **Cards per round:** 4 integers drawn from [1, 10] with replacement
- **Reveal delay:** Mean player sees cards `teacherControls.meanBufferSeconds` seconds before the median player (default: 1 second; adjustable by teacher between rounds)
- **Winning a round:** First player to submit the correct answer
- **Reward choice:** Winner privately sees both the mean and median values and chooses one as their point total for the round
- **Reveal:** Both players see mean and median only after the winner has chosen
- **Scoring:** Points accumulate across rounds; highest score after `maxRounds` wins
- **Mean tolerance:** Accept answers within ±0.01 of the true mean
- **Median:** Must be exact (medians of 4 numbers drawn from integers will always be a multiple of 0.5)

---

## Execution Order for Claude Code

1. **Subagent 1** — Firebase setup and data model (blocks all others)
2. **Subagents 2 & 5** — Lobby/matchmaking and styling (can run in parallel after Subagent 1)
3. **Subagents 3 & 4** — Core game logic and teacher dashboard (can run in parallel; both require Subagent 1's data model and Subagent 5's HTML structure)
4. **Subagent 6** — Deployment and docs (runs last, after all code is complete)
