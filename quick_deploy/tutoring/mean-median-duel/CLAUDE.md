# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

---

## Project Overview

**Mean & Median Duel** — a real-time, 2-player browser game built on Firebase. Two students compete to calculate the mean or median of a drawn card set. A teacher role provides live override controls. The spec is in `mean_median_game_spec.md`.

---

## Development Commands

```bash
# Local development server
firebase serve

# Deploy to Firebase Hosting
firebase deploy

# Initialize Firebase project (first-time setup)
firebase init
```

No package.json exists — Firebase SDK is loaded via CDN in HTML files. Only `firebase-tools` (global CLI) is required locally.

---

## Architecture

**Vanilla JS, no frameworks.** Three distinct user roles with separate HTML entry points:

| File | Role |
|---|---|
| `public/index.html` + `public/js/lobby.js` | Student lobby & matchmaking |
| `public/game.html` + `public/js/game.js` | Active game screen |
| `public/teacher.html` + `public/js/teacher.js` | Teacher login & dashboard |
| `public/js/firebase-init.js` | Firebase app init; exports `db` |
| `public/style.css` | Global styles (shared by all pages) |
| `firestore.rules` | Field-level security rules |
| `firebase.json` | Hosting config (public dir: `public/`) |

**Core pattern:** All game state lives in Firestore. UIs subscribe via `onSnapshot` listeners — there is no local game state beyond what Firestore provides.

---

## Firestore Data Model

**`games/{gameId}`** — the single source of truth for a game session:
- `player1` / `player2`: `{ uid, displayName, role: "mean"|"median", score, ready }`
- `status`: `"waiting" | "active" | "paused" | "round_over" | "choosing" | "finished"`
- `teacherControls`: `{ paused, player1InputLocked, player2InputLocked, meanBufferSeconds }`
- `round`: `{ cards[], mean, median, meanRevealedAt, medianRevealedAt, winner, winnerChoice, pointsEarned, revealed }`

**`teachers/{uid}`** — existence here grants teacher privileges (checked by Firestore rules and by `teacher.js` after login).

---

## Key Game Mechanics

- **Roles are fixed** for the session: Player 1 = mean, Player 2 = median.
- **Staggered reveal:** mean player sees cards first; median player waits `teacherControls.meanBufferSeconds` seconds. This value is re-read from Firestore fresh at each round start — never cached.
- **Answer tolerance:** mean answers accepted within ±0.01; median must be exact.
- **Round winner** is the first to write `round.winner` when it is still `null`.
- **Choosing phase:** winner privately sees both values and picks one as their score; `revealed` is set to `true` only after the choice is written.

---

## Security Rules

- `round.mean` and `round.median` are **not readable** by clients until `round.revealed === true`.
- `teacherControls` fields are **writable only** by users whose UID exists in `teachers/`. Students may read but not write.
- **Known prototype limitation:** Firestore rules cannot fully hide fields from clients reading raw payloads. In production, `mean`/`median` should be stored in a subcollection or computed via Cloud Function and only written to the main document after `revealed === true`.

---

## Teacher Account Provisioning

There is no self-registration. To create a teacher account:
1. Create an Email/Password user in the Firebase Auth console.
2. Copy the UID.
3. Write `{ email: "..." }` to `teachers/{uid}` in Firestore.

---

## Environment Variables

Firebase config is never hard-coded. Use `.env.example` as a template:
```
FIREBASE_API_KEY=
FIREBASE_AUTH_DOMAIN=
FIREBASE_PROJECT_ID=
FIREBASE_STORAGE_BUCKET=
FIREBASE_MESSAGING_SENDER_ID=
FIREBASE_APP_ID=
```

---

## UI Design System

"Classroom Arcade" aesthetic — see `mean_median_game_spec.md` § Subagent 5 for full details.

- **Fonts:** Fredoka One (headings) + Nunito (body) via Google Fonts CDN
- **Colors:** background `#1a1a2e`, mean accent `#f7b731`, median accent `#26de81`, danger `#fc5c65`
- **Cards:** playing-card tiles with CSS 3D flip animation on reveal
- **Teacher dashboard:** same CSS file, light background (`#f7f8fc`), "control room" aesthetic

---

## Build Order

When implementing from scratch, follow the dependency order in the spec:
1. Firebase setup & data model (blocks everything)
2. Lobby/matchmaking + styling (parallel)
3. Core game logic + teacher dashboard (parallel; need data model + HTML structure)
4. Deployment + README (last)
