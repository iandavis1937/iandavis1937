# iandavis1937

Personal portfolio website built with [Quarto](https://quarto.org/).

---

## Secrets Management

No secrets are committed to this repository. The table below lists each secret, where it lives on the server, and which file uses it.

| Secret | Server location | Used by |
|---|---|---|
| GitHub webhook secret | `/home/iandavis1937/private/config.php` | `deploy.php` |
| TomTom API key (backend) | `backend/.env` | `backend/fetch_traffic_data.py` |
| TomTom API key (frontend) | `traffic-tutorial-web-sdk/config.js` | `traffic-tutorial-web-sdk/traffic.js` |
| HERE Maps credentials | `.here/credentials.properties` | `.here/here_py_test.py` |

### Setup

Each secret has a corresponding example file in the repo. Copy it to the location shown and fill in the real value.

| Example file | Copy to |
|---|---|
| `private/config.example.php` | `/home/iandavis1937/private/config.php` (outside `public_html`) |
| `backend/.env.example` | `backend/.env` |
| `traffic-tutorial-web-sdk/config.example.js` | `traffic-tutorial-web-sdk/config.js` |

### Notes

- `private/config.php` lives **outside `public_html`** and is therefore not web-accessible.
- The TomTom frontend key is visible in browser devtools by nature of client-side JS. Mitigate this by restricting the key to your domain via HTTP referer in the [TomTom Developer Portal](https://developer.tomtom.com/).
- `credentials.properties` and all `.env` files are covered by `.gitignore`.
- `private/` is covered by `.gitignore` so the directory cannot be accidentally committed.
- `.cpanel.yml` wipes `public_html/_site/` on deployment, leaving `private/`, `backend`, and `traffic-tutorial-...` with stable files for secrets

---

## Building

```bash
quarto render
```

Requires the `personalwebenv` conda environment. See `.here/here_maps_setup` for HERE SDK setup instructions.
