## Spacemacs Configuration Layout

```
~/.spacemacs.d/
├── init.el       # Main config (layers, packages, settings)
└── config/       # Modular config files, all loaded by user-config

~/.emacs.d/                    # Spacemacs runtime (DO NOT edit, unless explicitly requested)
├── core/                      # Spacemacs core framework
├── layers/                    # Built-in Spacemacs layer definitions
├── elpa/                      # installed ELPA packages
└── quelpa/              # quelpa recipes (for GitHub-sourced packages)
```

**Key locations in `init.el`:**
- `dotspacemacs-configuration-layers` — Active Spacemacs layers
- `dotspacemacs-additional-packages` — Extra packages (GitHub recipes supported)
- `dotspacemacs/user-config` — Loads all files from `config/` directory

### Adding a new package

1. Add to `dotspacemacs-additional-packages` in `init.el` (use `:location (recipe ...)` for GitHub packages)
2. Create configuration in `config/*.el`
3. Add the filename to the `dolist` in `dotspacemacs/user-config`
4. Load the new config file: `emacsclient --eval '(load ...)'`

### Running tests

Tests are [ERT](https://www.gnu.org/software/emacs/manual/html_node/ert/) and run headless — no live Emacs needed:

- `./run-tests.sh` — run everything
- `./run-tests.sh "gtx-.*"` — run a subset (arg is an ERT selector regexp); prefixes: `gtl-` (ghostel-toggle library), `gta-` (agents), `gtt-` (terminals), `gtx-` (cross-kind)

Tests live in `config/<name>-tests.el` and are **not** added to the `user-config` `dolist` (so they never load at startup). `run-tests.sh` loads the modules under test plus the suite; add `-l config/…` lines there when covering a new module.
