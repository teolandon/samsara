# Git Hooks
The hooks/ directory contains the recommended git hooks for this project, as
well as a bash script that generates the appropriate symlinks in .git/hooks/.
To install the hooks simply run `./install-hooks.sh` in this directory.
Alternatively, you can copy all hooks to the `.git/hooks/` directory.

## Pre-commit

Runs `make test` and stops commit if it exits with a non-zero code (meaning some
test failed).
