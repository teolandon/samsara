# Git Hooks
The hooks/ directory contains the recommended git hooks for this project, as
well as a bash script that generates the appropriate symlinks in the .git/hooks/
directory. To install the hooks simply run `./install-hooks.sh` in this folder.

## Pre-commit

Runs `make test` and stops commit if it exits with a non-zero code (meaning some
test failed).
