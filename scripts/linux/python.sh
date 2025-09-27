#!/bin/bash

ENV_DIRECTORY="$HOME/.config/emacs/pkgmanager/python"

if [ -d "$ENV_DIRECTORY" ]; then
  "$ENV_DIRECTORY/bin/pip" install --upgrade "python-lsp-server[all]" mypy
else
  python3 -m venv "$ENV_DIRECTORY"
  source "$ENV_DIRECTORY/bin/activate"
  pip install --upgrade pip
  pip install "python-lsp-server[all]" mypy
  deactivate
fi
