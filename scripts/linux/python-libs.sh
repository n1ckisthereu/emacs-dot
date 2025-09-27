#!/bin/bash

ENV_DIRECTORY="$HOME/.config/emacs/pkgmanager/python-libs"

if [ -d "$ENV_DIRECTORY" ]; then
  "$ENV_DIRECTORY/bin/pip" install --upgrade pandas
else
  python3 -m venv "$ENV_DIRECTORY"
  source "$ENV_DIRECTORY/bin/activate"
  pip install --upgrade pip
  pip install pandas
  deactivate
fi
