#!/bin/sh
#
# GNU Guix Reproducible Development Environment

WD=$(pwd) # WD=$(dirname "$0") # i.e. path to this file

# --container   run command within an isolated container
# --network     allow containers to access the network
# --no-cwd      don't share current working directory with an isolated container
# --preserve    preserve environment variables that match the REGEXP
# --expose      expose read-only host file system according to SPEC
# The --preserve and --expose parameters are needed so that `raco test laundry`
# doesn't complain:
#  raco test: (submod "~/.local/share/racket/8.3/pkgs/laundry/colorer.rkt" test)
#  Unable to init server: Could not connect: Connection refused
#  Gtk initialization failed for display ":0"
#  ...
#  raco test: "~/.local/share/racket/8.3/pkgs/laundry/perf.rkt"
#  Unable to init server: Could not connect: Connection refused
#  Gtk initialization failed for display ":0"

# Full freeze of Guix channels:
# guix time-machine --channels=./channels-lock.scm -- shell \
#      --container --network REST-OF-ARGS-OF-GUIX-SHELL

# xtrace: Print commands and their arguments as they are executed
set -x

guix shell \
     --container --network --no-cwd \
     racket bash grep coreutils which openssl nss-certs \
     --preserve='^DISPLAY$' --preserve='^XAUTHORITY$' --expose=$XAUTHORITY \
     --share=$WD/guix-rde/.bash_profile=$HOME/.bash_profile \
     --share=$WD/guix-rde/.bashrc=$HOME/.bashrc \
     --share=$WD/guix-rde/etc=/usr/etc \
     --share=/etc/ssl/certs \
     --share=$WD \
     -- bash
