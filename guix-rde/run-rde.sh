#!/bin/sh
#
# GNU Guix Reproducible Development Environment

WD=$(pwd) # WD=$(dirname "$0") # i.e. path to this file

# --container   run command within an isolated container
# --network     allow containers to access the network
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

## Full freeze of Guix channels:
# cat << EOF >> /tmp/channels-lock.scm
# (list (channel
#        (name 'guix)
#         (url "https://git.savannah.gnu.org/git/guix.git")
#         (branch "master")
#         (commit
#           "67817299808a03e2750cfb630dc09fe8eb99c468")
#         (introduction
#           (make-channel-introduction
#             "9edb3f66fd807b096b48283debdcddccfea34bad"
#             (openpgp-fingerprint
#               "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
# EOF
# guix time-machine --channels=/tmp/channels-lock.scm -- shell \
#      --container --network REST-OF-ARGS-OF-GUIX-SHELL

# xtrace: Print commands and their arguments as they are executed
set -x

guix shell \
     --container --network \
     racket bash grep coreutils which openssl nss-certs \
     --preserve='^DISPLAY$' --preserve='^XAUTHORITY$' --expose=$XAUTHORITY \
     --share=$WD/guix-rde/.bash_profile=$HOME/.bash_profile \
     --share=$WD/guix-rde/.bashrc=$HOME/.bashrc \
     --share=/etc/ssl/certs \
     --share=$WD \
     -- bash
