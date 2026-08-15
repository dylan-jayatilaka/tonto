# Proves the RGBI install list is COMPLETE, by starting from a machine that has
# nothing.
#
#     docker build -f docker/rgbi.Dockerfile -t tonto-rgbi .
#
# A developer machine is a poor witness: years of accumulated packages mean an
# install list derived from it cannot be shown to be sufficient. This starts
# from a bare ubuntu:24.04, installs exactly what docs/INSTALLING_RGBI.md tells
# a workshop participant to install, and then draws the pictures. If the list
# is wrong, the build fails -- which is the entire point.
#
# WHAT THIS DOES *NOT* TEST
#
#   . WSL. A container is ordinary Linux and has none of the Windows/Linux
#     boundary problems (a Windows java.exe on PATH, /mnt/c build trees, CRLF,
#     .wslconfig memory limits). Those live in scripts/wsl_selftest.sh.
#   . macOS. Nothing here says anything about it; it is deferred.
#   . Tonto itself. Building it needs a JDK, gfortran, LAPACK and a good while;
#     that is covered by the other CI workflows. Here the .tex fragments are
#     supplied as a fixture (docker/example-N2, produced by a real N2 run), so
#     this stays a test of the PICTURE toolchain and its install list.
#
# This image is a test fixture, not a supported way to run Tonto. Do not grow
# it into one without deciding to.

FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive

# THE LIST. docs/INSTALLING_RGBI.md quotes exactly this; if the two drift, this
# build is the one telling the truth.
#
#   openbabel                    2D depiction layout
#   ghostscript                  pdfcrop shells out to gs -- undocumented for years
#   pipx                         to install mol2chemfig without touching system python
#   texlive-latex-base           longtable.sty, geometry.sty
#   texlive-latex-recommended    xcolor.sty
#   texlive-pictures             chemfig.sty, tikz.sty
#   texlive-extra-utils          pdfcrop
RUN apt-get update && apt-get install -y --no-install-recommends \
        openbabel \
        ghostscript \
        pipx \
        texlive-latex-base \
        texlive-latex-recommended \
        texlive-pictures \
        texlive-extra-utils \
        texlive-latex-extra \
        poppler-utils \
        ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# mol2chemfigPy3 declares "Requires-Dist: epam.indigo", so this one command
# brings Indigo too. The old instruction to install Indigo separately (and to
# chase cairo fonts) is obsolete.
ENV PIPX_HOME=/opt/pipx PIPX_BIN_DIR=/usr/local/bin
RUN pipx install mol2chemfigPy3

WORKDIR /tonto
COPY scripts/rgbi_doctor.sh   scripts/rgbi_selftest.sh   scripts/
COPY rgbi-scripts/            rgbi-scripts/
COPY scripts/docker/example-N2/  example-N2/

# 1. The doctor must come out CLEAN on a machine built from the list above.
#    Any FAIL here means the list is incomplete -- exactly what this exists for.
RUN scripts/rgbi_doctor.sh

# 2. The doctor's own self-test, so a doctor that has stopped checking cannot
#    quietly pass step 1.
RUN bash scripts/rgbi_selftest.sh

# 3. Draw both pictures for real, from fragments a genuine N2 run produced, and
#    assert the PDFs exist and are non-trivial. Exiting 0 while drawing nothing
#    is precisely how this pipeline used to fail.
RUN cd example-N2 \
    && TONTO_RGBI_SCRIPT_DIRECTORY=/tonto/rgbi-scripts \
       /tonto/rgbi-scripts/make-rgbi-pic --do-H \
    && for f in rgbi-mol-structure+H.pdf rgbi-dial-table+H.pdf; do \
           test -s "$f" || { echo "MISSING: $f"; exit 1; }; \
           pdfinfo "$f" | grep -q '^Pages: *[1-9]' || { echo "EMPTY: $f"; exit 1; }; \
           echo "ok  $f  ($(stat -c%s "$f") bytes)"; \
       done

CMD ["scripts/rgbi_doctor.sh"]
