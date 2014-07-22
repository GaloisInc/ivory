#!/bin/bash

ISABELLE="$HOME/Applications/Isabelle2013-2.app/Isabelle/bin/isabelle"
REV=$(git rev-list --max-count=1 HEAD --abbrev-commit)
OUT=output/document

mkdir -p ${OUT}

echo "\def\gitrev{$REV}" > ${OUT}/gitrev.tex

${ISABELLE} build -D . Ivory

cp output/document.pdf ivory.pdf
