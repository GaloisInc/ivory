#!/bin/sh

# Lee Pike
# (c) Galois, Inc.
#
# Package and upload packages to Hackage with new version numbers.
#
# Usage:
# In ivory/
# USER=XXX PASS=YYY sh hackage.sh

set -e

DIRS="ivory \
      ivory-artifact
      ivory-backend-c \
      ivory-eval \
      ivory-examples \
      ivory-hw \
      ivory-opts \
      ivory-serialize \
      ivory-stdlib \
      ivory-quickcheck"

# Print current package versions
show_versions() {
for i in $DIRS
do
  cd $i
  echo "package: " $i
  awk '/^version:/ {print $1 "\t" $2}' $i.cabal
  cd ..
done
}

OLD_VERSION=0.1.0.4
NEW_VERSION=0.1.0.5

change_versions() {
for i in $DIRS
do
  cd $i
  echo "package: " $i
  sed -i.tmp -e "s/^\(version:[ ]*\)\($OLD_VERSION\)/\1$NEW_VERSION/" $i.cabal
  rm $i.cabal.tmp
  cd ..
done
}

package() {
for i in $DIRS
do
  cd $i
  echo "package: " $i
  cabal sdist
  cabal check
  cd ..
done
}

upload() {
for i in $DIRS
do
  cd $i
  echo "package: " $i
  cabal upload -u $USER -p $PASS dist/$i-$NEW_VERSION.tar.gz
  cd ..
done
}

show_versions;
change_versions;
show_versions;
package;
upload;


