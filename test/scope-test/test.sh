#!/bin/sh

case $2 in
  p*) exit 0 ;;
  n*) exit 1 ;;
esac

exit 2
