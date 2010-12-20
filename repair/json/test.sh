ulimit -t 2
case $2 in
	p1) tests/runtest $1 | diff expected - && exit 0 ;;
	n1) exit 1 ;;
esac
exit 1
