#!/bin/bash

arg=${1:-"main"}
ghc=${2:-"/usr/local/bin/ghc"}

case "$arg" in
	main) 
		$ghc --make Main.hs -o ccgserver -rtsopts
		;;
	clean)
		rm -rf ccgserver
		find . -regex '.*\.\(dyn_\)?\(hi\|o\)$' | xargs rm
		;;
	unpack)
		tar -x -C res -z -f res/cards.tgz
		;;
	depack)
		rm -rf res/cards/
		;;
	*)
		echo $"Usage: $0 {main|clean|unpack|depack}"
		exit 1
		;;
esac
