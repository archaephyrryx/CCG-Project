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
		tar xzf jpgs.tgz
		;;
	depack)
		rm -rf jpgs/
		;;
	*)
		echo $"Usage: $0 {main|clean|unpack|depack}"
		exit 1
		;;
esac
