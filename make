#!/bin/bash

arg=${1:-"main"}
ghc="/usr/local/bin/ghc"

case "$arg" in
	main) 
		$ghc --make Main.hs -o ccgui -rtsopts
		;;
	clean)
		rm -rf ccgui
		find . -regex '.*\.\(dyn_\)?\(hi\|o\)$' -delete
                find ./res/test -regex '.*\.\(asy\|eps\|eps001\.ppm\|png\)'  -delete
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
