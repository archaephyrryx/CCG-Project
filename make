#!/bin/bash

arg=${1:-"main"}

case "$arg" in
	main) 
		ghc --make Main.hs -o ccgserver
		;;
	clean)
		rm -rf ccgserver
		find . -regex '.*\.\(hi\|o\)$' | xargs rm
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
