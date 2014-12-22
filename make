#!/bin/bash

arg=${1:-"main"}

case "$arg" in
	main) 
		ghc --make Main.hs -o ccgserver
		;;
	clean)
		rm -rf *.o *.hi */*.o */*.hi ccgserver
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
