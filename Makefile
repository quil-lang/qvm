all:
	buildapp --output qvm \
		 --asdf-tree "~/quicklisp/dists/quicklisp/software/" \
		 --asdf-path "." \
		 --load-system qvm \
		 --compress-core \
		 --logfile build-output.log \
		 --entry qvm::%main

clean:
	rm -f qvm build-output.log
