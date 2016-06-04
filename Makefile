all:
	buildapp --output qvm \
		 --asdf-tree "~/quicklisp/dists/quicklisp/software/" \
		 --asdf-tree "./../" \
		 --load-system qvm-app \
		 --logfile build-output.log \
		 --entry qvm-app::%main

clean:
	rm -f qvm build-output.log
