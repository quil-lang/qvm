all: build-app

build-app:
	buildapp --output qvm \
		 --asdf-tree "~/quicklisp/dists/quicklisp/software/" \
		 --asdf-tree "./../" \
		 --load-system qvm-app \
		 --logfile build-output.log \
		 --entry qvm-app::%main

# Only works on Linux.
docker: build-app
	docker build -t qvm .

clean:
	rm -f qvm build-output.log
