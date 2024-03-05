
all: build

clean:
	rm *.so

lint:
	golangci-lint run

build:
	go build -buildmode=plugin -o mongo.so *.go

test: lint build
	go test -coverprofile=cov.out
	make -C test

.PHONY: all build
