
all: build

clean:
	rm *.so

lint:
	golangci-lint run

build:
	go build -buildmode=plugin -o mongo.so *.go

test: lint
	go test -coverprofile=cov.out

.PHONY: all build
