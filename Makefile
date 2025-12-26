
all: build

clean:
	rm -f *.so

lint:
	golangci-lint run

build:
	go mod tidy
	go build -buildmode=plugin -o mongo.so *.go

test: lint build
	make -C test
	$Q go tool cover -func=cov.out | grep "total:"
	$(eval COVERAGE = $(shell go tool cover -func=cov.out | grep "total:" | grep -Eo "[0-9]+\.[0-9]+"))
	sh ./assets/gen-coverage-badge.sh $(COVERAGE)

.PHONY: all build
