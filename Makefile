run:
	go run cmd/whistle/main.go

test:
	go test ./... -test.short -count=1

clean:
	rm -rf build

build: clean
	go build -o ./build/whistle ./cmd/whistle/

install:
	go install ./cmd/whistle
