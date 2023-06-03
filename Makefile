run:
	go run cmd/whistle/main.go

test:
	go test ./... -test.short -count=1

build:
	go build -o build/whistle cmd/whistle/main.go

install:
	go install ./cmd/whistle
