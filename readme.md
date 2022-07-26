# Prototool
Prototool is a tool for working with Protocol Buffers.
- A formatter that formats you Protobuf files.
- A generator that generates clone code of go language.
## Protofmt
Protofmt formats protobuf files.
### Install
```bash
go install github.com/liwnn/prototool/cmd/protofmt@latest
```
### Usage
```
protofmt [flags] [path ...]
```
flags:
- -w  
    Do not print reformatted output to standard output. It overrides the file.
- -a  
    Align the field
### Example
1. format example.proto and print to console
```
protofmt -a ./example.proto
```
2. format example.proto and overwrite the file
```
protofmt -w -a ./example.proto
```

## protoclone-go
protoclone generate protobuf file's clone code for go.
### Install
```bash
go install github.com/liwnn/prototool/cmd/protoclone-go@latest
```
### Usage
```
protoclone-go [flags] [path ...]
```
flags:
- -w <filename> 
  Do not print reformatted output to standard output. It write to the file.
- -m <message[,message]>
  Generate clone code for message.
### Example
1. Generate clone code for file example.proto and print to console
```
protoclone-go ./example.proto
```
2. Generate clone code for file example.proto and print to example.clone.go
```
protofmt -w=example.clone.go ./example.proto
```
