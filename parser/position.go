package parser

type Pos int

const NoPos Pos = 0

type Position struct {
	Line   int
	Column int
	Offset int
}

type FileInfo struct {
	lines []int
}

func NewFileInfo() *FileInfo {
	return &FileInfo{lines: []int{0}}
}

func (f *FileInfo) AddLine(offset int) {
	f.lines = append(f.lines, offset)
}

func (f *FileInfo) Line(pos Pos) int {
	i, j := 0, len(f.lines)
	for i < j {
		h := i + (j-i)>>1
		if f.lines[h] <= int(pos) {
			i = h + 1
		} else {
			j = h
		}
	}
	return i
}

func (f *FileInfo) Position(p Pos) (pos Position) {
	pos.Offset = int(p)
	pos.Line = f.Line(p)
	pos.Column = int(p) - f.lines[pos.Line-1]
	return
}
