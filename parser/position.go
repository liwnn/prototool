package parser

type Pos int

const NoPos Pos = 0

type Position struct {
	Line   int
	Column int
	Offset int
}
type FileSet struct {
	files []*FileInfo // list of files in the order added to the set
}

func NewFileSet() *FileSet {
	return &FileSet{}
}

func (s *FileSet) AddFile(filename string) *FileInfo {
	file := NewFileInfo(filename)
	s.files = append(s.files, file)
	return file
}

func (s *FileSet) PositionFor(p Pos) (pos Position) {
	if p != NoPos {
		if f := s.file(p); f != nil {
			return f.Position(p)
		}
	}
	return
}

func (s *FileSet) file(pos Pos) *FileInfo {
	return s.files[0] // TODO: search
}

func (s *FileSet) File(pos Pos) *FileInfo {
	return s.file(pos)
}

func (s *FileSet) Line(pos Pos) int {
	return s.file(pos).Line(pos)
}

type FileInfo struct {
	name  string
	lines []int
}

func NewFileInfo(name string) *FileInfo {
	return &FileInfo{name: name, lines: []int{0}}
}

func (f *FileInfo) Name() string {
	return f.name
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
