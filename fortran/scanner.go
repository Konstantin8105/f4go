package fortran

import (
	"bufio"
	"bytes"
	"go/token"
	"io"
	"strings"
)

// Scanner represents a lexical scanner.
type Scanner struct {
	r     *bufio.Reader
	start bool
}

// NewScanner returns a new instance of Scanner.
func NewScanner(r io.Reader) *Scanner {
	return &Scanner{
		r:     bufio.NewReader(r),
		start: true,
	}
}

// Scan returns the next token and literal value.
func (s *Scanner) Scan() (tok token.Token, lit string) {

	if s.start {
		s.start = false
		ch := s.read()
		s.unread()
		if ch == 'C' || ch == 'c' || ch == '*' {
			return s.scanComment()
		}
	}

	var ok bool
	if tok, lit, ok = s.ignoreWhitespace(); ok {
		return tok, lit
	}

	ch := s.read()

	// If we see whitespace then consume all contiguous whitespace.
	// If we see a letter then consume as an ident or reserved word.
	// If we see a digit then consume as a number.
	if isLetter(ch) {
		s.unread()
		return s.scanIdent()
	} else if isDigit(ch) {
		s.unread()
		return s.scanNumber()
	}

	// Otherwise read the individual character.
	switch ch {
	case '$':
		// contiguous line
		return s.Scan()
	case eof:
		return token.EOF, ""
	case '(':
		return token.LPAREN, string(ch)
	case ')':
		return token.RPAREN, string(ch)
	case ',':
		return token.COMMA, string(ch)
	case ':':
		s.unread()
		return s.scanColon()
	case '=':
		return token.ASSIGN, string(ch)
	case '/':
		return token.QUO, string(ch)
	case '+':
		return token.ADD, string(ch)
	case '-':
		return token.SUB, string(ch)

	case '.':
		s.unread()
		return s.scanPeriod()

	case '>':
		return token.GTR, string(ch)
	case '<':
		return token.LSS, string(ch)

	case '*':
		s.unread()
		return s.scanStar()
	case '!':
		s.unread()
		return s.scanComment()
	case '"', '\'':
		s.unread()
		return s.scanString()
	}

	return token.ILLEGAL, string(ch)
}

func (s *Scanner) ignoreWhitespace() (
	tok token.Token, lit string, found bool) {
	for {
		if ch := s.read(); ch == eof {
			break
		} else if ch != ' ' && ch != '\t' && ch != '\n' && ch != '\r' {
			s.unread()
			break
		} else if ch == '\n' {
			tok, lit, found = NEW_LINE, "\n", true
			s.start = true
			return
		}
	}

	return
}

func (s *Scanner) scanStar() (tok token.Token, lit string) {
	// Create a buffer and read the current character into it.
	var buf bytes.Buffer
	buf.WriteRune(s.read())

	// Read every subsequent ident character into the buffer.
	// Non-ident characters and EOF will cause the loop to exit.
	ch := s.read()
	if ch == '*' {
		buf.WriteRune(ch)
		return DOUBLE_STAR, buf.String()
	}
	s.unread()

	return token.MUL, buf.String()
}

func (s *Scanner) scanPeriod() (tok token.Token, lit string) {
	var buf bytes.Buffer

	for i := 0; i < 6; i++ {
		if ch := s.read(); ch == eof {
			break
		} else if ch == '.' && i != 0 {
			_, _ = buf.WriteRune(ch)
			break
		} else {
			_, _ = buf.WriteRune(ch)
		}
	}

	switch strings.ToUpper(buf.String()) {
	case ".LT.":
		return token.LSS, buf.String()
	case ".GT.":
		return token.GTR, buf.String()
	case ".LE.":
		return token.LEQ, buf.String()
	case ".GE.":
		return token.GEQ, buf.String()
	case ".NOT.", ".NE.":
		return token.NEQ, buf.String()
	case ".EQ.":
		return token.EQL, buf.String()
	case ".AND.":
		return token.LAND, buf.String()
	case ".OR.":
		return token.LOR, buf.String()
	case ".TRUE.", ".FALSE.":
		return token.IDENT, buf.String()
	}

	for i := 0; i < buf.Len()-1; i++ {
		s.unread()
	}

	// Otherwise return as a regular identifier.
	return token.PERIOD, "."
}

func (s *Scanner) scanColon() (tok token.Token, lit string) {
	// Create a buffer and read the current character into it.
	var buf bytes.Buffer
	buf.WriteRune(s.read())

	// Read every subsequent ident character into the buffer.
	// Non-ident characters and EOF will cause the loop to exit.
	for {
		if ch := s.read(); ch == eof {
			break
		} else if ch != ':' {
			s.unread()
			break
		} else {
			_, _ = buf.WriteRune(ch)
		}
	}

	switch buf.String() {
	case ":":
		return token.COLON, buf.String()
	case "::":
		return DOUBLE_COLON, buf.String()
	}

	// Otherwise return as a regular identifier.
	return token.ILLEGAL, buf.String()
}

func (s *Scanner) scanNumber() (tok token.Token, lit string) {
	// Create a buffer and read the current character into it.
	var buf bytes.Buffer
	buf.WriteRune(s.read())

	// Read every subsequent ident character into the buffer.
	// Non-ident characters and EOF will cause the loop to exit.
	for {
		if ch := s.read(); ch == eof {
			break
		} else if ch == 'E' || ch == 'e' ||
			ch == 'D' || ch == 'd' {
			_, _ = buf.WriteRune(ch)
			d := s.read()
			if d == '+' || d == '-' || isDigit(d) {
				_, _ = buf.WriteRune(d)
			} else {
				s.unread()
			}
		} else if !isDigit(ch) && ch != '.' &&
			ch != 'E' && ch != 'e' &&
			ch != 'D' && ch != 'd' {
			s.unread()
			break
		} else {
			_, _ = buf.WriteRune(ch)
		}
	}

	if !strings.Contains(buf.String(), ".") {
		return token.INT, buf.String()
	}

	// Otherwise return as a regular identifier.
	return token.FLOAT, buf.String()
}

func (s *Scanner) scanString() (tok token.Token, lit string) {
	// Create a buffer and read the current character into it.
	var buf bytes.Buffer
	symbol := s.read()
	buf.WriteRune(symbol)

	// Read every subsequent ident character into the buffer.
	// Non-ident characters and EOF will cause the loop to exit.
	for {
		if ch := s.read(); ch == eof {
			break
		} else if ch == symbol {
			_, _ = buf.WriteRune(ch)
			break
		} else {
			_, _ = buf.WriteRune(ch)
		}
	}

	// Otherwise return as a regular identifier.
	return token.STRING, buf.String()
}

func (s *Scanner) scanComment() (tok token.Token, lit string) {
	// Create a buffer and read the current character into it.
	var buf bytes.Buffer
	buf.WriteRune(s.read())

	// Read every subsequent ident character into the buffer.
	// Non-ident characters and EOF will cause the loop to exit.
	for {
		if ch := s.read(); ch == eof {
			break
		} else if ch == '\n' {
			s.unread()
			break
		} else {
			_, _ = buf.WriteRune(ch)
		}
	}

	// Otherwise return as a regular identifier.
	return token.COMMENT, buf.String()
}

// scanIdent consumes the current rune and all contiguous ident runes.
func (s *Scanner) scanIdent() (tok token.Token, lit string) {
	// Create a buffer and read the current character into it.
	var buf bytes.Buffer
	buf.WriteRune(s.read())

	// Read every subsequent ident character into the buffer.
	// Non-ident characters and EOF will cause the loop to exit.
	for {
		if ch := s.read(); ch == eof {
			break
		} else if !isLetter(ch) && !isDigit(ch) && ch != '_' {
			s.unread()
			break
		} else {
			_, _ = buf.WriteRune(ch)
		}
	}

	switch strings.ToUpper(buf.String()) {
	case "SUBROUTINE":
		return SUBROUTINE, buf.String()
	case "IMPLICIT":
		return IMPLICIT, buf.String()

	case "INTEGER":
		return INTEGER, buf.String()
	case "CHARACTER":
		return CHARACTER, buf.String()
	case "LOGICAL":
		return LOGICAL, buf.String()
	case "COMPLEX":
		return COMPLEX, buf.String()
	case "REAL":
		return REAL, buf.String()

	case "EXTERNAL":
		return EXTERNAL, buf.String()
	case "END":
		return END, buf.String()
	case "DO":
		return DO, buf.String()
	case "ENDDO":
		return ENDDO, buf.String()
	case "FUNCTION":
		return FUNCTION, buf.String()
	case "IF":
		return token.IF, buf.String()
	case "ELSE":
		return token.ELSE, buf.String()
	case "CONTINUE":
		return token.CONTINUE, buf.String()
	case "CALL":
		return CALL, buf.String()
	case "THEN":
		return THEN, buf.String()
	case "RETURN":
		return token.RETURN, buf.String()
	}

	// Otherwise return as a regular identifier.
	return token.IDENT, buf.String()
}

// read reads the next rune from the buffered reader.
// Returns the rune(0) if an error occurs (or io.EOF is returned).
func (s *Scanner) read() rune {
	ch, _, err := s.r.ReadRune()
	if err != nil {
		return eof
	}
	return ch
}

// unread places the previously read rune back on the reader.
func (s *Scanner) unread() { _ = s.r.UnreadRune() }

// isLetter returns true if the rune is a letter.
func isLetter(ch rune) bool { return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') }

// isDigit returns true if the rune is a digit.
func isDigit(ch rune) bool { return (ch >= '0' && ch <= '9') }

// eof represents a marker rune for the end of the reader.
var eof = rune(0)
