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
	// Read the next rune.
	firstLetterInLine := s.ignoreWhitespace()
	if firstLetterInLine == 'C' || firstLetterInLine == '*' {
		return s.scanComment()
	}
	ch := s.read()

	if s.start {
		s.start = false
		if ch == 'C' || ch == '*' {
			return s.scanComment()
		}
	}

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
		return token.PERIOD, string(ch)
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

func (s *Scanner) ignoreWhitespace() (firstLetterInLine rune) {
	for {
		if ch := s.read(); ch == eof {
			break
		} else if ch != ' ' && ch != '\t' && ch != '\n' {
			s.unread()
			break
		} else if ch == '\n' {
			firstLetterInLine = s.read()
			s.unread()
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
	} else {
		s.unread()
	}

	return token.MUL, buf.String()
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
		} else if !isDigit(ch) {
			s.unread()
			break
		} else {
			_, _ = buf.WriteRune(ch)
		}
	}

	// Otherwise return as a regular identifier.
	return token.IDENT, buf.String()
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
	case "PROGRAM":
		return PROGRAM, buf.String()
	case "IMPLICIT":
		return IMPLICIT, buf.String()
	case "INTEGER":
		return INTEGER, buf.String()
	case "FUNCTION":
		return FUNCTION, buf.String()
	case "IF":
		return token.IF, buf.String()
	case "ELSE":
		return token.ELSE, buf.String()
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
