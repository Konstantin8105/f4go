package fortran

func TestIo(t *testing.T) {
	tcs := []struct {
		in  string
		out string
	}{
		{"", ""},
		{"I2", "%2d\n"},
		{"I2 I3 I4 I5", "%2d%3d%4d%5d\n"},
		{"I2    I3     I4    I5", "%2d%3d%4d%5d\n"},
	}

	for i := range tcs {
		t.Run(tcs[i].in, func(t *testing.T) {
			ns := scan(tcs[i].in)
			fs := p.parseFormat(ns)
			if fs != tcs[i].out {
				t.Errorf("Not valid : %s", fs)
			}
		})
	}
}
