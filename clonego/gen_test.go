package clonego

import (
	"testing"
)

func TestGen(t *testing.T) {
	{
		_, err := Gen("./testprotos/proto2.proto", nil, []string{"Outer"})
		if err != nil {
			t.Error(err)
		}
	}
	{
		_, err := Gen("./testprotos/proto3.proto", nil, []string{"Outer"})
		if err != nil {
			t.Error(err)
		}
	}
}

func TestGenAll(t *testing.T) {
	{
		_, err := GenAll("./testprotos/proto2.proto", nil)
		if err != nil {
			t.Error(err)
		}
	}
	{
		_, err := GenAll("./testprotos/proto3.proto", nil)
		if err != nil {
			t.Error(err)
		}
	}
}
