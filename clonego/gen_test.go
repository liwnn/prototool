package clonego

import (
	"testing"
)

var (
	pb2src = []byte(`
syntax = "proto2";
import public "other.proto";
option go_package = "foo";
enum EnumAllowingAlias {
  option allow_alias = true;
  UNKNOWN = 0;
  STARTED = 1;
  RUNNING = 2 [(custom_option) = "hello world"];
}
message Outer {
  option (my_option).a = true;
  message Inner {   // Level 2
    required int64 ival = 1;
  }
  repeated Inner inner_message = 2;
  optional EnumAllowingAlias enum_field = 3;
  map<int32, string> my_map = 4;
  extensions 20 to 30;
}
`)

	pb3src = []byte(`
syntax = "proto3";
import public "other.proto";
option go_package = "foo";
enum EnumAllowingAlias {
  option allow_alias = true;
  UNKNOWN = 0;
  STARTED = 1;
  RUNNING = 2 [(custom_option) = "hello world"];
}
message Outer {
  option (my_option).a = true;
  message Inner {   // Level 2
    int64 ival = 1;
  }
  Inter m = 1;
  repeated Inner inner_message = 2;
  EnumAllowingAlias enum_field =3;
  map<int32, string> my_map = 4;
}
`)
)

func TestGen(t *testing.T) {
	{
		_, err := Gen(pb2src, []string{"Outer"})
		if err != nil {
			t.Error(err)
		}
	}
	{
		_, err := Gen(pb3src, []string{"Outer"})
		if err != nil {
			t.Error(err)
		}
	}
}
func TestGenAll(t *testing.T) {
	{
		_, err := GenAll(pb2src)
		if err != nil {
			t.Error(err)
		}
	}
	{
		_, err := GenAll(pb3src)
		if err != nil {
			t.Error(err)
		}
	}
}
