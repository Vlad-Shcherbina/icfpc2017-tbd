import std.stdio;
import darg;
import std.socket;
import stdx.data.json;
import std.algorithm.searching;
import std.conv;
import std.format;

import vibe.vibe;

import config;
static import networking;

int main(string[] args) {
  try {
    globalConfig = parseArgs!Options(args[1 .. $]);
  }
  catch (ArgParseError e) {
    writeln(e.msg);
    writeln(usage);
    return 1;
  }
  catch (ArgParseHelp e)
  {
    writeln(usage);
    writeln(help);
    return 0;
  }

  if (globalConfig.playground) {
    writeln(toJSONValue(`{"name":"dlang"}`));
    writeln(find("2:{}", ":")[1 .. $]);
    writeln(pJ("{\"hello\":\"world\"}"));
  }

  // runWorkerTask(&networking.start);
  // runEventLoop();

  return 0;
}

string xJ(string x) {
	return find(x, ":")[1 .. $];
}

string pJ(string x) {
	return format("%s:%s", x.length, x);
}
