import std.stdio;
import darg;
import std.socket;
import stdx.data.json;
import std.algorithm.searching;
import std.conv;
import std.format;

import config;

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

  //online(o.hostname, o.port);


	if (globalConfig.playground) {
		writeln(toJSONValue(`{"name":"dlang"}`));
		writeln(find("2:{}", ":")[1 .. $]);
		writeln(pJ("{\"hello\":\"world\"}"));
  }

  return 0;
}

void online(const char[] hostname, ushort port) {
	auto socket = new Socket(AddressFamily.INET,  SocketType.STREAM);
	char[1024] buffer;
	socket.connect(new InternetAddress(hostname, port));
	auto received = socket.receive(buffer); // wait for the server to say hello
	writeln("Server said: ", buffer[0 .. received]);
	foreach(line; stdin.byLine) {
		socket.send(line);
		writeln("Server said: ", buffer[0 .. socket.receive(buffer)]);
	}
}

string xJ(string x) {
	return find(x, ":")[1 .. $];
}

string pJ(string x) {
	return format("%s:%s", x.length, x);
}
