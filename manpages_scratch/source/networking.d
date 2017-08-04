module networking;

import config;

import vibe.vibe;
import stdx.data.json;

import std.exception : enforce;
import std.format;
import std.range.primitives;

void sendMsg (TCPConnection conn, JSONValue json)
{
    static ubyte[] buffer;
    buffer.length = 0;
    assumeSafeAppend(buffer);

    json.writeJSON!(GeneratorOptions.compact)(buffer);
    conn.write(to!string(buffer.length));
    conn.write(":");
    conn.write(buffer);
    conn.flush();
}

JSONValue recvMsg (TCPConnection conn)
{
    auto bytes = to!size_t(assumeUTF(conn.readUntil(cast(ubyte[])":")));

    static ubyte[] buffer;
    buffer.length = bytes;
    assumeSafeAppend(buffer);

    conn.read(buffer);
    return toJSONValue(assumeUTF(buffer));
}

// runs under vibe.d task, I/O is async under the hood
void start ()
{
    enforce(globalConfig.hostname.length > 0 && globalConfig.port > 0);

    auto conn = connectTCP(globalConfig.hostname, globalConfig.port);
    conn.sendMsg(JSONValue([ "me" : JSONValue("name") ]));
    auto response = conn.recvMsg();
    enforce(response["you"] == "name");
}
