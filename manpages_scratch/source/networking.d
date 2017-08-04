module networking;

import config;

import vibe.vibe;

void start ()
{
    auto conn = connectTCP(globalConfig.hostname, globalConfig.port);
}
