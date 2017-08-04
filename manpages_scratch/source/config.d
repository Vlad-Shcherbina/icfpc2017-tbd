module config;

import darg;

struct Options {
  @Option("help", "h")
  @Help("Prints this help.")
  OptionFlag help;

  @Option("server", "s")
  @Help("Game server to connect to.")
  string hostname; 

  @Option("port", "p")
  @Help("Port of the game server")
  ushort port; 

  @Option("playground", "g")
  @Help("Print a bunch of shit from learning D")
  bool playground;
}

immutable usage = usageString!Options("manpages_scratch");
immutable help = helpString!Options;

Options globalConfig;
