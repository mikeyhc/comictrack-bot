  -module(custom_logger_formatter).
  -export([format/2]).

  format(#{meta := #{file := File}} = Log, Config) ->
      #{meta := Meta} = Log,
      Filename = filename:basename(File),
      logger_formatter:format(Log#{meta => Meta#{file => Filename}}, Config);
  format(Log, Config) ->
      logger_formatter:format(Log, Config).
