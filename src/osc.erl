-module(osc).

-export([
    encode/1,
    decode/1
  ]).

encode(Data) ->
  osc_message:encode(Data).

decode(Bin) when is_binary(Bin) ->
  osc_message:decode(Bin).

