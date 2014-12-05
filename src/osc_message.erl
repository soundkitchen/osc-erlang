-module(osc_message).

-export([
    encode/1,
    decode/1
  ]).

encode({bundle, Time, Messages}) ->
  EncodedTime = encode_time(Time),
  EncodedMessages = encode_bundle(Messages, <<>>),
  <<"#bundle", 0, EncodedTime/binary, EncodedMessages/binary>>;
encode({message, Address, Args}) ->
  Bytes = encode_string(Address),
  {Data, Types} = encode_args(Args),
  <<Bytes/binary, Types/binary, Data/binary>>.

encode_bundle([], Acc) ->
  Acc;
encode_bundle([Msg|Rest], Acc) ->
  Bin = encode(Msg),
  Size = byte_size(Bin),
  encode_bundle(Rest, <<Acc/binary, Size:32, Bin:Size/binary>>).

encode_args(Args) ->
  encode_args(Args, <<>>, <<>>).
encode_args([], Data, Types) ->
  {Data, pad(<<$,, Types/binary, 0>>, 4)};
encode_args([Int|Rest], Acc, Types) when is_integer(Int) ->
  encode_args(Rest, <<Acc/binary, Int:32/integer>>, <<Types/binary, $i>>);
encode_args([Float|Rest], Acc, Types) when is_float(Float) ->
  encode_args(Rest, <<Acc/binary, Float:32/float>>, <<Types/binary, $f>>);
encode_args([{time, Seconds, Fractions}|Rest], Acc, Types) ->
  Time = encode_time({time, Seconds, Fractions}),
  encode_args(Rest, <<Acc/binary, Time:64>>, <<Types/binary, $t>>);
encode_args([immediately|Rest], Acc, Types) ->
  Time = encode_time(immediately),
  encode_args(Rest, <<Acc/binary, Time:64>>, <<Types/binary, $t>>);
encode_args([{blob, Raw}|Rest], Acc, Types) when is_binary(Raw) ->
  Bin = encode_blob(Raw),
  encode_args(Rest, <<Acc/binary, Bin/binary>>, <<Types/binary, $b>>);
encode_args([true|Rest], Acc, Types) ->
  encode_args(Rest, Acc, <<Types/binary, $T>>);
encode_args([false|Rest], Acc, Types) ->
  encode_args(Rest, Acc, <<Types/binary, $F>>);
encode_args([N|Rest], Acc, Types) when N =:= null orelse N =:= undefined ->
  encode_args(Rest, Acc, <<Types/binary, $N>>);
encode_args([impulse|Rest], Acc, Types) ->
  encode_args(Rest, Acc, <<Types/binary, $I>>);
encode_args([Atom|Rest], Acc, Types) when is_atom(Atom) ->
  Symbol = encode_string(atom_to_list(Atom)),
  encode_args(Rest, <<Acc/binary, Symbol/binary>>, <<Types/binary, $s>>);
encode_args([{rgba, R, G, B, A}|Rest], Acc, Types) ->
  encode_args(Rest, <<Acc/binary, R/integer, G/integer, B/integer, A/integer>>, <<Types/binary, $r>>);
encode_args([{midi, Port, Status, Data1, Data2}|Rest], Acc, Types) ->
  encode_args(Rest, <<Acc/binary, Port/integer, Status/integer, Data1/binary, Data2/binary>>, <<Types/binary, $m>>);
encode_args([L|Rest], Acc, Types) when is_list(L) ->
  case is_string(L) of
    true ->
      Bin = encode_string(L),
      encode_args(Rest, <<Acc/binary, Bin/binary>>, <<Types/binary, $s>>);
    false ->
      {Bytes, T} = encode_args(L, <<>>, <<>>),
      encode_args(Rest, <<Acc/binary, Bytes/binary>>, <<Types/binary, $[, T/binary, $]>>)
  end;
encode_args([Raw|Rest], Acc, Types) when is_binary(Raw) ->
  Bin = encode_string(Raw),
  encode_args(Rest, <<Acc/binary, Bin/binary>>, <<Types/binary, $s>>).

encode_string(Str) when is_list(Str) orelse is_binary(Str) ->
  Bin = unicode:characters_to_binary(Str),
  pad(<<Bin/binary, 0>>, 4).

encode_blob(Bin) when is_binary(Bin) ->
  pad(<<(byte_size(Bin)):32, Bin/binary>>, 4).

encode_time(immediately) ->
  <<1:64>>;
encode_time({time, Seconds, Fractions}) ->
  <<Seconds:32, Fractions:32>>.

%encode_types([], Acc) ->
%  ok;
%encode_types([Types|Rest], Acc) ->
%    encode_types(Rest, [<<Types/integer>>|Acc]).

decode(<<"#bundle", 0, Time:8/binary, Rest/binary>>) ->
  {bundle, decode_time(Time), decode_bundle(Rest, [])};
decode(Bin) ->
  {Address, Rest1} = decode_string(Bin),
  {<<$,, Types/binary>>, Rest2} = decode_string(Rest1),
  io:format("~p, ~p~n", [Address, Types]),
  {Args, _} = decode_args(Rest2, Types),
  {message, Address, Args}.

decode_bundle(<<>>, Acc) ->
  lists:reverse(Acc);
decode_bundle(<<Length:32, Bin:Length/binary, Rest/binary>>, Acc) ->
  decode_bundle(Rest, [decode(Bin)|Acc]).

decode_args(Bin, Types) ->
  decode_args(Bin, Types, []).

decode_args(Bin, <<>>, Acc) ->
  {lists:reverse(Acc), Bin};
decode_args(<<Int:32, Rest/binary>>, <<$i, Types/binary>>, Acc) ->
  decode_args(Rest, Types, [Int|Acc]);
decode_args(<<Float:32/float, Rest/binary>>, <<$f, Types/binary>>, Acc) ->
  decode_args(Rest, Types, [Float|Acc]);
decode_args(Bin, <<$s, Types/binary>>, Acc) ->
  {Str, Rest} = decode_string(Bin),
  decode_args(Rest, Types, [Str|Acc]);
decode_args(Bin, <<$b, Types/binary>>, Acc) ->
  {Blob, Rest} = decode_blob(Bin),
  decode_args(Rest, Types, [Blob|Acc]);
decode_args(<<Int:64, Rest/binary>>, <<$h, Types/binary>>, Acc) ->
  decode_args(Rest, Types, [Int|Acc]);
decode_args(<<Time:8/binary, Rest/binary>>, <<$t, Types/binary>>, Acc) ->
  decode_args(Rest, Types, [decode_time(Time)|Acc]);
decode_args(<<Float:64/float, Rest/binary>>, <<$d, Types/binary>>, Acc) ->
  decode_args(Rest, Types, [Float|Acc]);
decode_args(Bin, <<$S, Types/binary>>, Acc) ->
  {Str, Rest} = decode_string(Bin),
  decode_args(Rest, Types, [Str|Acc]);
decode_args(<<Char:32, Rest/binary>>, <<$c, Types/binary>>, Acc) ->
  decode_args(Rest, Types, [Char|Acc]);
decode_args(<<R, G, B, A, Rest/binary>>, <<$r, Types/binary>>, Acc) ->
  decode_args(Rest, Types, [{rgba, R, G, B, A}|Acc]);
decode_args(<<Port, Status, Data1, Data2, Rest/binary>>, <<$m, Types/binary>>, Acc) ->
  decode_args(Rest, Types, [{midi, Port, Status, Data1, Data2}|Acc]);
decode_args(Bin, <<$T, Types/binary>>, Acc) ->
  decode_args(Bin, Types, [true|Acc]);
decode_args(Bin, <<$F, Types/binary>>, Acc) ->
  decode_args(Bin, Types, [false|Acc]);
decode_args(Bin, <<$N, Types/binary>>, Acc) ->
  decode_args(Bin, Types, [null|Acc]);
decode_args(Bin, <<$I, Types/binary>>, Acc) ->
  decode_args(Bin, Types, [impulse|Acc]);
decode_args(Bin, <<$[, Types/binary>>, Acc) ->
  {Array, RestBin, RestTypes} = decode_args(Bin, Types, []),
  decode_args(RestBin, RestTypes, [Array|Acc]);
decode_args(Bin, <<$], Types/binary>>, Acc) ->
  {lists:reverse(Acc), Bin, Types}.

decode_string(Bin) ->
  decode_string(Bin, <<>>).
decode_string(<<0, Data/binary>>, Acc) ->
  L = pad_len(byte_size(Acc) + 1, 4),
  <<_:L/integer-unit:8, Rest/binary>> = Data,
  {Acc, Rest};
decode_string(<<Byte, Rest/binary>>, Acc) ->
  decode_string(Rest, <<Acc/binary, Byte>>).

decode_blob(<<Length:32, Bytes:Length/binary, Rest/binary>>) ->
  L = pad_len(Length + 4, 4),
  <<_:L/integer-unit:8, Rest1/binary>> = Rest,
  {{blob, Bytes}, Rest1}.

decode_time(<<1:64>>) ->
  immediately;
decode_time(<<Seconds:32, Fractions:32>>) ->
  {time, Seconds, Fractions}.

% internal api.

%% @doc Zero-pads the binary.
%% @spec pad(Bytes::binary(), Pad::integer()) -> binary()
pad(B, P) when is_binary(B), is_integer(P) ->
  L = pad_len(size(B), P),
  <<B/binary, 0:L/integer-unit:8>>.

%% @doc Returns the length the binary has to be padded by.
%% @spec pad_len(Length::binary(), Padding::integer()) -> integer()
pad_len(L, P) when L rem P == 0 ->
  0;
pad_len(L, P) ->
  P - (L rem P).

%% @doc Checks whether a list is a string.
%% @spec is_string(List) -> true | false
is_string([]) -> true;
is_string([H|_]) when not is_integer(H) -> false;
is_string([_|T]) -> is_string(T);
is_string(_) -> false.
