-module(osc_message_tests).

-include_lib("eunit/include/eunit.hrl").

encode_time_test_() ->
  [
    ?_assertEqual(<<0,0,0,0,0,0,0,1>>, osc_message:encode_time(immediately)),
    ?_assertEqual(<<0,0,4,210,0,0,22,46>>, osc_message:encode_time({time, 1234, 5678}))
  ].

decode_time_test_() ->
  [
    ?_assertEqual(immediately, osc_message:decode_time(<<0,0,0,0,0,0,0,1>>)),
    ?_assertEqual({time, 1234, 5678}, osc_message:decode_time(<<0,0,4,210,0,0,22,46>>))
  ].

pad_test_() ->
  [
    ?_assertEqual(<<115,112,97,109>>, osc_message:pad(<<115,112,97,109>>, 4)),
    ?_assertEqual(<<101,103,103,0>>, osc_message:pad(<<101,103,103>>, 4)),
    ?_assertEqual(<<110,105,0,0>>, osc_message:pad(<<110,105>>, 4))
  ].

pad_len_test_() ->
  [
    ?_assertEqual(0, osc_message:pad_len(4, 4)),
    ?_assertEqual(0, osc_message:pad_len(8, 4)),
    ?_assertEqual(1, osc_message:pad_len(3, 4)),
    ?_assertEqual(1, osc_message:pad_len(7, 4))
  ].
