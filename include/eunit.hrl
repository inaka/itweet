-include_lib("eunit/include/eunit.hrl").
-define(assertLess(Min, Max),
  ((fun (__X, __V) ->
             case (__X < __V) of
               true -> ok;
               false -> erlang:error({assertLess_failed,
                                     [{module, ?MODULE},
                                      {line, ?LINE},
                                      {expression, (??Min " < " ??Max)},
                                      {expected_min, __X},
                                      {actual_min, __V}]})
             end
    end)(Min, Max))).
-define(assertLessOrEq(Min, Max),
  ((fun (__X, __V) ->
             case (__X =< __V) of
               true -> ok;
               false -> erlang:error({assertLessOrEq_failed,
                                     [{module, ?MODULE},
                                      {line, ?LINE},
                                      {expression, (??Min " =< " ??Max)},
                                      {expected_min, __X},
                                      {actual_min, __V}]})
             end
    end)(Min, Max))).
-define(assertMember(Member, List),
  ((fun (__X, __V) ->
             case lists:member(__X, __V) of
               true -> ok;
               false -> erlang:error({assertMember_failed,
                                     [{module, ?MODULE},
                                      {line, ?LINE},
                                      {expression, ("lists:member(" ??Member " , " ??List ")")},
                                      {element, __X},
                                      {list, __V}]})
             end
    end)(Member, List))).
-define(assertNotMember(Member, List),
  ((fun (__X, __V) ->
             case lists:member(__X, __V) of
               false -> ok;
               true -> erlang:error({assertNotMember_failed,
                                     [{module, ?MODULE},
                                      {line, ?LINE},
                                      {expression, ("not lists:member(" ??Member " , " ??List ")")},
                                      {element, __X},
                                      {list, __V}]})
             end
    end)(Member, List))).
-define(assertBinMatch(Text, Patterns),
  ((fun (__X, __V) ->
              case binary:match(__X, __V) of
                nomatch -> erlang:error({assertBinMatch_failed,
                                         [{module, ?MODULE},
                                          {line, ?LINE},
                                          {expression, ("binary:match(" ??Text " , " ??Patterns ")")},
                                          {text, __X},
                                          {patterns, __V}]});
                _ -> ok
             end
    end)(Text, Patterns))).
-define(fail(Msg),
        erlang:error({failed,
                       [{module, ?MODULE},
                       {line, ?LINE},
                       {msg, Msg}]})).
