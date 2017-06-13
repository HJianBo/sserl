-module (sserl_utils).

-export([record_to_proplist/2, trim/1, timestamp/0, gen_randnum/0]).


%% @spec record_to_proplist(Record, Fields) -> proplist()
%% @doc calls record_to_proplist/3 with a default TypeKey of '__record'
record_to_proplist(Record, Fields) ->
    record_to_proplist(Record, Fields, '__record').

%% @spec record_to_proplist(Record, Fields, TypeKey) -> proplist()
%% @doc Return a proplist of the given Record with each field in the
%%      Fields list set as a key with the corresponding value in the Record.
%%      TypeKey is the key that is used to store the record type
%%      Fields should be obtained by calling record_info(fields, record_type)
%%      where record_type is the record type of Record
record_to_proplist(Record, Fields, TypeKey)
  when tuple_size(Record) - 1 =:= length(Fields) ->
    lists:zip([TypeKey | Fields], tuple_to_list(Record)).

trim(S) ->
    re:replace(S, "(^\\s+)|(\\s+$)", "", [global, {return, list}]).

timestamp() ->
    {T1, T2, T3} = erlang:timestamp(),
    % s
    T1*1000000+T2+T3/1000000.
    % ms
    % T1*1000000000+T2*1000+T3/1000.

gen_randnum() ->
    erlang:list_to_integer(lists:concat(erlang:binary_to_list(crypto:strong_rand_bytes(5)))).
    