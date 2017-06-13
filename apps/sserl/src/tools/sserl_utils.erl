-module (sserl_utils).

-export([record_to_proplist/2, trim/1, gen_randnum/0, timestamp_to_datetime/1]).

-export([timestamp/0, timestamp_ms/0, timestamp_us/0]).


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

% 返回时间戳(单位: 秒)
timestamp() ->
    {T1, T2, T3} = erlang:timestamp(),
    T1*1000000+T2.

% 返回时间戳(单位: 毫秒)
timestamp_ms() ->
    {T1, T2, T3} = erlang:timestamp(),
    T1*1000000000+T2*1000+(T3 div 1000).

% 返回时间戳(单位: 微秒)
timestamp_us() ->
    {T1, T2, T3} = erlang:timestamp(),
    T1*1000000000000+T2*1000000+T3.

timestamp_to_datetime(Timestamp) ->  
    calendar:gregorian_seconds_to_datetime(Timestamp +  
        calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}})). 

gen_randnum() ->
    erlang:list_to_integer(lists:concat(erlang:binary_to_list(crypto:strong_rand_bytes(5)))).
    