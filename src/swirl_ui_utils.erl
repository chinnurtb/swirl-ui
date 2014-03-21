-module(swirl_ui_utils).

%% public
-export([
    safe_binary_to_pid/2,
    safe_string_to_uuid/2,
    split_path/1
]).

%% public
safe_binary_to_pid(List, Default) ->
    try list_to_pid(binary_to_list(List))
    catch
        exit:badarg -> Default
    end.

safe_string_to_uuid(String, Default) ->
    try uuid:string_to_uuid(String)
    catch
        exit:badarg -> Default
    end.

split_path(<<"/">>) ->
    [];
split_path(<<"/", Rest/binary>>) ->
    binary:split(Rest, <<"/">>, [global, trim]).