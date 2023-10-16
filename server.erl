-module(server).
-export([start/1, stop/1, chat_handler/2]).

-record(server_state,
 {channels = ["#doom"]}).

% Logic server
% Parameters match the generic server start function
% Start a new server process with the given name
% Do not change the signature of this function.
 % ServerAtom is "shire" in our case.
start(ServerAtom) ->
    genserver:start(ServerAtom, #server_state.channels, fun chat_handler/2).

%   - takes 2 params : state, request message
%   - returns a tuple: new state, response message
%   - handler reply to genserver
chat_handler(State, Data) ->
    case Data of
       {join, From, Channel, Nick} ->
            join_server(State, Channel, From, Nick)
        end.

% Check to see if channel exist or not
% If exist -> join that channel
% If not exist -> Create and join
join_server(State, Channel, From, Nick) ->
    Channels = State#server_state.channels,
    io:fwrite("~p ~n", [Channel]),
    case lists:member(Channel, Channels) of
        true ->
            %channel:join(Channel, Nick, From),
            genserver:request(Channel, {join, Nick, From});
        false ->
            channel:start(Channel),
            genserver:request(Channel, {join, Nick, From}),
                NewChannels = [Channel | Channels],
                #server_state{channels = NewChannels}
                end.



    




% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    ServerAtom ! {stop, self(), 0},
    ok.
    % TODO Implement function
    % Return ok
    % not_implemented.
