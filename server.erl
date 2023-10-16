-module(server).
-export([start/1, stop/1, chat_handler/2]).

-record(server_state,
 {channels = []}).

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

        end.


join_handler(State, Channel, From, Client) ->
    Channels = #server_state.channels,


    




% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    ServerAtom ! {stop, self(), 0},
    ok.
    % TODO Implement function
    % Return ok
    % not_implemented.
