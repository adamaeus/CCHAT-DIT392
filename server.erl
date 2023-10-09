-module(server).
-export([start/1,stop/1,status/1]).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    genserver:start(ServerAtom, 0, fun server_handler/2).

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) -> 
    genserver:stop(ServerAtom).

% Calls for the state of the server.
status(ServerAtom) -> genserver:request(ServerAtom, status).

%%% Main Module Handler %%%
server_handler(State, Request) ->
    case Request of
        status -> status_handler(State, status)
end.


%%% Handlers %%%
status_handler(State, status) -> {reply, State, State}.
