-module(server).
-export([start/1, stop/1, status/1, active_server/1]).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    genericserver:start(0, fun server_handler/3, ServerAtom, self()),
    receive
        {server_reqistry, Pid} ->
        register(ServerAtom, Pid),
        Pid
end.

stop(ServerAtom) -> genericserver:stop(ServerAtom).

status(ServerAtom) -> genericserver:request(ServerAtom, status).

active_server(ServerAtom) -> genericserver:request(ServerAtom, started).

%%% MAIN HANDLER (MAIN SWITCH %%%

server_handler(State, Request, ServerAtom) ->
    case Request of
        started -> active_server_handler(State, {ServerAtom, started});
        status -> status_handler(State, status)
end.

%%% HANDLERS %%%
status_handler(State, status) -> {reply, State, State}.

active_server_handler(State, started) -> {reply, State, started}.
