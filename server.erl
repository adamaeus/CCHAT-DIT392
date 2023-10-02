-module(server).
-export([start/1, stop/1, status/1, test/1]).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    Pid = spawn(fun() -> server(0) end),
    register(ServerAtom, Pid),
    Pid.

% - Register this process to ServerAtom
% - Return the process ID
%not_implemented.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    ServerAtom ! {stop, self(), 0},
    ok.
% TODO Implement function
% Return ok
% not_implemented.


status(ServerAtom) ->
    Ref = make_ref(),
    ServerAtom ! {status, self(), Ref},
    receive
        {current_status, Ref, Result} -> Result
    end.

test(ServerAtom) ->
    Ref = make_ref(),
    ServerAtom ! {is_server_alive, self(), Ref},
    receive
        {response, Ref, Response} -> Response
    end.

server(N) ->
    receive
        {start, From, Ref} ->
            From ! started_server,
            server(N);
        {stop, From, Ref} ->
            From ! ok,
            server(N);
        {status, From, Ref} ->
            From ! {current_status, Ref, N},
            server(N);
        {is_server_alive, From, Ref} ->
            From ! {response, Ref, alive},
            server(N);
        _ ->
            undefined_message_call,
            server(N)
    end.
