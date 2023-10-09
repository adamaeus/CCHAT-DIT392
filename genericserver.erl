-module(genericserver).
-export([start/3, stop/1, request/2]).

%% Added "Server" as parameter for the server function.
start(State, Handler, From) ->
    Pid = spawn( fun() -> server(State, Handler)end),
    From ! Pid.


stop(Server) ->
    Server ! {stop, self, 0},
    ok.

%% Added parameter for the server function.
server(State, Handler) ->
    receive
        {request, From, Ref, Request} ->
            %% Also added server here for the handler.
            case Handler(State, Request) of
                {reply, NewState, Result} ->
                    From ! {response, Ref, Result},
                    %% Added server for the recall of server function.
                    server(NewState, Handler)
            end;
        {stop, _From, _Ref} -> ok
    end.



request(Server, Request) ->
    Ref = make_ref(),
    Server ! {request, self(), Ref, Request},
    receive
{response, Ref, Result} -> Result end.