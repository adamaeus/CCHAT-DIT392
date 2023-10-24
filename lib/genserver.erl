-module(genserver).
-export([start/3, stop/1, request/2, request/3, update/2]).

% Spawn a process and register it with a given atom
% This is why list_to_atom function is used every time
% there is interaction with genserver via request function
% - Atom is the atom to register the process to
% - State is the initial state for the server loop
% - Function F is the body of the server:
%   - takes 2 params : state, request message
%   - returns a tuple: new state, response message
start(Atom, State, F) ->
  Pid = spawn(fun() -> loop(State, F) end),
  catch(unregister(Atom)),
  register(Atom, Pid),
  Pid.

stop(Atom) ->
  Atom!stop,
  catch(unregister(Atom)),
  ok.

% Remember that both server and channel module are genservers
% and follows the pattern in this event loop
% Every client handle request will follow the events in this loop
loop(State, F) ->
  receive
    % receives request from client
    {request, From, Ref, Data} ->
      % F is the client handle or handler functions in channel/server
      % catch exceptions on top of the "good" results
      case catch(F(State, Data)) of
        % if client handle request receives the wrong reply
        % an exit exception is cast where the reason tuple is defined in client
        {'EXIT', Reason} ->
          % sends the reason message to the process that initiated the request (gui)
          From!{exit, Ref, Reason},
          % recursively call to restart the loop, potentially with an updated state and a new handler F
          loop(State, F);
        % reply tuple of a successful request with response R
        {reply, R, NewState} ->
          % is sent to the process requesting it (gui)
          From!{result, Ref, R},
          % recursive call again
          loop(NewState, F)
        end;
    % receives this clause from update function
    {update, From, Ref, NewF} ->
      % genserver reply to process that requested the update
      From ! {ok, Ref},
      % loop restarted with the new handler
      loop(State, NewF);
    stop ->
      true
  end.

% Send a request to a Pid and wait for a response
request(Pid, Data) ->
  request(Pid, Data, 3000).

% Send a request to a Pid and wait for a response
% With a specified timeout.
% If Pid is an atom which is not registered: an "error:badarg" error is raised.
% If timeout expires: a "timeout_error" exception is thrown.
request(Pid, Data, Timeout) ->
  Ref = make_ref(),
  Pid ! {request, self(), Ref, Data},
  receive
    {result, Ref, Result} ->
      Result;
    {exit, Ref, Reason} ->
      exit(Reason)
  after Timeout ->
    throw(timeout_error)
  end.

% Update loop function
update(Pid, Fun) ->
  % Generates a unique reference for tracking the response.
  Ref = make_ref(),
  % Sends the update request message to the GenServer. (possibly client)
  Pid!{update, self(), Ref, Fun},
  receive
  % Waits for and acknowledges the successful update.
    {ok, Ref} ->
  % Returns 'ok' to confirm the update was received and applied.
      ok
  end.
