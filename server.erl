-module(server).
-export([start/1, stop/1, chat_handler/2]).

-record(server_state, {
    channels
}).
% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state() ->
    #server_state{
        channels = []
    }.
% Logic server
% Parameters match the generic server start function
% Start a new server process with the given name
% Do not change the signature of this function.
% ServerAtom is "shire" in our case.
start(ServerAtom) ->
    State = initial_state(),
    genserver:start(ServerAtom, State, fun chat_handler/2).

%   - takes 2 params : state, request message
%   - returns a tuple: new state, response message
%   - handler reply to genserver
chat_handler(State, Data) ->
    case Data of
    % when Data pattern match to
        {join, From, Channel, Nick} ->
        % this code block will be executed
            join_channel(State, Channel, From, Nick);
        % when Data pattern match to
        {stopChannels} ->
        % this code block will be executed
            handleStop(State)
    end.

% Client request to join channel if the channel exists and if it doesn't
% the channel is started and client reques to join.
join_channel(State, Channel, From, Nick) ->
    % current channels
    Channels = State#server_state.channels,

% Upptäckte ett nytt fel. Vi sparar inte channels som atoms utan som strings.
% detta blir problem då vi joinar och skickar meddelande. Dock tar det för lång
% tid att ändra det nu. 
    case lists:member(list_to_atom(Channel), Channels) of
        % if channel is in the channel list
            true ->
            % return channels
                Channels;
            % if channel does not exist in list
            false ->
                % start the channel
                channel:start(Channel)
    end,
    % client (From) request to join channel, sent to channel_handler via genserver
    ChannelRequest = genserver:request(list_to_atom(Channel), {join, Nick, From}),
    NewChannels = [list_to_atom(Channel) | Channels],
    UpdatedState = State#server_state{channels = NewChannels},
    {reply, ChannelRequest, UpdatedState}.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    genserver:request(ServerAtom, {stopChannels}),
    genserver:stop(ServerAtom).
% Handles the request to stop all active channels.
handleStop(State) ->
    %ListToAtom = fun(Channel) -> list_to_atom(Channel) end,
    %Channels = lists:foreach(ListToAtom, State#server_state.channels),
   % Channels2 = State#server_state.channels,
   Channels = State#server_state.channels,
    StopChannel = fun(C) -> genserver:stop(C) end,
    lists:foreach(StopChannel, Channels),
    {reply, ok, State}.