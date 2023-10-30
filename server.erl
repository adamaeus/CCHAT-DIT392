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


chat_handler(State, Data) ->
    case Data of
    % when Data pattern match to
        {join, From, Channel, Nick} ->
        % this code block will be executed
            join_channel(State, Channel, From, Nick);
        % when Data pattern match to
        {stop_channels} ->
        % this code block will be executed
    stop_associated_channels(State)
    end.

% Client request to join channel if the channel exists and if it doesn't
% the channel is started and client request to join.
join_channel(State, Channel, From, Nick) ->
    Channels = State#server_state.channels,
    case lists:member(list_to_atom(Channel), Channels) of
            true ->
                Channels;
            false ->
                channel:start(Channel)
    end,
    % client (From) request to join channel, sent to chat_handler via genserver
    ChannelRequest = genserver:request(list_to_atom(Channel), {join, Nick, From}),
    % add the channel to the list of channels
    NewChannels = [list_to_atom(Channel) | Channels],
    %Update state.
    UpdatedState = State#server_state{channels = NewChannels},
    {reply, ChannelRequest, UpdatedState}.

% Stops the server process, sends request to the genserver which will receive in
% chat_handler to stop all channels associated with the server, via the handleStop function.
stop(ServerAtom) ->
    genserver:request(ServerAtom, {stop_channels}),
    genserver:stop(ServerAtom).

% Handles the request to stop all active channels.
stop_associated_channels(State) ->
   Channels = State#server_state.channels,
    StopChannel = fun(Channel) -> genserver:stop(Channel) end,
    lists:foreach(StopChannel, Channels),
    {reply, ok, State}.