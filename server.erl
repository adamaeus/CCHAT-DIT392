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
        {join, From, Channel, Nick} ->
            join_server(State, Channel, From, Nick)
    end.

% Check to see if channel exist or not
% If exist -> join that channel
% If not exist -> Create and join
join_server(State, Channel, From, Nick) ->
    Channels = State#server_state.channels,
    case lists:member(Channel, Channels) of
        true ->
            Channels;
        false ->
            channel:start(Channel)
    end,

    ChannelResponse = genserver:request(list_to_atom(Channel), {join, Nick, From}),
    NewChannels = [Channel | Channels],
    UpdatedState = State#server_state{channels = NewChannels},
    {reply, ChannelResponse, UpdatedState}.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % Added
    genserver:stop(ServerAtom),
    ServerAtom ! {stop, self(), 0},
    ok.
