-module(server).
-export([start/1,stop/1,status/1]).

%%% Channels %%%
% This is the datatype for a channel. Each channel will have a name
% and a list of users.
% Important to note here is that a server can have many channels, hence the list.
-record(server_state, {
    channels = []
}).

% Data type for channel, which have a name "shire" and users "Dildo Faggins, Glamwise Saggy "...
-record(channel, {
    name,
    users = []
}).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    genserver:start(ServerAtom, 0, fun server_handler/2).

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) -> 
    genserver:stop(ServerAtom).

% Calls for the state of the server.
status(ServerAtom) -> 
    genserver:request(ServerAtom, status).

%%% Main Module Handler %%%
server_handler(State, Request) ->
    case Request of
        status -> status_handler(State, status);
        {join, Channel, Nickname} -> join_handler(State, Channel, Nickname)
end.


%%% Handlers %%%
status_handler(State, status) -> {reply, State, State}.

join_handler(State, Channel, Nickname) ->
    % If the channel does not exist, create one. 
    NewState = check_channel_existing(State, Channel),
    % Add a user to the channel.
    UpdatedState = add_user_to_channel(NewState, Channel, Nickname),
    % Return updated state. This message is received by the 
    % request in genserver.
    {reply, ok, UpdatedState}.


%%% CHANNEL MANAGEMENT %%%

    % Denna funktionen kollar om en kanal redan finns i listan av "server.state.channels"
    % Om true, då returneras State
    % Om false, då skapar man en ny channel.

    check_channel_existing(State, Channel) ->

        % lists:keyfind är en inbyggd Erlang metod (från lib/package lists) som kan
        % söka en lista efter en input. Vi söker alltså i State#server_state.channels efter
        % #channel.name som matchar Channel.

        case lists:keyfind(Channel, #channel.name, State#server_state.channels )of
            {Channel, _Users} ->
                State;
            false -> create_new_channel(Channel, State)
end.


% Skapar en ny channel. Vi döper den initialt till NewChannel, vi
% skriver värdet "name" till Channel (alltdå den kanal vi söker efter från början via genserver)
% Listan med users initieras till 0 (vi adderar Nickname/user sen)

% Fortfarande work in progress på vad "NewChannels" faktiskt innebär. (chatgpt).
create_new_channel(Channel, State) ->
    NewChannel = #channel{name = Channel, users = []},
    NewChannels = [NewChannel | State#server_state.channels],
    NewState = State#server_state{channels = NewChannels},
    NewState.


add_user_to_channel(State, Channel, Nickname) ->
    
    % Extrahera alla kanaler från server listan. Kallar dem
    % temporärt för "Channels"
    ServerChannels = State#server_state.channels,

    % När vi lagt till user i en channel sparar vi denna som "Updated Channel"
    UpdatedChannels = add_user(ServerChannels, Channel, Nickname),

    % Skriver över befintlig lista av channels med channel
    % där vi precis lade till en användare.
    State#server_state{channels = UpdatedChannels}.

    % Inte säkert på om detta är rätt väg att gå...

add_user([], _Channel, _Nickname) -> [];
% add_user([Channel])