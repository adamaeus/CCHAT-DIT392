-module(server).
%11:43 debugging. Added export server_handler
-export([start/1, stop/1, status/1, server_handler/2]).

-record(server_state, {
    channels = []
}).
%%% Channels %%%
% This is the datatype for a channel. Each channel will have a name
% and a list of users.
% Important to note here is that a server can have many channels, hence the list.

% Data type for channel, which have a name "shire" and users "Dildo Faggins, Glamwise Saggy "...
-record(channel, {
    name,
    users = []
}).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    genserver:start(ServerAtom, #server_state.channels, fun server_handler/2).

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    genserver:stop(ServerAtom).

% Calls for the state of the server.
status(ServerAtom) ->
    genserver:request(ServerAtom, status).

%%% Main Module Handler %%%
server_handler(State, Data) ->
    case Data of
        status -> status_handler(State, status);
        {join, Channel, Nickname} -> join_handler(State, Channel, Nickname)
    end.

%%% Handlers %%%
status_handler(State, status) -> {reply, State, State}.

join_handler(State, Channel, Nickname) ->
    
    % Om metoden "check_channel_existing" returnerar true, alltså att kanalen finns,
    % då returneras "State" som här blir "NewState". Sedan går vi vidare och lägger till user i kanalen.
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

check_channel_existing(Channel, State) ->
    ChannelList = State#server_state.channels,
    find_in_channels(Channel, ChannelList, State).


find_in_channels(Channel, [], State) ->
    create_new_channel(Channel, State);
find_in_channels(Channel, [ChName | RestChannels], State) when ChName == Channel ->
    State;
find_in_channels(Channel, [_OtherChannel | RestChannels], State) ->
    find_in_channels(Channel, RestChannels, State).


% Skapar en ny channel. Vi döper den initialt till NewChannel, vi
% skriver värdet "name" till Channel (alltdå den kanal vi söker efter från början via genserver,
% exempelvis sökte vi kanske på "Quake", finns den inte, skapar vi ny channel med namnet Quake)
% Listan med users initieras till 0 (vi adderar Nickname/user sen)

create_new_channel(Channel, State) ->
    %Skapar en ny kanal. Listan är tom då en ny kanal ej har users (den är ju ny).
    NewChannel = #channel{name = Channel, users = []},
    %Vi skapar här en ny lista av channels. Detta ser man då vi bygger en lista, vi stoppar in den nya kanalen
    % (NewChannel) först i listan, och resterande element i listan hämtas från listan av channels i current state.
    NewChannels = [NewChannel | State#server_state.channels],
    % Vi skapar här ett nytt State, och skriver över current lista av channels mede den nya listan
    %(den uppdaterade listan, där vi inkluderat den nyskapade kanalen).
    NewState = State#server_state{channels = NewChannels},
    % Returnerar det nya State.
    NewState.

%% I denna metoden
add_user_to_channel(State, Channel, Nickname) ->
    % Extrahera alla kanaler från server listan. Kallar dem
    % temporärt för "ServerChannels"
    ServerChannels = State#server_state.channels,

    % När vi lagt till user i en channel sparar vi denna som "Updated Channel"
    UpdatedChannels = add_user(ServerChannels, Channel, Nickname),

    % Skriver över befintlig lista av channels med channel
    % där vi precis lade till en användare.
    State#server_state{channels = UpdatedChannels}.

% Inte säkert på om detta är rätt väg att gå...

add_user([], _Channel, _Nickname) ->
    [];
add_user([SC | SCs], Channel, Nickname) when
    Channel == SC
->
    UpdatedSC = SC#channel{users = [Nickname | SC#channel.users]},
    [UpdatedSC | SCs];
add_user([SC | SCs], Channel, Nickname) when
    Channel =/= SC
->
    [SC | add_user(SCs, Channel, Nickname)].
