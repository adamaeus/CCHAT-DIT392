-module(channel).
-export([start/1, channel_handler/2, client_already_in_channel_client_list/2]).

-record(channel_state, {
    name,
    clients
}).

initial_state(Channel) ->
    #channel_state{
        name = Channel,
        clients = []
    }.

start(Channel) ->
    State = initial_state(Channel),
    genserver:start(list_to_atom(Channel), State, fun channel_handler/2).

% the client handle is the input parameters to this handler
channel_handler(State, Request) ->
    case Request of
        {join, Nick, From} ->
            add_to_channel_client_list(State, Nick, From);
        {leave, From} ->
            removeFromChannel(State, From);
        {message_send, Msg, From, Nick} ->
             sendMessage(State, From, Msg, Nick)
    end.

removeFromChannel(State, From) ->
    NewChannelList = lists:delete(From, State#channel_state.clients),
   NewState = State#channel_state{clients = NewChannelList},
    {reply, ok, NewState}.

add_to_channel_client_list(State, Nick, From) ->
    %register(From, Nick),
    %User = {From, Nick},
    case lists:member(From, State#channel_state.clients) of
        true ->
            {reply, error_already_joined, "Client already joined", State};
        false ->
    NewChannelList = [From| State#channel_state.clients],
    NewState = State#channel_state{clients = NewChannelList},
    {reply, ok, NewState}
    end.

sendMessage(State, From, Msg, Nick) ->
        case lists:member(From, State#channel_state.clients) of
        % if client is a member of the channels client list
            true ->
        % remove the client from the client list to avoid showing message to self
        NewList = lists:delete(From, State#channel_state.clients),
        lists:foreach(
            fun(Member) ->
                %MemberPid = getFromValue(Member),
                Member ! {request, self(), make_ref(), {message_receive, State#channel_state.name, Nick, Msg}} end, NewList),
                {reply, ok, State};
            false ->
            {reply, {error, user_not_joined, "User is not in channel"}, State}
            end.


client_already_in_channel_client_list(State, From) ->
    ClientList = State#channel_state.clients,
    lists:member(From, ClientList).

