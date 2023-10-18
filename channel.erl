-module(channel).
-export([start/1, channel_handler/2]).

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

channel_handler(State, Request) ->
    case Request of
        {join, Nick, From} -> addToChannel(State, Nick, From),
            {reply, user_added, State#channel_state.clients} %Added State
    end.



addToChannel(State, Nick, From) ->
    %register(Nick, From),
    %User = {Nick, From},
    NewChannelList = [From | State#channel_state.clients],
    State#channel_state{clients = NewChannelList}.
