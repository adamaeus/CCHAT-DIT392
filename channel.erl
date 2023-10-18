-module(channel).
-export([start/1, channel_handler/2]).

-record(channel_state, {
                        name,
                        clients
                        }).

initial_state(Name) ->
    #channel_state{
            name = Name,
            clients = []
        }.


% list_to_atom is required for every genserver call.
start(Channel) ->
    State = initial_state(Channel),
    io:format("module channel, start function, print State ~p", [State]),
    genserver:start(list_to_atom(Channel), State , fun channel_handler/2).




channel_handler(State, Request) ->
    case Request of
        {join, Nick, From} -> addToChannel(State, Nick, From),
            {reply, user_added, State#channel_state.clients}
    end.

addToChannel(State, Nick, From) ->
    % register(Nick, From),
    % User = {Nick, From},
    NewChannelList = [From | State#channel_state.clients],
    State#channel_state{clients = NewChannelList}.
