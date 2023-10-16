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



start(Channel) ->
    State = initial_state(Channel),
    genserver:start(Channel, State , fun channel_handler/2).




channel_handler(State, Request) ->
    case Request of
        {join, Nick, From} -> addToChannel(Nick, From),
            {reply, user_added, #channel_state.clients}
    end.

addToChannel(Nick, From) ->
    register(Nick, From),
    User = {Nick, From},
    NewChannelList = [User | #channel_state.clients],
    #channel_state.clients = NewChannelList.
