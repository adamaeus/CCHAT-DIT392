-module(channel).
-export().

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
