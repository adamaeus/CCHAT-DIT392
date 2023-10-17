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
    io:fwrite("~p ~n", [inside_channel_start]),
    io:fwrite("~p ~n", [Channel]),
    State = initial_state(Channel),
    io:fwrite("~p ~n", [after_STATE_inside_channel_start]),
    io:fwrite("~p ~n", [State]),
    genserver:start(list_to_atom(Channel), State, fun channel_handler/2),
    io:fwrite("~p ~n", [after_genserver_start_in_channel]),
    io:fwrite("~p ~n", [list_to_atom(Channel)]).
    

channel_handler(State, Request) ->
    case Request of
        {join, Nick, From} ->
            addToChannel(State, Nick, From),
            {reply, user_added, #channel_state.clients}
    end.



addToChannel(State, Nick, From) ->
    %register(Nick, From),
    %User = {Nick, From},
    NewChannelList = [From | State#channel_state.clients],
    State#channel_state{clients = NewChannelList}.
