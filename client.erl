-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    % atom of the GUI process
    gui,
    % nick/username of the client
    nick,
    % atom of the chat server
    server,

    channels
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom,
        channels = []
    }.

%------------------------- FOR TESTS -----------------------------

channel_already_joined(St, Channel) ->
    ChannelList = St#client_st.channels,
    lists:member(Channel, ChannelList).

%----------------------------------------------------------------

handle(St, {join, Channel}) ->
    Server = St#client_st.server,

    Nickname = St#client_st.nick,

    case channel_already_joined(St, Channel) of
        true ->
            {reply, {error, user_already_joined, "Channel already joined"}, St};
        false ->
            genserver:request(Server, {join, self(), Channel, Nickname}),

            UpdatedChannelList = [Channel | St#client_st.channels],

            UpdatedState = St#client_st{channels = UpdatedChannelList},
            {reply, ok, UpdatedState}
    end;



% Leave channel
handle(St, {leave, Channel}) ->
    ChannelList = St#client_st.channels,
    case lists:member(Channel, ChannelList) of
        true ->
            genserver:request(list_to_atom(Channel), {leave, self()}),
            RemovedChannel = lists:delete(Channel, ChannelList),
            {reply, ok, St#client_st{channels = RemovedChannel}};
        false ->
            {reply, {error, user_not_joined, Channel}, St}
    end;


% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    % TODO: Implement this function
    ChannelList = St#client_st.channels,
    Nick = St#client_st.nick,
    
    case lists:member(Channel, ChannelList) of
        true ->
            genserver:request(list_to_atom(Channel), {message_send, Msg, self(), Nick}),
            {reply, ok, St};
        false ->
            {reply, {error, user_not_joined, Channel}, St}
    end;
% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    {reply, ok, St#client_st{nick = NewNick}};
% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St};
% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick ++ "> " ++ Msg}),
    {reply, ok, St};
% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St};
% Catch-all for any unhandled requests
handle(St, Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St}.
