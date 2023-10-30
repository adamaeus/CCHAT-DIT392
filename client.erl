-module(client).
-export([handle/2, initial_state/3]).

-import(channel, [client_already_in_channel_client_list/2]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    % atom of the GUI process
    gui,
    % nick/username of the client
    nick,
    % atom of the chat server
    server,
    % atom of the channels a client has joined
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

% Boolean function that checks if channel is already in the channel list
channel_already_in_client_channel_list(St, Channel) ->
    % current channels saved in variable
    ChannelList = St#client_st.channels,
    % check if channel is a member of current channel list
    lists:member(Channel, ChannelList).

%----------------------------------------------------------------
% Needs to be changed so that client_already_in_channel_client_list

% Join channel
handle(St, {join, Channel}) ->
    % variable assigned to the atom of the server
    Server = St#client_st.server,
    % variable assigned to the nick/username atom of the client
    Nickname = St#client_st.nick,
    % pattern matching
    case channel_already_in_client_channel_list(St, Channel) of
        true ->
            % client handle sends this reply to gui
            {reply, {error, user_already_joined, "Channel already joined"}, St};
        % if channel is not in the client channel list
        false ->
            % client send request to genserver to join channel via chat_handler in server module
            case catch genserver:request(Server, {join, self(), Channel, Nickname}) of
                {'EXIT', _} ->
                    {reply, {error, server_not_reached, "Unable to join server"}, St};
                timeout_error ->
                    {reply, {error, server_not_reached, "Unable to join server"}, St};
                ok ->
            % channel is added to the clients channel list and saved in a variable
            UpdatedChannelList = [Channel | St#client_st.channels],
            % state is updated/overwritten with the updated list
            UpdatedState = St#client_st{channels = UpdatedChannelList},
            % client handle sends reply with updated state to gui
            {reply, ok, UpdatedState}
                        end
    end;


% Leave channel
handle(St, {leave, Channel}) ->
    % current channels saved in variable
    ChannelList = St#client_st.channels,
    % pattern matching
    case channel_already_in_client_channel_list(St, Channel) of
        true ->
            % requesting genserver to leave channel via channel_handler in channel module
            genserver:request(list_to_atom(Channel), {leave, self()}),
            % list with the channel removed saved in variable
            RemovedChannel = lists:delete(Channel, ChannelList),
            % client handle sends reply with updated state to gui
            {reply, ok, St#client_st{channels = RemovedChannel}};
        % if channel is not a member of channel list
        false ->
            % client handle sends reply to gui with current state
            {reply, {error, user_not_joined, Channel}, St}
    end;

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    % nickname saved in variable
    Nick = St#client_st.nick,

 case whereis(list_to_atom(Channel)) of
        undefined ->
            {reply, {error, server_not_reached, "The server is not reachable"}, St};
   ServerAtom ->
            case catch genserver:request(ServerAtom, {message_send, Msg, self(), Nick}) of
                    timeout_error ->
                        Text = "Recipient does not respond.",
                        Reply = {error, server_not_reached, Text},
                        {reply, Reply, St};
                    Response ->
                        {reply, Response, St}
                end
   end;

% Kvarstående fel att lösa: Clienten kan skriva till kanalen även fast servern är död
% Det ska den inte kunna göra!


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
