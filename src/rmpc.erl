%%% ------------------------------------------------------------------------------------------------
%%% "rmpc" is available for use under the following license, commonly known as the 3-clause
%%% (or "modified") BSD license:
%%%
%%% Copyright (c) 2018-2019, Pouriya Jahanbakhsh
%%% (pouriya.jahanbakhsh@gmail.com)
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without modification, are permitted
%%% provided that the following conditions are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright notice, this list of
%%%    conditions and the following disclaimer.
%%%
%%% 2. Redistributions in binary form must reproduce the above copyright notice, this list of
%%%    conditions and the following disclaimer in the documentation and/or other materials provided
%%%    with the distribution.
%%%
%%% 3. Neither the name of the copyright holder nor the names of its contributors may be used to
%%%    endorse or promote products derived from this software without specific prior written
%%%    permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
%%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
%%% FITNESS FOR A  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.
%%% ------------------------------------------------------------------------------------------------
%% @author   Pouriya Jahanbakhsh <pouriya.jahanbakhsh@gmail.com>
%% @version  18.5.9
%% @doc
%%           Remote Multi Procedure Call.
%% @end
%% -------------------------------------------------------------------------------------------------
-module(rmpc).
-author("pouriya.jahanbakhsh@gmail.com").
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% API:
-export([start/2
        ,start_link/2
        ,execute/4]).

%% 'gen_server':
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3]).

%% Compiler:
-export([parse_transform/2]).

%% Internal:
-export([start/3]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-define(S, state).
-record(?S, {}).

-define(EXECUTE_TAG, execute).
-define(DEF_HIBERNATE_TIMEOUT, 7000).

%% -------------------------------------------------------------------------------------------------
%% API:

-spec
start_link(node(), atom()) ->
    {'ok', pid()} | {'error', term()}.
%% @doc
%%      Starts and links an RMPC process and registers it locally on Node.<br/>
%%      If Node is not same node, this call will load this module code on Node.
%% @end
start_link(Node, Name) when erlang:is_atom(Node) andalso erlang:is_atom(Name) ->
    start(Node, Name, link).


-spec
start(node(), atom()) ->
    {'ok', pid()} | {'error', term()}.
%% @doc
%%      Starts an RMPC process and registers it locally on Node.<br/>
%%      If Node is not same node, this call will load this module code on Node.
%% @end
start(Node, Name) when erlang:is_atom(Node) andalso erlang:is_atom(Name) ->
    start(Node, Name, nolink).


-spec
execute(node(), atom(), module(), atom()) ->
    term() | {'error', term()}.
%% @doc
%%      Executes parse-transformed function Func from module Mod on node Node.
%% @end
execute(Node, Name, Mod, Func) when erlang:is_atom(Node) andalso
                                    erlang:is_atom(Name) andalso
                                    erlang:is_atom(Mod) andalso
                                    erlang:is_atom(Func) ->
    try erlang:binary_to_term(Mod:Func()) of
        Exprs ->
            case rpc:server_call(Node, Name, ?EXECUTE_TAG, {?EXECUTE_TAG, Exprs}) of
                {badrpc, Rsn} ->
                    {error, {rpc, [{reason, Rsn}
                                  ,{node, Node}
                                  ,{name, Name}
                                  ,{module, Mod}
                                  ,{function, Func}]}};
                Other ->
                    Other
            end
    catch
        _:Rsn ->
            {error, {crash, [{reason, Rsn}
                            ,{stacktrace, erlang:get_stacktrace()}
                            ,{node, Node}
                            ,{name, Name}
                            ,{module, Mod}
                            ,{function, Func}]}}
    end.

%% -------------------------------------------------------------------------------------------------
%% 'gen_server':

%% @hidden
init(_) ->
    {ok, #?S{}, ?DEF_HIBERNATE_TIMEOUT}.


%% @hidden
handle_call(Req, _, S) ->
    {reply, {error, {unknown, [{request, Req}]}}, S, ?DEF_HIBERNATE_TIMEOUT}.


%% @hidden
handle_cast(_, S) ->
    {noreply, S, ?DEF_HIBERNATE_TIMEOUT}.


%% @hidden
handle_info({From, {?EXECUTE_TAG, Exprs}}, S) ->
    From ! {?EXECUTE_TAG, erlang:node(), execute(Exprs)},
    {noreply, S, ?DEF_HIBERNATE_TIMEOUT};
handle_info(timeout, S) ->
    {noreply, S, hibernate};
handle_info(_, S) ->
    {noreply, S, ?DEF_HIBERNATE_TIMEOUT}.


%% @hidden
terminate(_, _) ->
    ok.


%% @hidden
code_change(_, S, _) ->
    {ok, S}.

%% -------------------------------------------------------------------------------------------------
%% Compiler:

%% @hidden
parse_transform(AST, _) ->
    transform(AST, []).

%% -------------------------------------------------------------------------------------------------
%% Internal exported:

%% @hidden
start(Node, Name, LinkType) ->
    Function =
        if
            LinkType == link ->
                start_link;
            true -> % nolink
                start
        end,
    if
        Node == erlang:node() ->
            gen_server:Function({local, Name}
                               ,?MODULE
                               ,undefined
                               ,[{spawn_opt, [{message_queue_data, off_heap}]}]);
        true ->
            {_, Binary, Filename} = code:get_object_code(?MODULE),
            case rpc:call(Node, code, load_binary, [?MODULE, Filename, Binary]) of
                {module, _} ->
                    case rpc:call(Node, ?MODULE, start, [Node, Name, LinkType]) of
                        {badrpc, Rsn} ->
                            {error, {rpc, [{reason, Rsn}, {node, Node}, {name, Name}]}};
                        Other -> % {ok, _} | {error, _}
                            Other
                    end;
                {badrpc, Rsn} ->
                    {error, {rpc, [{reason, Rsn}, {node, Node}, {name, Name}]}};
                error ->
                    {error, {load, [{node, Node}, {name, Name}]}}
            end
    end.

%% -------------------------------------------------------------------------------------------------
%% Internal:

execute(Exprs) ->
    try erl_eval:exprs(Exprs, erl_eval:new_bindings()) of
        {value, Val, _} ->
            Val
    catch
        _:Rsn ->
            {error, {crash, [{reason, Rsn}, {stacktrace, erlang:get_stacktrace()}]}}
    end.


transform([Form|AST], NewAST) ->
    NewForm =
        case erl_syntax:type(Form) of
            function ->
                case validate_function(Form) of
                    {ok, {Name, Body, Line}} ->
                        make_function(Name, Body, Line);
                    _ -> % false
                        Form
                end;
            _ ->
                Form
        end,
    transform(AST, [NewForm|NewAST]);
transform(_, NewAST) -> % ([], NewAST)
    lists:reverse(NewAST).


validate_function(Func) ->
    {Name, Arity} = erl_syntax_lib:analyze_function(Func),
    case {lists:reverse(erlang:atom_to_list(Name)), Arity} of
        {[$c, $p, $m, $r, $_, _ | _], 0} -> % function name: *_rmpc/0
            % So function has one clause
            {ok, {Name
                 ,erl_syntax:clause_body(erlang:hd(erl_syntax:function_clauses(Func)))
                 ,erl_syntax:get_pos(Func)}};
        _ ->
            error
    end.


make_function(Name, Body, Line) ->
    BodyBinary = erlang:binary_to_list(erlang:term_to_binary(Body)),
    BodyAST = erl_syntax:binary([erl_syntax:binary_field(erl_syntax:string(BodyBinary))]),
    FunctionClause = erl_syntax:revert(erl_syntax:clause([], [], [BodyAST])),
    FunctionName = erl_syntax:revert(erl_syntax:set_pos(erl_syntax:atom(Name), Line)),
    erl_syntax:revert(erl_syntax:function(FunctionName, [FunctionClause])).