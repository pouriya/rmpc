# RMPC (Remote Multi Procedure Call)
An Erlang library for calling multiple `M:F/A` of other nodes in **one** request.

# Example
Suppose i have node `x` +200K processes:
```sh
~/rmpc $ erl -sname x
Erlang/OTP 19 [erts-8.0] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]
Eshell V8.0  (abort with ^G)
```
```erlang
%% Spawning 200K processes:
(x@codefather)1> lists:foreach(fun(_) -> spawn(fun() -> receive _ -> ok end end) end, lists:seq(1, 200000)).
ok

%% A filter function for giving process with memory >= 10KB:
(x@codefather)2> Filter = fun(Pid) -> case erlang:process_info(Pid, memory) of {_, Int} when Int > 10240 -> true; _ -> false end end.   
#Fun<erl_eval.6.52032458>

(x@codefather)3> lists:filter(Filter, processes()).
[<0.4.0>,<0.31.0>,<0.36.0>,<0.44.0>,<0.46.0>,<0.55.0>,
 <0.57.0>,<0.58.0>,<0.64.0>]

%% Giving time:
(x@codefather)4> timer:tc(lists, filter, [Filter, processes()]).
{412862, %% About 0.4s
 [<0.4.0>,<0.31.0>,<0.36.0>,<0.44.0>,<0.46.0>,<0.55.0>,
  <0.57.0>,<0.58.0>,<0.64.0>]}
```

I want to do this on  node `x` from node `y` using Erlang/OTP `rpc` module:
```sh
~/rmpc $ erl -pa _build/default/lib/rmpc/ebin/ -sname y
Erlang/OTP 19 [erts-8.0] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]
Eshell V8.0  (abort with ^G)
```
```erlang
%% Use rpc module:
(y@codefather)1> RPC = fun(Node) ->
(y@codefather)1>  Filter = fun(Pid) -> case rpc:call(Node, erlang, process_info, [Pid, memory]) of {_, Int} when Int > 10240 -> true; _ -> false end end,
(y@codefather)1>  lists:filter(Filter, rpc:call(Node, erlang, processes, []))
(y@codefather)1> end.
#Fun<erl_eval.6.52032458>

%% Giving time:
(y@codefather)2> timer:tc(RPC, [x@codefather]).
{12714994, %% About 13s !
 [<7444.4.0>,<7444.31.0>,<7444.36.0>,<7444.40.0>,<7444.44.0>,
  <7444.46.0>,<7444.48.0>,<7444.55.0>,<7444.57.0>,<7444.58.0>,
  <7444.64.0>]}
```

Finally i want to use our `rmpc` module. First we need to package our code in a module:
```erlang
-module(monitoring).
-export([process_memory_rmpc/0]).

%% Every function with arity 0 which its name ends with _rmpc has to transformed.
%% So you need rmpc header file included.
-include("include/rmpc.hrl").

process_memory_rmpc() ->
    Filter =
        fun(Pid) ->
            case erlang:process_info(Pid, memory) of
                {_, Int} when Int > 10240 ->
                    true;
                _ ->
                    false
            end
        end,
    lists:filter(Filter, processes()).
```

Back to the shell `y`:
```erlang
%% We need to start an RMCP process on that node, I call it monitoring_agent
%% Note that we dont need to have this library beam files there, RMCP will load its binary object code there.
(y@codefather)3> rmpc:start(x@codefather, monitoring_agent).
{ok,<7444.17184.30>}

%% Compiling the code:
(y@codefather)4> c(monitoring).                             
{ok,monitoring}

(y@codefather)5> rmpc:execute(x@codefather, monitoring_agent, monitoring, process_memory_rmpc).
[<7444.4.0>,<7444.31.0>,<7444.36.0>,<7444.40.0>,<7444.44.0>,
 <7444.46.0>,<7444.48.0>,<7444.55.0>,<7444.57.0>,<7444.58.0>,
 <7444.64.0>,<7444.17184.30>]
 
 %% Giving time:
(y@codefather)6> timer:tc(rmpc, execute, [x@codefather, monitoring_agent, monitoring,process_memory_rmpc]).
{417276, %% About 0.4s !
 [<7444.4.0>,<7444.31.0>,<7444.36.0>,<7444.40.0>,<7444.44.0>,
  <7444.46.0>,<7444.48.0>,<7444.55.0>,<7444.57.0>,<7444.58.0>,
  <7444.64.0>,<7444.17184.30>]}
```

### License
`BSD 3-Clause`

### Author
`pouriya.jahanbakhsh@gmail.com`

### Hex version
[]()
