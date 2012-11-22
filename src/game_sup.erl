-module(game_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-type strategy() :: one_for_all | one_for_one | rest_for_one | simple_one_for_one.

%% @child_spec(). See http://www.erlang.org/doc/man/supervisor.html#type-child_spec
-type child_spec() :: {child_id(),
     mfargs(),
     restart(),
     shutdown(),
     worker(),
     modules()}.

-type child_id() :: term().
-type restart()  :: permanent | transient | temporary.
-type mfargs()   :: {module(),atom(),[term()] | undefined}.
-type shutdown() :: brutal_kill | timeout().
-type worker()   :: worker | supervisor.
-type modules()  :: [module()] | dynamic.


%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> {error,_} | {ok,pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

-spec init([]) -> {ok,{{strategy(),non_neg_integer(),non_neg_integer()},[child_spec()]}}.
init([]) ->
    Childs = [
        ?CHILD(game, worker)
    ],
    {ok, {{one_for_one, 5, 10}, Childs}}.

