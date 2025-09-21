%% Module for handling sequence literal AST nodes
%% This module centralizes all sequence-related logic for the Elixir compiler
-module(elixir_literal).

-export([is_raw_node/1, is_valid_ast/1, quote_node/2, escape_node/2, expand/3]).

%% Check if a node is a sequence node
is_raw_node({raw_block, _Meta, _BracketType, _Args}) -> true;
is_raw_node({raw_section, _Meta, _Args}) -> true;
is_raw_node(_) -> false.

%% Validate sequence AST nodes
%% For raw_block (4-tuple format), validate the inner args
is_valid_ast({raw_block, Meta, BracketType, Args})
  when is_list(Meta), is_atom(BracketType), is_list(Args) ->
    % For raw_block, we assume the args are valid since they will be processed by Paxir
    true;
%% For other sequence nodes (3-tuple format), they're always valid
is_valid_ast({raw_section, Meta, Args})
  when is_list(Meta), is_list(Args) -> true;
is_valid_ast(_) -> false.

%% Quote sequence nodes - keep them as-is without transforming internals
quote_node({raw_block, _Meta, _BracketType, _Args} = Node, _Q) -> Node;
quote_node({raw_section, _Meta, _Args} = Node, _Q) -> Node;
quote_node(_, _) -> false.

%% Escape sequence nodes - keep them as-is without transforming internals
escape_node({raw_block, _Meta, _BracketType, _Args} = Node, _Q) -> Node;
escape_node({raw_section, _Meta, _Args} = Node, _Q) -> Node;
escape_node(_, _) -> false.

%% Expand sequence nodes - keep them as-is since they will be processed by Paxir
expand({raw_block, _Meta, _BracketType, _Args} = Node, S, E) -> {Node, S, E};
expand({raw_section, _Meta, _Args} = Node, S, E) -> {Node, S, E};
expand(_, _, _) -> false.
