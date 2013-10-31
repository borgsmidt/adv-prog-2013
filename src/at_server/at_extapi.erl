%%%-------------------------------------------------------------------
%%% @author Michael Kirkedal Thomsen <shapper@diku.dk>
%%% @copyright (C) 2013, Michael Kirkedal Thomsen
%%% @doc
%%% Skeleton for AP Exam 2013.
%%% Implementation of the atomic transaction server
%%% @end
%%% Created : Oct 2013 by Michael Kirkedal Thomsen <shapper@diku.dk>
%%%-------------------------------------------------------------------
%%% Student name:
%%% Student KU-id:
%%%-------------------------------------------------------------------

-module(at_extapi).

-export([abort/2, tryUpdate/2, ensureUpdate/2, choiceUpdate/3]).

%%%-------------------------------------------------------------------
%%% Extended API
%%%-------------------------------------------------------------------

abort(AT, Ref) -> put_your_code.

tryUpdate(AT, Fun) -> put_your_code.

ensureUpdate(AT, Fun) ->put_your_code.

choiceUpdate(AT, Fun, Val_list) -> put_your_code.

%%%-------------------------------------------------------------------
%%% Communication primitives
%%%-------------------------------------------------------------------

% Copy what you need from at_server or create you own.