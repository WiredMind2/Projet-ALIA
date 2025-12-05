% File: run.prolog
% Purpose: This is the entry point script for the Connect 4 game. It consults (loads) all the necessary modules and defines the initial run predicate to start the game by calling hello.
% This allows the game to be launched by consulting run.prolog.
% Adapted from morpion.prolog: morpion.prolog is a single file; this version splits into modules for better organization, with run.prolog loading them. The run predicate is adapted to call hello from main.prolog instead of morpion's initialize.

% Consult (load) the module files
:- consult('board.prolog').
:- consult('game_logic.prolog').
:- consult('ai.prolog').
:- consult('main.prolog').
