# Tic tac toe

Write a program that reads a **tic-tac-toe** board presented as a *list*
or a *tuple* of *size nine*. Return the winner (`x` or `o`) if a winner
has been determined, `cat` if there are no more possible moves,
or `no_winner` if no player has won yet.

Useful commands:
```
typer tic_tac_toe.erl
dialyzer --no_check_plt --src tic_tac_toe.erl

```