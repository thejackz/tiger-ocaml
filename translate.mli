open Tree

type level

type access

type exp

type new_level_arg = {
  parent  : level;
  name    : Temp.label;
  formals : bool list;
}

val outermost : level

val new_level : new_level_arg -> level

val formals : level -> access list

val alloc_local : level -> bool -> access


