{
  # The binindgs here don't use the arguments that are abstracted:
  bad = x : let y = z; in x + y;

  # We can pull them out of said abstraction:
  good = let y = z; in x : x + y;
}
