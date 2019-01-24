{
  # Instead of merging two set literals with //:
  bad = { a = 1; } // { b = 2; };

  # Consider writing the result instead:
  good = { a = 1; } // { b = 2; };
}
