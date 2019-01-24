{
  # Instead of using a `let` block here, we can directly specify this within the
  # recursive set.
  bad =
    let
      x = 5;
    in rec {
      inherit x;
    };

  # This can be done as follows:
  good =
    rec {
      x = 5;
    };

  # Note that this doesn't need to work for non-recursive sets.
}
