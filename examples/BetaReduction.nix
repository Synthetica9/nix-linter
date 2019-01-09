{
  # Check whether a beta reduction can be done:
  bad = (x: x + x) (y + 1);

  # In the general case, this can be replaced with a `let` block:
  good = let x = y + 1; in x + x;
}
