{
  # A let binding that isn't used can be left out:
  bad = let x = 1; in y;
  good = y;
}
