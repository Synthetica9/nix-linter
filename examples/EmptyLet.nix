{
  # Empty `let` blocks are a no-op.
  bad = let in x;

  # Best is to remove them.
  good = x;
}
