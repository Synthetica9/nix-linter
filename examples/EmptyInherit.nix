{
  # Empty `inherit` statements are a no-op.
  bad = { inherit (x); inherit; };

  # Best is to remove them:
  good = {};
}
