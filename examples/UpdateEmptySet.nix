
{
  # Don't update an empty set:
  bad = { } // { x = 1; };

  # Use the set you are updating with instead:
  good = { x = 1; };
}
