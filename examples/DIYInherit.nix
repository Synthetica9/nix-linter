{
  # Checks for so called "DIY Inherit" (binding a variable to the same name):
  bad = { x = x; };

  # This can be done better with `inherit`:
  good = { inherit x; };
}
