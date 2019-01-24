{
  # In some cases, changing the name used in an abstraction can make use of an
  # `inherit` possible:
  bad = x: { y = x; };

  # Change `x` to `y` to enable this:
  good = y: { inherit y; };
}
