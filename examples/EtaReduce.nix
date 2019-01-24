{
  # When you have a function abstraction, only to immediately apply the
  # argument to a function, this is called an η-abstraction (or eta-abstraction)
  # See also: https://wiki.haskell.org/Eta_conversion
  bad = x: f x;

  # Generally, it nicer to be direct:
  good = f;
}
