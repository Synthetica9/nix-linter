{
  # Sometimes (but not in all cases! nix-linter does not detect the difference
  # between these cases!), antiquoting can be redundant.
  bad = "${x}";

  # In these cases, one can remove the quotes and antiquotes:
  good = x;
}
