{
  # This doesn't need to split into multiple blocks:
  bad =
    let
      x = 1;
    in
    let
      y = 2;
    in
      { z = x + y; };

  # Merge those blocks instead:
  good =
    let
      x = 1;
      y = 2;
    in
      { z = x + y; };
}
