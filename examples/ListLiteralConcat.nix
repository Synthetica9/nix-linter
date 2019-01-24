{
  # When concatenating list literals:
  bad = [ a ] ++ [ b ];

  # Just write the concatenated list instead!
  good = [ a b ];

  note = {
    # Note: the following can also raise this warning:
    bad = with x; [ a ] ++ [ b ];

    # This is because the `with` is parsed to affect the scope of both lists!
    # To silence this warning in this case, use parentheses:
    good = (with x; [ a ]) ++ [ b ];
  };
}
