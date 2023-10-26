# assign_colors

    Code
      assign_colors(mini_diamond, cut, colors = sci_colors("nejm", 8)) %>% dplyr::count(
        cut, assigned_colors)
    Output
      # A tibble: 3 x 3
        cut   assigned_colors     n
        <chr> <chr>           <int>
      1 Fair  #BC3C29FF          35
      2 Good  #0072B5FF          31
      3 Ideal #E18727FF          34

# assign_colors, fill na

    Code
      assign_colors(mini_diamond, clarity, colors = sci_colors("nejm", 3)) %>% dplyr::count(
        clarity, assigned_colors)
    Warning <simpleWarning>
      input colors are not enough, fill na items by #F5F5F5
    Output
      # A tibble: 8 x 3
        clarity assigned_colors     n
        <chr>   <chr>           <int>
      1 I1      #BC3C29FF          14
      2 IF      #0072B5FF          13
      3 SI1     #E18727FF          14
      4 SI2     #F5F5F5            12
      5 VS1     #F5F5F5            10
      6 VS2     #F5F5F5            11
      7 VVS1    #F5F5F5            14
      8 VVS2    #F5F5F5            12

