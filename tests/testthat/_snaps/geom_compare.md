# extract_compare

    Code
      extract_compare(p)
    Output
      [[1]]
        PANEL x xend n1 n2     p plim psymbol       y1       y2       fc
      1     1 1    2 35 31 0.041 0.05       * 4995.057 3730.387 1.339018
      2     1 1    3 35 34 0.018 0.05       * 4995.057 3036.588 1.644957
        right_deno_fc left_deno_fc   fc1  fc2    label cp_step     y  yend group
      1          1.3x        0.75x 0.75x 1.3x *\n0.75x       0 20000 20000     1
      2          1.6x        0.61x 0.61x 1.6x *\n0.61x       1 20000 20000     1
      
      [[2]]
        PANEL x xend n1 n2     p plim psymbol       y1       y2       fc
      1     1 2    1 35 31 0.041 0.05       * 4995.057 3730.387 1.339018
      2     1 2    3 31 34  0.93 1.01      NS 3730.387 3036.588 1.228480
        right_deno_fc left_deno_fc   fc1   fc2     label cp_step     y  yend group
      1          1.3x        0.75x  1.3x 0.75x   *\n1.3x       0 22000 22000     1
      2          1.2x        0.81x 0.81x  1.2x NS\n0.81x       1 22000 22000     1
      
      [[3]]
        PANEL x xend n1 n2     p plim psymbol       y1       y2       fc
      1     1 3    1 35 34 0.018 0.05       * 4995.057 3036.588 1.644957
      2     1 3    2 31 34  0.93 1.01      NS 3730.387 3036.588 1.228480
        right_deno_fc left_deno_fc  fc1   fc2    label cp_step     y  yend group
      1          1.6x        0.61x 1.6x 0.61x  *\n1.6x       0 24000 24000     1
      2          1.2x        0.81x 1.2x 0.81x NS\n1.2x       1 24000 24000     1
      

