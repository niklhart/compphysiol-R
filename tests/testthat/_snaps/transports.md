# Transport printing works correctly

    Code
      print(t1)
    Output
       Transports:
        (1) <all molec>: cen → per, amount rate = k1 * a[cen]
        (2) <all molec>: per → cen, amount rate = k2 * a[per]

---

    Code
      print(t2)
    Output
       Transports:
        (1) drug: cen → ∅, amount rate = k1 * a[cen]/(a[cen] + K)

