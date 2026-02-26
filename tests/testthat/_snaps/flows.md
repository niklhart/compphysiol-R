# Flow printing works correctly

    Code
      print(f1)
    Output
       Flows:
        (1) A → C, rate = k1 * A
        (2) B → D, rate = k2 * B

---

    Code
      print(f2)
    Output
       Flows:
        (1) A → ∅, rate = k1 * A/(A + K)

