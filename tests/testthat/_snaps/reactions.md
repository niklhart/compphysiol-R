# States require explicit molecule and compartment names

    Code
      print(s)
    Output
       States:
        (1) R[membrane]

# States support vectorized molecule-compartment pairs

    Code
      print(s)
    Output
       States:
        (1) R[membrane]
        (2) L[plasma]

# Reaction printing works correctly

    Code
      print(r1)
    Output
       Reactions:
        (1) A[cyt]+B[cyt] → C[cyt], rate = k * c[A, cyt] * c[B, cyt]

---

    Code
      print(r2)
    Output
       Reactions:
        (1) A → B (<all cmt>), rate = kAB * c[A]

---

    Code
      print(r3)
    Output
       Reactions:
        (1) A[plasma] → B[membrane], rate = kAB * c[A, plasma]

---

    Code
      print(r4)
    Output
       Reactions:
        (1) L[plasma]+R[membrane] → LR[membrane], scale = membrane, rate = kon * c[L, plasma] * c[R, membrane]

