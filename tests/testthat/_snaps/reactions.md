# States require explicit molecule and compartment names

    Code
      print(s)
    Output
       compphysiol_states:
        (1) R[membrane]

# States support vectorized molecule-compartment pairs

    Code
      print(s)
    Output
       compphysiol_states:
        (1) R[membrane]
        (2) L[plasma]

# Reaction printing works correctly

    Code
      print(r1)
    Output
       compphysiol_reactions:
        (1) A[cyt]+B[cyt] → C[cyt], rate = k * c[A, cyt] * c[B, cyt]

---

    Code
      print(r2)
    Output
       compphysiol_reactions:
        (1) A → B (<all cmt>), rate = kAB * c[A]

---

    Code
      print(r3)
    Output
       compphysiol_reactions:
        (1) A[plasma] → B[membrane], rate = kAB * c[A, plasma]

---

    Code
      print(r4)
    Output
       compphysiol_reactions:
        (1) L[plasma]+R[membrane] → LR[membrane], scale = membrane, rate = kon * c[L, plasma] * c[R, membrane]

