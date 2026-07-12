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
        (1) A+B → C, scale = <all cmt>, rate = k * c[A] * c[B]

---

    Code
      print(r2)
    Output
       Reactions:
        (1) A → ∅, scale = <all cmt>, rate = k1 * c[A]/(c[A] + K)

