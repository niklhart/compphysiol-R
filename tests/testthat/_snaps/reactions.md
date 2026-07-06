# Reaction printing works correctly

    Code
      print(r1)
    Output
       Reactions:
        (1) A+B → C in <all cmt>, rate = k * c[A] * c[B]

---

    Code
      print(r2)
    Output
       Reactions:
        (1) A → ∅ in <all cmt>, rate = k1 * c[A]/(c[A] + K)

