# AnalyticalModel print method renders symbolic A matrix

    Code
      print(analytical_model)
    Output
      compphysiol_analytical_model:
       compphysiol_states:
        (1) a[drug, Central], initial = A0
        (2) a[drug, Peripheral], initial = 0
       A:
          (1)         (2) 
      (1) -k10 + -k12 k21 
      (2) k12         -k21
       b:
        (1) 0
        (2) 0
       compphysiol_equations: (none)
       compphysiol_observables:
        (1) Acentral = a[drug, Central]
       compphysiol_parameters: (none)
       Free parameters: A0, k10, k12, k21

