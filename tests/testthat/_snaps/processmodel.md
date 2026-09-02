# ProcessModel print method uses DSL state names

    Code
      print(process_model)
    Output
      ProcessModel:
       States:
        (1) a[drug, Central], initial = A0
        (2) a[metabolite, Central], initial = 0
       Process rates:
        (1) kmet * a[drug, Central]
       Stoichiometry:
                             (1)
      a[drug, Central]        -1
      a[metabolite, Central]   1
       Equations: (none)
       Observables:
        (1) C = a[drug, Central]/V
       Dosing:
        (1) add 100 to a[drug, Central] at 0
       Parameters:
        (1) V = 10
       Free parameters: A0, kmet

