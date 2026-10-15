# ProcessModel print method uses DSL state names

    Code
      print(process_model)
    Output
      compphysiol_process_model:
       compphysiol_states:
        (1) a[drug, Central], initial = A0
        (2) a[metabolite, Central], initial = 0
       Process rates:
        (1) kmet * a[drug, Central]
       Stoichiometry:
                             (1)
      a[drug, Central]        -1
      a[metabolite, Central]   1
       compphysiol_equations: (none)
       compphysiol_observables:
        (1) C = a[drug, Central]/V
       compphysiol_dosing:
        (1) add 100 to a[drug, Central] at 0
       compphysiol_parameters:
        (1) V = 10
       Free parameters: A0, kmet

