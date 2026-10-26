Tests
  ./examples/benchmark/mutants/bsort
    bsort_M0_ROR_LT_LTE.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 843, mtrFeasibilityTrivial = 498, mtrFeasibilityPruned = 530, mtrFeasibilityPrunedTrivial = 498, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (33.44s)
      no simplifier: Metrics {mtrFeasibilityChecks = 843, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 530, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (62.62s)
    bsort_M10_AOR_MINUS_ROTATE.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 7, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 0, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.22s)
      no simplifier: Metrics {mtrFeasibilityChecks = 7, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 0, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (1.37s)
    bsort_M11_AOR_MINUS_PLUS.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 7, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 0, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.18s)
      no simplifier: Metrics {mtrFeasibilityChecks = 7, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 0, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (1.37s)
    bsort_M12_AOR_MINUS_DROP_RIGHT.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 4131, mtrFeasibilityTrivial = 2799, mtrFeasibilityPruned = 2799, mtrFeasibilityPrunedTrivial = 2799, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    FAIL (136.54s)
        app/Test.hs:28:
        Mutant not killed
        Use -p '/bsort_M12_AOR_MINUS_DROP_RIGHT.gcl.simplifier/' to rerun this test only.
      no simplifier: Metrics {mtrFeasibilityChecks = 4131, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 2799, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: FAIL (208.51s)
        app/Test.hs:29:
        Mutant not killed
        Use -p '/bsort_M12_AOR_MINUS_DROP_RIGHT.gcl.no simplifier/' to rerun this test only.
    bsort_M13_AOR_MINUS_ROTATE.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 7, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 0, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.74s)
      no simplifier: Metrics {mtrFeasibilityChecks = 7, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 0, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (1.47s)
    bsort_M14_AOR_MINUS_PLUS.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 7, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 0, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.18s)
      no simplifier: Metrics {mtrFeasibilityChecks = 7, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 0, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (1.41s)
    bsort_M15_AOR_MINUS_DROP_RIGHT.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 7, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 0, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.20s)
      no simplifier: Metrics {mtrFeasibilityChecks = 7, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 0, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (1.33s)
    bsort_M16_AOR_MINUS_ROTATE.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 7, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 0, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.91s)
      no simplifier: Metrics {mtrFeasibilityChecks = 7, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 0, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (1.38s)
    bsort_M17_AOR_MINUS_PLUS.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 7, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 0, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.21s)
      no simplifier: Metrics {mtrFeasibilityChecks = 7, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 0, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (1.45s)
    bsort_M18_AOR_MINUS_DROP_RIGHT.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 222, mtrFeasibilityTrivial = 206, mtrFeasibilityPruned = 207, mtrFeasibilityPrunedTrivial = 206, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    FAIL (3.03s)
        app/Test.hs:28:
        Mutant not killed
        Use -p '/bsort_M18_AOR_MINUS_DROP_RIGHT.gcl.simplifier/' to rerun this test only.
      no simplifier: Metrics {mtrFeasibilityChecks = 222, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 207, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: FAIL (5.18s)
        app/Test.hs:29:
        Mutant not killed
        Use -p '/bsort_M18_AOR_MINUS_DROP_RIGHT.gcl.no simplifier/' to rerun this test only.
    bsort_M19_AOR_MINUS_ROTATE.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 13, mtrFeasibilityTrivial = 3, mtrFeasibilityPruned = 6, mtrFeasibilityPrunedTrivial = 3, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.17s)
      no simplifier: Metrics {mtrFeasibilityChecks = 13, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 6, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (0.18s)
    bsort_M1_ROR_LT_GT.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 2, mtrFeasibilityTrivial = 1, mtrFeasibilityPruned = 1, mtrFeasibilityPrunedTrivial = 1, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.02s)
      no simplifier: Metrics {mtrFeasibilityChecks = 2, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 1, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (0.02s)
    bsort_M20_AOR_MINUS_PLUS.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 2026, mtrFeasibilityTrivial = 1520, mtrFeasibilityPruned = 1520, mtrFeasibilityPrunedTrivial = 1520, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    FAIL (59.00s)
        app/Test.hs:28:
        Mutant not killed
        Use -p '/bsort_M20_AOR_MINUS_PLUS.gcl.simplifier/' to rerun this test only.
      no simplifier: Metrics {mtrFeasibilityChecks = 2026, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 1520, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: FAIL (114.50s)
        app/Test.hs:29:
        Mutant not killed
        Use -p '/bsort_M20_AOR_MINUS_PLUS.gcl.no simplifier/' to rerun this test only.
    bsort_M21_AOR_PLUS_DROP_LEFT.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 4512, mtrFeasibilityTrivial = 2837, mtrFeasibilityPruned = 2945, mtrFeasibilityPrunedTrivial = 2837, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    FAIL (166.29s)
        app/Test.hs:28:
        Mutant not killed
        Use -p '/bsort_M21_AOR_PLUS_DROP_LEFT.gcl.simplifier/' to rerun this test only.
      no simplifier: Metrics {mtrFeasibilityChecks = 4512, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 2945, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: FAIL (240.12s)
        app/Test.hs:29:
        Mutant not killed
        Use -p '/bsort_M21_AOR_PLUS_DROP_LEFT.gcl.no simplifier/' to rerun this test only.
    bsort_M22_AOR_PLUS_DROP_RIGHT.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 4646, mtrFeasibilityTrivial = 2988, mtrFeasibilityPruned = 3101, mtrFeasibilityPrunedTrivial = 2988, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    FAIL (143.48s)
        app/Test.hs:28:
        Mutant not killed
        Use -p '/bsort_M22_AOR_PLUS_DROP_RIGHT.gcl.simplifier/' to rerun this test only.
      no simplifier: Metrics {mtrFeasibilityChecks = 4646, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 3101, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: FAIL (239.99s)
        app/Test.hs:29:
        Mutant not killed
        Use -p '/bsort_M22_AOR_PLUS_DROP_RIGHT.gcl.no simplifier/' to rerun this test only.
    bsort_M23_AOR_PLUS_MINUS.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 7, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 0, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.20s)
      no simplifier: Metrics {mtrFeasibilityChecks = 7, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 0, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (1.41s)
    bsort_M2_AOR_MINUS_DROP_RIGHT.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 7, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 0, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.12s)
      no simplifier: Metrics {mtrFeasibilityChecks = 7, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 0, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (0.55s)
    bsort_M3_AOR_MINUS_ROTATE.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 18, mtrFeasibilityTrivial = 6, mtrFeasibilityPruned = 10, mtrFeasibilityPrunedTrivial = 6, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.20s)
      no simplifier: Metrics {mtrFeasibilityChecks = 18, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 10, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (0.23s)
    bsort_M4_AOR_MINUS_PLUS.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 7, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 0, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.12s)
      no simplifier: Metrics {mtrFeasibilityChecks = 7, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 0, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (0.69s)
    bsort_M5_ROR_LT_LTE.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 7, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 0, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.22s)
      no simplifier: Metrics {mtrFeasibilityChecks = 7, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 0, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (1.44s)
    bsort_M6_ROR_LT_GT.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 18, mtrFeasibilityTrivial = 7, mtrFeasibilityPruned = 10, mtrFeasibilityPrunedTrivial = 7, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.17s)
      no simplifier: Metrics {mtrFeasibilityChecks = 18, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 10, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (0.25s)
    bsort_M7_ROR_LT_LTE.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 4847, mtrFeasibilityTrivial = 3150, mtrFeasibilityPruned = 3172, mtrFeasibilityPrunedTrivial = 3150, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    FAIL (173.44s)
        app/Test.hs:28:
        Mutant not killed
        Use -p '/bsort_M7_ROR_LT_LTE.gcl.simplifier/' to rerun this test only.
      no simplifier: Metrics {mtrFeasibilityChecks = 4847, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 3172, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: FAIL (257.01s)
        app/Test.hs:29:
        Mutant not killed
        Use -p '/bsort_M7_ROR_LT_LTE.gcl.no simplifier/' to rerun this test only.
    bsort_M8_ROR_LT_GT.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 7, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 0, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.20s)
      no simplifier: Metrics {mtrFeasibilityChecks = 7, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 0, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (1.37s)
    bsort_M9_AOR_MINUS_DROP_RIGHT.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 89, mtrFeasibilityTrivial = 76, mtrFeasibilityPruned = 76, mtrFeasibilityPrunedTrivial = 76, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (1.04s)
      no simplifier: Metrics {mtrFeasibilityChecks = 89, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 76, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (1.91s)
  ./examples/benchmark/mutants/divByN
    divByN_M0_LOR_AND_DROP_RIGHT.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 16, mtrFeasibilityTrivial = 6, mtrFeasibilityPruned = 6, mtrFeasibilityPrunedTrivial = 6, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.23s)
      no simplifier: Metrics {mtrFeasibilityChecks = 16, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 6, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (0.31s)
    divByN_M10_AOR_PLUS_MINUS.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 107, mtrFeasibilityTrivial = 96, mtrFeasibilityPruned = 96, mtrFeasibilityPrunedTrivial = 96, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    FAIL (1.25s)
        app/Test.hs:28:
        Mutant not killed
        Use -p '/divByN_M10_AOR_PLUS_MINUS.gcl.simplifier/' to rerun this test only.
      no simplifier: Metrics {mtrFeasibilityChecks = 107, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 96, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: FAIL (2.27s)
        app/Test.hs:29:
        Mutant not killed
        Use -p '/divByN_M10_AOR_PLUS_MINUS.gcl.no simplifier/' to rerun this test only.
    divByN_M11_EOR_EQUAL_NEGATE.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 26, mtrFeasibilityTrivial = 16, mtrFeasibilityPruned = 17, mtrFeasibilityPrunedTrivial = 16, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.35s)
      no simplifier: Metrics {mtrFeasibilityChecks = 26, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 17, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (0.45s)
    divByN_M12_AOR_TIMES_DROP_LEFT.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 23, mtrFeasibilityTrivial = 13, mtrFeasibilityPruned = 13, mtrFeasibilityPrunedTrivial = 13, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.29s)
      no simplifier: Metrics {mtrFeasibilityChecks = 23, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 13, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (0.43s)
    divByN_M13_AOR_TIMES_DROP_RIGHT.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 87, mtrFeasibilityTrivial = 76, mtrFeasibilityPruned = 76, mtrFeasibilityPrunedTrivial = 76, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    FAIL (0.98s)
        app/Test.hs:28:
        Mutant not killed
        Use -p '/divByN_M13_AOR_TIMES_DROP_RIGHT.gcl.simplifier/' to rerun this test only.
      no simplifier: Metrics {mtrFeasibilityChecks = 87, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 76, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: FAIL (1.83s)
        app/Test.hs:29:
        Mutant not killed
        Use -p '/divByN_M13_AOR_TIMES_DROP_RIGHT.gcl.no simplifier/' to rerun this test only.
    divByN_M14_AOR_PLUS_DROP_LEFT.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 87, mtrFeasibilityTrivial = 76, mtrFeasibilityPruned = 76, mtrFeasibilityPrunedTrivial = 76, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    FAIL (0.99s)
        app/Test.hs:28:
        Mutant not killed
        Use -p '/divByN_M14_AOR_PLUS_DROP_LEFT.gcl.simplifier/' to rerun this test only.
      no simplifier: Metrics {mtrFeasibilityChecks = 87, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 76, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: FAIL (1.78s)
        app/Test.hs:29:
        Mutant not killed
        Use -p '/divByN_M14_AOR_PLUS_DROP_LEFT.gcl.no simplifier/' to rerun this test only.
    divByN_M15_AOR_PLUS_DROP_RIGHT.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 87, mtrFeasibilityTrivial = 76, mtrFeasibilityPruned = 76, mtrFeasibilityPrunedTrivial = 76, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    FAIL (0.97s)
        app/Test.hs:28:
        Mutant not killed
        Use -p '/divByN_M15_AOR_PLUS_DROP_RIGHT.gcl.simplifier/' to rerun this test only.
      no simplifier: Metrics {mtrFeasibilityChecks = 87, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 76, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: FAIL (1.78s)
        app/Test.hs:29:
        Mutant not killed
        Use -p '/divByN_M15_AOR_PLUS_DROP_RIGHT.gcl.no simplifier/' to rerun this test only.
    divByN_M16_AOR_PLUS_MINUS.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 87, mtrFeasibilityTrivial = 76, mtrFeasibilityPruned = 76, mtrFeasibilityPrunedTrivial = 76, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    FAIL (1.02s)
        app/Test.hs:28:
        Mutant not killed
        Use -p '/divByN_M16_AOR_PLUS_MINUS.gcl.simplifier/' to rerun this test only.
      no simplifier: Metrics {mtrFeasibilityChecks = 87, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 76, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: FAIL (1.85s)
        app/Test.hs:29:
        Mutant not killed
        Use -p '/divByN_M16_AOR_PLUS_MINUS.gcl.no simplifier/' to rerun this test only.
    divByN_M1_LOR_AND_DROP_LEFT.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 87, mtrFeasibilityTrivial = 76, mtrFeasibilityPruned = 76, mtrFeasibilityPrunedTrivial = 76, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    FAIL (0.97s)
        app/Test.hs:28:
        Mutant not killed
        Use -p '/divByN_M1_LOR_AND_DROP_LEFT.gcl.simplifier/' to rerun this test only.
      no simplifier: Metrics {mtrFeasibilityChecks = 87, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 76, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: FAIL (1.82s)
        app/Test.hs:29:
        Mutant not killed
        Use -p '/divByN_M1_LOR_AND_DROP_LEFT.gcl.no simplifier/' to rerun this test only.
    divByN_M2_LOR_AND_OR.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 87, mtrFeasibilityTrivial = 76, mtrFeasibilityPruned = 77, mtrFeasibilityPrunedTrivial = 76, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    FAIL (0.96s)
        app/Test.hs:28:
        Mutant not killed
        Use -p '/divByN_M2_LOR_AND_OR.gcl.simplifier/' to rerun this test only.
      no simplifier: Metrics {mtrFeasibilityChecks = 87, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 77, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: FAIL (1.85s)
        app/Test.hs:29:
        Mutant not killed
        Use -p '/divByN_M2_LOR_AND_OR.gcl.no simplifier/' to rerun this test only.
    divByN_M3_ROR_LTE_LT.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 87, mtrFeasibilityTrivial = 76, mtrFeasibilityPruned = 76, mtrFeasibilityPrunedTrivial = 76, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    FAIL (1.00s)
        app/Test.hs:28:
        Mutant not killed
        Use -p '/divByN_M3_ROR_LTE_LT.gcl.simplifier/' to rerun this test only.
      no simplifier: Metrics {mtrFeasibilityChecks = 87, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 76, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: FAIL (1.79s)
        app/Test.hs:29:
        Mutant not killed
        Use -p '/divByN_M3_ROR_LTE_LT.gcl.no simplifier/' to rerun this test only.
    divByN_M4_ROR_LTE_GTE.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 2, mtrFeasibilityTrivial = 1, mtrFeasibilityPruned = 1, mtrFeasibilityPrunedTrivial = 1, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.02s)
      no simplifier: Metrics {mtrFeasibilityChecks = 2, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 1, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (0.03s)
    divByN_M5_NOT_DROP.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 2, mtrFeasibilityTrivial = 1, mtrFeasibilityPruned = 1, mtrFeasibilityPrunedTrivial = 1, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.02s)
      no simplifier: Metrics {mtrFeasibilityChecks = 2, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 1, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (0.05s)
    divByN_M6_ROR_LT_LTE.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 41, mtrFeasibilityTrivial = 31, mtrFeasibilityPruned = 31, mtrFeasibilityPrunedTrivial = 31, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.52s)
      no simplifier: Metrics {mtrFeasibilityChecks = 41, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 31, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (0.87s)
    divByN_M7_ROR_LT_GT.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 59, mtrFeasibilityTrivial = 48, mtrFeasibilityPruned = 48, mtrFeasibilityPrunedTrivial = 48, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.65s)
      no simplifier: Metrics {mtrFeasibilityChecks = 59, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 48, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (1.15s)
    divByN_M8_AOR_PLUS_DROP_LEFT.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 107, mtrFeasibilityTrivial = 96, mtrFeasibilityPruned = 96, mtrFeasibilityPrunedTrivial = 96, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    FAIL (1.12s)
        app/Test.hs:28:
        Mutant not killed
        Use -p '/divByN_M8_AOR_PLUS_DROP_LEFT.gcl.simplifier/' to rerun this test only.
      no simplifier: Metrics {mtrFeasibilityChecks = 107, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 96, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: FAIL (2.15s)
        app/Test.hs:29:
        Mutant not killed
        Use -p '/divByN_M8_AOR_PLUS_DROP_LEFT.gcl.no simplifier/' to rerun this test only.
    divByN_M9_AOR_PLUS_DROP_RIGHT.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 107, mtrFeasibilityTrivial = 96, mtrFeasibilityPruned = 96, mtrFeasibilityPrunedTrivial = 96, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    FAIL (1.19s)
        app/Test.hs:28:
        Mutant not killed
        Use -p '/divByN_M9_AOR_PLUS_DROP_RIGHT.gcl.simplifier/' to rerun this test only.
      no simplifier: Metrics {mtrFeasibilityChecks = 107, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 96, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: FAIL (2.23s)
        app/Test.hs:29:
        Mutant not killed
        Use -p '/divByN_M9_AOR_PLUS_DROP_RIGHT.gcl.no simplifier/' to rerun this test only.
  ./examples/benchmark/mutants/memberOf
    memberOf_M0_ROR_LT_LTE.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 12, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 0, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.19s)
      no simplifier: Metrics {mtrFeasibilityChecks = 12, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 0, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (0.16s)
    memberOf_M1_ROR_LT_GT.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 2, mtrFeasibilityTrivial = 1, mtrFeasibilityPruned = 1, mtrFeasibilityPrunedTrivial = 1, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.03s)
      no simplifier: Metrics {mtrFeasibilityChecks = 2, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 1, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (0.04s)
    memberOf_M2_EOR_EQUAL_NEGATE.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 7441, mtrFeasibilityTrivial = 7, mtrFeasibilityPruned = 10, mtrFeasibilityPrunedTrivial = 7, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (403.05s)
      no simplifier: Metrics {mtrFeasibilityChecks = 7441, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 10, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (309.88s)
    memberOf_M3_AOR_PLUS_DROP_LEFT.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 148, mtrFeasibilityTrivial = 42, mtrFeasibilityPruned = 111, mtrFeasibilityPrunedTrivial = 42, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    FAIL (2.53s)
        app/Test.hs:28:
        Mutant not killed
        Use -p '/memberOf_M3_AOR_PLUS_DROP_LEFT.gcl.simplifier/' to rerun this test only.
      no simplifier: Metrics {mtrFeasibilityChecks = 148, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 111, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: FAIL (3.04s)
        app/Test.hs:29:
        Mutant not killed
        Use -p '/memberOf_M3_AOR_PLUS_DROP_LEFT.gcl.no simplifier/' to rerun this test only.
    memberOf_M4_AOR_PLUS_DROP_RIGHT.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 89, mtrFeasibilityTrivial = 24, mtrFeasibilityPruned = 66, mtrFeasibilityPrunedTrivial = 24, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    FAIL (1.68s)
        app/Test.hs:28:
        Mutant not killed
        Use -p '/memberOf_M4_AOR_PLUS_DROP_RIGHT.gcl.simplifier/' to rerun this test only.
      no simplifier: Metrics {mtrFeasibilityChecks = 89, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 66, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: FAIL (1.92s)
        app/Test.hs:29:
        Mutant not killed
        Use -p '/memberOf_M4_AOR_PLUS_DROP_RIGHT.gcl.no simplifier/' to rerun this test only.
    memberOf_M5_AOR_PLUS_MINUS.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 7446, mtrFeasibilityTrivial = 2812, mtrFeasibilityPruned = 2812, mtrFeasibilityPrunedTrivial = 2812, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    FAIL (216.53s)
        app/Test.hs:28:
        Mutant not killed
        Use -p '/memberOf_M5_AOR_PLUS_MINUS.gcl.simplifier/' to rerun this test only.
      no simplifier: Metrics {mtrFeasibilityChecks = 7446, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 2812, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: FAIL (272.79s)
        app/Test.hs:29:
        Mutant not killed
        Use -p '/memberOf_M5_AOR_PLUS_MINUS.gcl.no simplifier/' to rerun this test only.
  ./examples/benchmark/mutants/pullUp
    pullUp_M0_ROR_GTE_GT.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 26, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 11, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.43s)
      no simplifier: Metrics {mtrFeasibilityChecks = 26, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 11, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (0.35s)
    pullUp_M10_ROR_GTE_GT.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 7, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 2, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.10s)
      no simplifier: Metrics {mtrFeasibilityChecks = 7, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 2, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (0.12s)
    pullUp_M11_ROR_GTE_LTE.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 7, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 2, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.11s)
      no simplifier: Metrics {mtrFeasibilityChecks = 7, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 2, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (0.11s)
    pullUp_M12_AOR_PLUS_DROP_LEFT.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 17, mtrFeasibilityTrivial = 2, mtrFeasibilityPruned = 9, mtrFeasibilityPrunedTrivial = 2, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    FAIL (0.21s)
        app/Test.hs:28:
        Mutant not killed
        Use -p '/pullUp_M12_AOR_PLUS_DROP_LEFT.gcl.simplifier/' to rerun this test only.
      no simplifier: Metrics {mtrFeasibilityChecks = 17, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 9, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: FAIL (0.23s)
        app/Test.hs:29:
        Mutant not killed
        Use -p '/pullUp_M12_AOR_PLUS_DROP_LEFT.gcl.no simplifier/' to rerun this test only.
    pullUp_M13_AOR_PLUS_DROP_RIGHT.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 17, mtrFeasibilityTrivial = 4, mtrFeasibilityPruned = 9, mtrFeasibilityPrunedTrivial = 4, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    FAIL (0.21s)
        app/Test.hs:28:
        Mutant not killed
        Use -p '/pullUp_M13_AOR_PLUS_DROP_RIGHT.gcl.simplifier/' to rerun this test only.
      no simplifier: Metrics {mtrFeasibilityChecks = 17, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 9, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: FAIL (0.30s)
        app/Test.hs:29:
        Mutant not killed
        Use -p '/pullUp_M13_AOR_PLUS_DROP_RIGHT.gcl.no simplifier/' to rerun this test only.
    pullUp_M14_AOR_PLUS_MINUS.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 17, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 9, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    FAIL (0.33s)
        app/Test.hs:28:
        Mutant not killed
        Use -p '/pullUp_M14_AOR_PLUS_MINUS.gcl.simplifier/' to rerun this test only.
      no simplifier: Metrics {mtrFeasibilityChecks = 17, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 9, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: FAIL (0.23s)
        app/Test.hs:29:
        Mutant not killed
        Use -p '/pullUp_M14_AOR_PLUS_MINUS.gcl.no simplifier/' to rerun this test only.
    pullUp_M15_AOR_PLUS_DROP_LEFT.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 4, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 1, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.06s)
      no simplifier: Metrics {mtrFeasibilityChecks = 4, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 1, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (0.08s)
    pullUp_M16_AOR_PLUS_DROP_RIGHT.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 4, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 1, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.06s)
      no simplifier: Metrics {mtrFeasibilityChecks = 4, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 1, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (0.06s)
    pullUp_M17_AOR_PLUS_MINUS.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 4, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 1, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.07s)
      no simplifier: Metrics {mtrFeasibilityChecks = 4, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 1, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (0.06s)
    pullUp_M18_AOR_PLUS_DROP_LEFT.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 4, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 1, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.06s)
      no simplifier: Metrics {mtrFeasibilityChecks = 4, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 1, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (0.06s)
    pullUp_M19_AOR_PLUS_DROP_RIGHT.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 4, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 1, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.07s)
      no simplifier: Metrics {mtrFeasibilityChecks = 4, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 1, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (0.06s)
    pullUp_M1_ROR_GTE_LTE.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 26, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 11, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.36s)
      no simplifier: Metrics {mtrFeasibilityChecks = 26, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 11, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (0.39s)
    pullUp_M20_AOR_PLUS_MINUS.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 4, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 1, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.06s)
      no simplifier: Metrics {mtrFeasibilityChecks = 4, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 1, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (0.06s)
    pullUp_M21_AOR_PLUS_DROP_LEFT.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 173, mtrFeasibilityTrivial = 125, mtrFeasibilityPruned = 129, mtrFeasibilityPrunedTrivial = 125, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    FAIL (2.26s)
        app/Test.hs:28:
        Mutant not killed
        Use -p '/pullUp_M21_AOR_PLUS_DROP_LEFT.gcl.simplifier/' to rerun this test only.
      no simplifier: Metrics {mtrFeasibilityChecks = 173, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 129, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: FAIL (3.76s)
        app/Test.hs:29:
        Mutant not killed
        Use -p '/pullUp_M21_AOR_PLUS_DROP_LEFT.gcl.no simplifier/' to rerun this test only.
    pullUp_M22_AOR_PLUS_DROP_RIGHT.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 173, mtrFeasibilityTrivial = 125, mtrFeasibilityPruned = 129, mtrFeasibilityPrunedTrivial = 125, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    FAIL (2.22s)
        app/Test.hs:28:
        Mutant not killed
        Use -p '/pullUp_M22_AOR_PLUS_DROP_RIGHT.gcl.simplifier/' to rerun this test only.
      no simplifier: Metrics {mtrFeasibilityChecks = 173, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 129, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: FAIL (3.74s)
        app/Test.hs:29:
        Mutant not killed
        Use -p '/pullUp_M22_AOR_PLUS_DROP_RIGHT.gcl.no simplifier/' to rerun this test only.
    pullUp_M23_AOR_PLUS_MINUS.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 7188, mtrFeasibilityTrivial = 2622, mtrFeasibilityPruned = 2626, mtrFeasibilityPrunedTrivial = 2622, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    FAIL (158.86s)
        app/Test.hs:28:
        Mutant not killed
        Use -p '/pullUp_M23_AOR_PLUS_MINUS.gcl.simplifier/' to rerun this test only.
      no simplifier: Metrics {mtrFeasibilityChecks = 7188, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 2626, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: FAIL (386.07s)
        app/Test.hs:29:
        Mutant not killed
        Use -p '/pullUp_M23_AOR_PLUS_MINUS.gcl.no simplifier/' to rerun this test only.
    pullUp_M2_AOR_PLUS_DROP_LEFT.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 4, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 1, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.04s)
      no simplifier: Metrics {mtrFeasibilityChecks = 4, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 1, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (0.04s)
    pullUp_M3_AOR_PLUS_DROP_RIGHT.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 4, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 1, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.04s)
      no simplifier: Metrics {mtrFeasibilityChecks = 4, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 1, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (0.03s)
    pullUp_M4_AOR_PLUS_MINUS.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 4, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 1, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.03s)
      no simplifier: Metrics {mtrFeasibilityChecks = 4, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 1, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (0.03s)
    pullUp_M5_ROR_LT_LTE.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 75, mtrFeasibilityTrivial = 2, mtrFeasibilityPruned = 37, mtrFeasibilityPrunedTrivial = 2, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    FAIL (0.65s)
        app/Test.hs:28:
        Mutant not killed
        Use -p '/pullUp_M5_ROR_LT_LTE.gcl.simplifier/' to rerun this test only.
      no simplifier: Metrics {mtrFeasibilityChecks = 75, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 37, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: FAIL (0.67s)
        app/Test.hs:29:
        Mutant not killed
        Use -p '/pullUp_M5_ROR_LT_LTE.gcl.no simplifier/' to rerun this test only.
    pullUp_M6_ROR_LT_GT.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 7, mtrFeasibilityTrivial = 4, mtrFeasibilityPruned = 4, mtrFeasibilityPrunedTrivial = 4, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.03s)
      no simplifier: Metrics {mtrFeasibilityChecks = 7, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 4, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (0.05s)
    pullUp_M7_AOR_MINUS_DROP_RIGHT.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 75, mtrFeasibilityTrivial = 2, mtrFeasibilityPruned = 37, mtrFeasibilityPrunedTrivial = 2, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    FAIL (0.68s)
        app/Test.hs:28:
        Mutant not killed
        Use -p '/pullUp_M7_AOR_MINUS_DROP_RIGHT.gcl.simplifier/' to rerun this test only.
      no simplifier: Metrics {mtrFeasibilityChecks = 75, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 37, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: FAIL (0.68s)
        app/Test.hs:29:
        Mutant not killed
        Use -p '/pullUp_M7_AOR_MINUS_DROP_RIGHT.gcl.no simplifier/' to rerun this test only.
    pullUp_M8_AOR_MINUS_ROTATE.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 7, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 4, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    OK (0.08s)
      no simplifier: Metrics {mtrFeasibilityChecks = 7, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 4, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: OK (0.05s)
    pullUp_M9_AOR_MINUS_PLUS.gcl
      simplifier:    Metrics {mtrFeasibilityChecks = 190, mtrFeasibilityTrivial = 5, mtrFeasibilityPruned = 120, mtrFeasibilityPrunedTrivial = 5, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      simplifier:    FAIL (2.11s)
        app/Test.hs:28:
        Mutant not killed
        Use -p '/pullUp_M9_AOR_MINUS_PLUS.gcl.simplifier/' to rerun this test only.
      no simplifier: Metrics {mtrFeasibilityChecks = 190, mtrFeasibilityTrivial = 0, mtrFeasibilityPruned = 120, mtrFeasibilityPrunedTrivial = 0, mtrSimplifierElapsed = 0s, mtrElapsed = 0s}
      no simplifier: FAIL (2.27s)
        app/Test.hs:29:
        Mutant not killed
        Use -p '/pullUp_M9_AOR_MINUS_PLUS.gcl.no simplifier/' to rerun this test only.