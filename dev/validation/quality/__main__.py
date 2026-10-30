import sys
if len(sys.argv) > 1 and sys.argv[1] == 'truth':
    from quality.truth import main
else:
    from quality.harness.check import main
raise SystemExit(main())
