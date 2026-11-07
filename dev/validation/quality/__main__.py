import sys
if len(sys.argv) > 1 and sys.argv[1] == 'truth':
    from quality.truth import main
elif len(sys.argv) > 1 and sys.argv[1] == 'reference':
    from quality.reference import main as reference_main
    raise SystemExit(reference_main(sys.argv[2:]))
else:
    from quality.harness.check import main
raise SystemExit(main())
