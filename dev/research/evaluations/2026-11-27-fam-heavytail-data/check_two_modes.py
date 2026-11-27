"""Pin the rational derivative signs for the two-mode Student profile."""
from pathlib import Path
from runpy import run_path


def main():
    study = Path(__file__).resolve().parents[1] / "2026-11-25-tcert-math-data"
    score = run_path(str(study / "exact_modes.py"))["score"]
    for shape, sign in [(4, 1), (6, -1), (20, -1), (50, 1), (100, -1)]:
        lower, upper = score(shape)
        assert (lower > 0 if sign > 0 else upper < 0), (shape, lower, upper)
        print(f"df={shape}: {'positive' if sign > 0 else 'negative'}")


if __name__ == "__main__":
    main()
