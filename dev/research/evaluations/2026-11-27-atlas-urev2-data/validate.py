"""Check the reviewed fragment against atlas regimes and Markdown headings."""
from collections import Counter
import csv
from pathlib import Path
import re
from urllib.parse import unquote

ROOT = Path(__file__).resolve().parents[4]
HERE = Path(__file__).resolve().parent
ATLAS = ROOT / "dev/research/existence-atlas"
FIELDS = (
    "family", "held", "question", "claim", "direction", "status",
    "evidence", "frontier", "origin",
)


def read_csv(path):
    with path.open(newline="", encoding="utf-8") as source:
        reader = csv.DictReader(source)
        return tuple(reader.fieldnames), list(reader)


def headings(path):
    slugs = Counter()
    anchors = set()
    for heading in re.findall(r"^#{1,6}\s+(.+?)\s*#*\s*$", path.read_text(), re.M):
        slug = re.sub(r"[^\w -]", "", heading.lower()).replace(" ", "-")
        suffix = f"-{slugs[slug]}" if slugs[slug] else ""
        anchors.add(slug + suffix)
        slugs[slug] += 1
    return anchors


def main():
    schema, rows = read_csv(HERE / "reviewed.csv")
    assert schema == FIELDS
    _, atlas_rows = read_csv(ATLAS / "atlas.csv")
    atlas_keys = {
        (row["family"], "" if row["held"] == "all free" else row["held"], "uniqueness")
        for row in atlas_rows
    }
    origins = {}
    for job in ("discrete", "shape"):
        _, proposed = read_csv(HERE.parent / f"2026-11-27-uniq-{job}-math-data/proofs.csv")
        for row in proposed:
            key = tuple(row[field] for field in FIELDS[:3])
            assert key not in origins, key
            origins[key] = row["origin"]

    seen = set()
    anchor_cache = {}
    for row in rows:
        assert None not in row and all(value is not None for value in row.values()), row
        key = tuple(row[field] for field in FIELDS[:3])
        assert key in atlas_keys and key not in seen, key
        assert row["origin"] == origins[key], key
        assert row["claim"] and row["status"] == "written", key
        assert row["direction"] in {"exact", "one-sided"}, key
        seen.add(key)
        filename, separator, fragment = row["evidence"].partition("#")
        path = ROOT / filename
        assert separator and fragment and path.is_file(), row["evidence"]
        if path not in anchor_cache:
            anchor_cache[path] = headings(path)
        assert unquote(fragment) in anchor_cache[path], row["evidence"]

    assert seen == (atlas_keys & origins.keys()), (atlas_keys & origins.keys()) - seen
    print(f"{len(rows)} unique atlas keys and evidence anchors verified.")
    print("Grade counts:", dict(Counter(row["direction"] for row in rows)))
    print("Source counts:", dict(Counter(row["origin"] for row in rows)))


if __name__ == "__main__":
    main()
