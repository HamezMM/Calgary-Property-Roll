#!/usr/bin/env python3
"""
One-time repair for Tax_CSVs_Clean/.

clean_and_split.py was run once per raw per-zoning export file in
`Tax CSVs/` (the city's export is pre-split by LAND_USE_DESIGNATION), with
each run's out_dir set to a fresh subfolder named after that input file.
That left an extra, redundant top-level folder in front of the layout the
script actually writes (<class>/<comm>/<land_use>.csv), so the tree looks
like:

    Tax_CSVs_Clean/<ZONE_COMBO>/<class>/<comm>/<land_use>.csv

instead of the flat layout draft_parcels.lsp expects:

    Tax_CSVs_Clean/<class>/<comm>/<land_use>.csv

Because the raw LAND_USE_DESIGNATION field itself contains comma-joined
zoning codes (e.g. "DC,I-G,I-H") and sanitize_name() in clean_and_split.py
never stripped commas, the leaf CSV filenames also contain commas -- which
AutoCAD rejects in layer names (ensure-layer uses vl-filename-base as the
layer name).

This script, run once against the already-cleaned tree:
  1. Drops the redundant top-level <ZONE_COMBO> folder (each such folder
     maps 1:1 to exactly one <class>/<comm>/<land_use>.csv leaf, so this is
     a pure move, never a merge).
  2. Replaces "," with "-" in the resulting filename.
  3. Removes the emptied <ZONE_COMBO> directories afterward.

Usage: python3 scripts/flatten_tax_csvs_clean.py [--root Tax_CSVs_Clean] [--dry-run]
"""

import argparse
import os
import shutil
import sys

VALID_CLASSES = {"Residential", "Non-Residential", "Farm Land"}


def find_moves(root):
    moves = []  # (src, dst)
    dest_seen = {}  # dst -> src, to detect collisions
    zone_dirs = sorted(
        d for d in os.listdir(root) if os.path.isdir(os.path.join(root, d))
    )
    for zone in zone_dirs:
        zone_path = os.path.join(root, zone)
        for klass in sorted(os.listdir(zone_path)):
            class_path = os.path.join(zone_path, klass)
            if not os.path.isdir(class_path):
                continue
            if klass not in VALID_CLASSES:
                print(f"WARNING: unexpected class folder {class_path!r}", file=sys.stderr)
            for comm in sorted(os.listdir(class_path)):
                comm_path = os.path.join(class_path, comm)
                if not os.path.isdir(comm_path):
                    continue
                for fname in sorted(os.listdir(comm_path)):
                    src = os.path.join(comm_path, fname)
                    if not os.path.isfile(src) or not fname.lower().endswith(".csv"):
                        print(f"WARNING: unexpected entry {src!r}", file=sys.stderr)
                        continue
                    new_name = fname.replace(",", "-")
                    dst = os.path.join(root, klass, comm, new_name)
                    if dst in dest_seen:
                        raise SystemExit(
                            f"Collision: both {dest_seen[dst]!r} and {src!r} "
                            f"map to {dst!r}"
                        )
                    dest_seen[dst] = src
                    moves.append((src, dst))
    return moves, zone_dirs


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--root", default="Tax_CSVs_Clean")
    ap.add_argument("--dry-run", action="store_true")
    args = ap.parse_args()

    moves, zone_dirs = find_moves(args.root)
    print(f"Found {len(moves)} CSV files to relocate across {len(zone_dirs)} zone folders.")

    if args.dry_run:
        for src, dst in moves[:10]:
            print(f"  {src}  ->  {dst}")
        if len(moves) > 10:
            print(f"  ... and {len(moves) - 10} more")
        return

    for src, dst in moves:
        os.makedirs(os.path.dirname(dst), exist_ok=True)
        shutil.move(src, dst)

    removed = 0
    for zone in zone_dirs:
        zone_path = os.path.join(args.root, zone)
        if os.path.isdir(zone_path):
            shutil.rmtree(zone_path)
            removed += 1

    print(f"Moved {len(moves)} files.")
    print(f"Removed {removed} now-empty zone-combo folders.")


if __name__ == "__main__":
    main()
