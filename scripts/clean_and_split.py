#!/usr/bin/env python3
"""
Clean the Calgary property tax roll export and split it into per-layer CSVs
ready for the AutoLISP drafting routine (lisp/draft_parcels.lsp).

Input columns expected (case-insensitive): ADDRESS, ASSESSMENT_CLASS_DESCRIPTION,
COMM_NAME, LAND_USE_DESIGNATION, MULTIPOLYGON (WKT).

Output layout:
    <out_dir>/<ASSESSMENT_CLASS_DESCRIPTION>/<COMM_NAME>/<LAND_USE_DESIGNATION>.csv

Each output CSV has two columns: ADDRESS, POINTS
    POINTS is a "|"-separated list of "x y" pairs (local meters, city-center
    origin), one closed ring per row. A parcel with multiple rings (holes or
    multi-part MULTIPOLYGONs) produces one row per ring, sharing the address.

If the city's export is pre-split into one file per LAND_USE_DESIGNATION
(as under Tax CSVs/), run this script once per input file but point every
run at the SAME out_dir (e.g. Tax_CSVs_Clean/) rather than a fresh
subfolder per input file -- the script already groups rows by class/comm
/land_use inside out_dir, so a shared out_dir merges runs correctly instead
of nesting a redundant top-level folder per input, which is what left
Tax_CSVs_Clean/ needing scripts/flatten_tax_csvs_clean.py to repair.
"""

import argparse
import csv
import math
import os
import re
import sys

# City of Calgary reference point -> becomes CAD origin (0, 0)
CITY_CENTER_LAT = 51.04802965982431
CITY_CENTER_LON = -114.07142221387855
EARTH_RADIUS_M = 6371000.0

RING_RE = re.compile(r"\(([^()]+)\)")
INVALID_NAME_CHARS = re.compile(r'[\\/:*?"<>|]')


def sanitize_name(name):
    """Sanitize a value for use as a path component / AutoCAD layer name.

    LAND_USE_DESIGNATION can be a comma-joined list of zoning codes (e.g.
    "DC,I-G,I-H"); AutoCAD forbids commas (and the other INVALID_NAME_CHARS)
    in layer/symbol names, so commas become "-" rather than "_" to stay
    readable while the rest of the invalid set collapses to "_".
    """
    name = (name or "UNSPECIFIED").strip()
    name = name.replace(",", "-")
    name = INVALID_NAME_CHARS.sub("_", name)
    return name or "UNSPECIFIED"


def project(lon, lat, lat0_rad):
    x = EARTH_RADIUS_M * math.radians(lon - CITY_CENTER_LON) * math.cos(lat0_rad)
    y = EARTH_RADIUS_M * math.radians(lat - CITY_CENTER_LAT)
    return x, y


def parse_multipolygon_rings(wkt):
    """Return a list of rings; each ring is a list of (lon, lat) tuples."""
    rings = []
    for group in RING_RE.findall(wkt):
        pts = []
        for pair in group.split(","):
            pair = pair.strip()
            if not pair:
                continue
            lon_s, lat_s = pair.split()
            pts.append((float(lon_s), float(lat_s)))
        if len(pts) >= 2 and pts[0] == pts[-1]:
            pts = pts[:-1]
        if len(pts) >= 3:
            rings.append(pts)
    return rings


def read_rows(input_path):
    ext = os.path.splitext(input_path)[1].lower()
    if ext in (".xlsx", ".xlsm"):
        import openpyxl

        wb = openpyxl.load_workbook(input_path, read_only=True, data_only=True)
        ws = wb.worksheets[0]
        rows = ws.iter_rows(values_only=True)
        header = [str(h).strip().upper() if h is not None else "" for h in next(rows)]
        for raw in rows:
            if raw is None or all(v is None for v in raw):
                continue
            yield dict(zip(header, raw))
    else:
        with open(input_path, newline="", encoding="utf-8-sig") as f:
            reader = csv.DictReader(f)
            reader.fieldnames = [
                (h or "").strip().upper() for h in reader.fieldnames
            ]
            for row in reader:
                yield {k: v for k, v in row.items()}


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("input", help="Raw tax roll CSV or XLSX export")
    ap.add_argument("out_dir", help="Directory to write the split/cleaned CSVs into")
    args = ap.parse_args()

    lat0_rad = math.radians(CITY_CENTER_LAT)

    groups = {}  # (class, comm, land_use) -> list[(address, points_str)]
    total_rows = 0
    total_rings = 0
    skipped = 0

    for row in read_rows(args.input):
        total_rows += 1
        address = (row.get("ADDRESS") or "").strip().replace(",", " ")
        klass = sanitize_name(row.get("ASSESSMENT_CLASS_DESCRIPTION"))
        comm = sanitize_name(row.get("COMM_NAME"))
        land_use = sanitize_name(row.get("LAND_USE_DESIGNATION"))
        wkt = (row.get("MULTIPOLYGON") or "").strip()

        if not wkt or not address:
            skipped += 1
            continue

        try:
            rings = parse_multipolygon_rings(wkt)
        except ValueError:
            skipped += 1
            continue

        if not rings:
            skipped += 1
            continue

        key = (klass, comm, land_use)
        bucket = groups.setdefault(key, [])
        for ring in rings:
            projected = [project(lon, lat, lat0_rad) for lon, lat in ring]
            points_str = "|".join(f"{x:.3f} {y:.3f}" for x, y in projected)
            bucket.append((address, points_str))
            total_rings += 1

    files_written = 0
    for (klass, comm, land_use), records in groups.items():
        out_path = os.path.join(args.out_dir, klass, comm, f"{land_use}.csv")
        os.makedirs(os.path.dirname(out_path), exist_ok=True)
        with open(out_path, "w", newline="", encoding="utf-8") as f:
            writer = csv.writer(f)
            writer.writerow(["ADDRESS", "POINTS"])
            writer.writerows(records)
        files_written += 1

    print(f"Rows read:        {total_rows}")
    print(f"Rows skipped:      {skipped} (missing address/geometry or unparsable WKT)")
    print(f"Rings drafted:     {total_rings}")
    print(f"Output CSVs:       {files_written}")
    print(f"Output root:       {os.path.abspath(args.out_dir)}")


if __name__ == "__main__":
    sys.exit(main())
