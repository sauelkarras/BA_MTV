#!/usr/bin/env python3
import argparse
import os

import pandas as pd
from ucimlrepo import fetch_ucirepo


DATASET_CONFIG = {
    # German Credit (Statlog)
    "german-credit": {
        "id": 144,
        "default_out": "data/german_credit.csv",
    },
    # Portuguese Bank Marketing
    "bank-marketing": {
        "id": 222,
        "default_out": "data/bank_marketing.csv",
    },
}


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Fetch a UCI dataset via ucimlrepo and write a full CSV (features + targets)."
    )
    parser.add_argument(
        "--dataset",
        choices=DATASET_CONFIG.keys(),
        required=True,
        help="Which dataset to fetch (german-credit or bank-marketing).",
    )
    parser.add_argument(
        "--out",
        help=(
            "Output CSV path, relative to the rocq directory. "
            "If omitted, a dataset-specific default is used."
        ),
    )

    args = parser.parse_args()
    cfg = DATASET_CONFIG[args.dataset]

    # Resolve output path
    script_dir = os.path.dirname(os.path.abspath(__file__))
    rocq_root = script_dir  # scripts/ is directly under rocq/
    default_out = cfg["default_out"]
    out_rel = args.out if args.out is not None else default_out
    out_path = os.path.join(rocq_root, out_rel)

    os.makedirs(os.path.dirname(out_path), exist_ok=True)

    # Fetch dataset from UCI
    ds = fetch_ucirepo(id=cfg["id"])
    X = ds.data.features
    y = ds.data.targets

    # Concatenate features and targets into a single DataFrame
    df = pd.concat([X, y], axis=1)

    # Write CSV without index column
    df.to_csv(out_path, index=False)

    print(f"Fetched dataset '{args.dataset}' (id={cfg['id']})")
    print(f"Wrote {len(df)} rows and {len(df.columns)} columns to: {out_path}")


if __name__ == "__main__":
    main()

