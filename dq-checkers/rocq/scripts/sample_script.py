#!/usr/bin/env python3
import argparse
import os

import pandas as pd
from ucimlrepo import fetch_ucirepo


def dataset_to_uci(dataset_name: str):
    """
    Map the old friendly names to UCI ids and CSV base names.
    """
    if dataset_name == "german-credit":
        return 144, "german_credit"
    if dataset_name == "bank-marketing":
        return 222, "bank_marketing"
    raise ValueError(f"Unknown dataset name: {dataset_name!r}")


def fetch_and_write(uci_id: int, base_name: str):
    repo = fetch_ucirepo(id=uci_id)

    X = repo.data.features
    y = repo.data.targets

    df = pd.concat([X, y], axis=1)

    here = os.path.dirname(os.path.abspath(__file__))
    out_dir = os.path.join(here, "data")
    os.makedirs(out_dir, exist_ok=True)

    csv_path = os.path.join(out_dir, f"{base_name}.csv")

    df.to_csv(csv_path, index=False)

    print(f"Fetched dataset '{base_name}' (id={uci_id})")
    print(f"Wrote {len(df)} rows and {len(df.columns)} columns to: {csv_path}")


def main():
    parser = argparse.ArgumentParser(
        description="Fetch UCI datasets and write them as CSV for MTV checker."
    )
    parser.add_argument(
        "--dataset",
        choices=["german-credit", "bank-marketing"],
        help="Named dataset (backwards-compatible mode).",
    )
    parser.add_argument(
        "--uci-id",
        type=int,
        help="Generic UCI dataset id (e.g. 2 for 'adult').",
    )
    parser.add_argument(
        "--uci-name",
        type=str,
        help="Base name for the CSV file (e.g. 'adult'). "
             "Defaults to 'uci_<id>' if omitted.",
    )

    args = parser.parse_args()

    # Generic mode: use any UCI dataset
    if args.uci_id is not None:
        base_name = args.uci_name if args.uci_name else f"uci_{args.uci_id}"
        fetch_and_write(args.uci_id, base_name)
        return

    # Backwards-compatible named mode
    if args.dataset is not None:
        uci_id, base_name = dataset_to_uci(args.dataset)
        fetch_and_write(uci_id, base_name)
        return

    parser.error("You must specify either --dataset or --uci-id.")


if __name__ == "__main__":
    main()


# python3 scripts/sample_script.py --n 500 --out bank_sample_simple.csv

# cd ~/BA_MTV/dq-checkers/rocq
# make -j
# cd out
# ghc -O2 Main.hs Generated.hs -o run
# ./run ../bank_sample_simple.csv
