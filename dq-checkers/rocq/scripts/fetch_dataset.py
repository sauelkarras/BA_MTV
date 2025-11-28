#!/usr/bin/env python3
import argparse
import os
import pandas as pd
from ucimlrepo import fetch_ucirepo

# ---------- helpers ----------

def _default_out_dir():
    here = os.path.dirname(os.path.abspath(__file__))
    return os.path.join(here, "data")


def _ensure_out_dir(out_dir: str) -> str:
    os.makedirs(out_dir, exist_ok=True)
    return out_dir


# ---------- named datasets (backwards-compatible mode) ----------

def fetch_named_dataset(dataset: str, out_dir: str) -> None:
    """
    Old behavior:
      --dataset german-credit    -> id=144,  output german_credit.csv
      --dataset bank-marketing   -> id=222,  output bank_marketing.csv
    """
    if dataset == "german-credit":
        uci_id   = 144
        csv_name = "german_credit.csv"
    elif dataset == "bank-marketing":
        uci_id   = 222
        csv_name = "bank_marketing.csv"
    else:
        raise ValueError(f"Unknown named dataset: {dataset!r}")

    ds = fetch_ucirepo(id=uci_id)
    X  = ds.data.features
    y  = ds.data.targets

    # Combine features + target
    df = pd.concat([X, y], axis=1)

    # For German credit we keep your Attribute1..Attribute20,class convention
    if dataset == "german-credit" and df.shape[1] == 21:
        cols = [f"Attribute{i}" for i in range(1, 21)] + ["class"]
        df.columns = cols

    out_dir  = _ensure_out_dir(out_dir)
    out_path = os.path.join(out_dir, csv_name)
    df.to_csv(out_path, index=False)

    print(f"Fetched dataset '{dataset}' (id={uci_id})")
    print(f"Wrote {len(df)} rows and {df.shape[1]} columns to: {out_path}")


# ---------- generic UCI mode ----------

def fetch_generic_uci(uci_id: int, uci_name: str, out_dir: str) -> None:
    """
    Generic mode:
      --uci-id N --uci-name NAME
    will fetch fetch_ucirepo(id=N) and write NAME.csv (features+targets).
    """
    ds = fetch_ucirepo(id=uci_id)
    X  = ds.data.features
    y  = ds.data.targets

    df = pd.concat([X, y], axis=1)

    out_dir  = _ensure_out_dir(out_dir)
    csv_name = f"{uci_name}.csv"
    out_path = os.path.join(out_dir, csv_name)
    df.to_csv(out_path, index=False)

    print(f"Fetched generic UCI dataset '{uci_name}' (id={uci_id})")
    print(f"Wrote {len(df)} rows and {df.shape[1]} columns to: {out_path}")


# ---------- main CLI ----------

def main() -> None:
    parser = argparse.ArgumentParser(description="Fetch UCI datasets and write CSV.")
    parser.add_argument(
        "--dataset",
        choices=["german-credit", "bank-marketing"],
        help="Named dataset (backwards-compatible mode)."
    )
    parser.add_argument(
        "--uci-id",
        type=int,
        help="UCI id for generic mode (used with --uci-name)."
    )
    parser.add_argument(
        "--uci-name",
        type=str,
        help="Base name for CSV in generic mode (e.g. 'adult')."
    )
    parser.add_argument(
        "--out",
        type=str,
        default=None,
        help="Output directory for CSV (default: ./data relative to this script)."
    )

    args = parser.parse_args()

    out_dir = args.out if args.out is not None else _default_out_dir()

    # Mutually exclusive logic
    if args.dataset is not None:
        if args.uci_id is not None or args.uci_name is not None:
            parser.error("Cannot use --dataset together with --uci-id/--uci-name.")
        fetch_named_dataset(args.dataset, out_dir)
    else:
        # generic UCI mode must specify both id and name
        if args.uci_id is None or args.uci_name is None:
            parser.error(
                "Either --dataset OR both --uci-id and --uci-name must be provided."
            )
        fetch_generic_uci(args.uci_id, args.uci_name, out_dir)


if __name__ == "__main__":
    main()
