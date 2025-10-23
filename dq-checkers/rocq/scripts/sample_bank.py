#!/usr/bin/env python3
import argparse
import pandas as pd
from ucimlrepo import fetch_ucirepo

def _find_col(df, key):
    """Find a column whose lowercased, stripped name contains key (lower)."""
    key = key.lower()
    for c in df.columns:
        if key in c.strip().lower():
            return c
    return None

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--n", type=int, default=300, help="sample size")
    ap.add_argument("--out", default="bank_sample_simple.csv",
                    help="output CSV written relative to CWD")
    args = ap.parse_args()

    bank = fetch_ucirepo(id=222)  # UCI Bank Marketing
    X = bank.data.features.copy()
    Y = bank.data.targets.copy() if bank.data.targets is not None else pd.DataFrame()

    # Normalize column names (trim)
    X.columns = X.columns.str.strip()
    if not Y.empty:
        Y.columns = Y.columns.str.strip()

    # Required feature columns
    age_col = _find_col(X, "age")
    dur_col = _find_col(X, "duration")
    bal_col = _find_col(X, "balance")

    if age_col is None or dur_col is None or bal_col is None:
        raise SystemExit(f"Missing columns. Found features: {X.columns.tolist()}")

    # y might live in targets (preferred) or in features (some variants)
    if not Y.empty:
        y_col = _find_col(Y, "y")
        if y_col is None:
            # fall back to features
            y_col = _find_col(X, "y")
            y_series = X[y_col] if y_col else None
        else:
            y_series = Y[y_col]
    else:
        y_col = _find_col(X, "y")
        y_series = X[y_col] if y_col else None

    if y_series is None:
        raise SystemExit("Could not find 'y' column in targets or features.")

    # Build combined frame with desired columns and drop missing
    df = pd.concat(
        [
            X[[age_col, bal_col, dur_col]].rename(
                columns={age_col: "age", bal_col: "balance", dur_col: "duration"}
            ),
            y_series.rename("y"),
        ],
        axis=1,
    ).dropna()

    # Sample
    n = max(1, min(args.n, len(df)))
    sample = df.sample(n=n, random_state=42)[["age", "balance", "duration", "y"]].copy()

    # Ensure y is string (yes/no). Many variants already are.
    sample["y"] = sample["y"].astype(str).str.strip().str.lower()

    sample.to_csv(args.out, index=False)
    print(f"Wrote {len(sample)} rows to {args.out}")

if __name__ == "__main__":
    main()


# python3 scripts/sample_bank.py --n 1000 --out bank_sample_simple.csv

# cd ~/BA_MTV/dq-checkers/rocq
# make -j
# cd out
# ghc -O2 Main.hs Generated.hs -o run
# ./run ../bank_sample_simple.csv
