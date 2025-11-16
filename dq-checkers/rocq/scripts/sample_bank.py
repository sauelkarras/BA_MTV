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
    edu_col = _find_col(X, "education")

    if age_col is None or dur_col is None or bal_col is None or edu_col is None:
        raise SystemExit(
            f"Missing columns. Found features: {X.columns.tolist()}"
        )

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
    features = X[[age_col, bal_col, dur_col, edu_col]].rename(
        columns={
            age_col: "age",
            bal_col: "balance",
            dur_col: "duration",
            edu_col: "education",
        }
    )

    df = pd.concat(
        [
            features,
            y_series.rename("y"),
        ],
        axis=1,
    ).dropna()

    # Normalise y (yes/no)
    df["y"] = df["y"].astype(str).str.strip().str.lower()

    # Normalise education to {primary, secondary, tertiary, unknown}
    def normalize_education(raw: str) -> str:
        s = str(raw).strip().lower()

        # If already in the desired coding, keep it
        if s in {"primary", "secondary", "tertiary", "unknown"}:
            return s

        # UCI Bank Marketing legacy variants (if present)
        # basic.4y, basic.6y, basic.9y, high.school,
        # professional.course, university.degree, illiterate, unknown
        if s.startswith("basic."):
            return "primary"
        if s in {"illiterate"}:
            return "primary"
        if s in {"high.school"}:
            return "secondary"
        if s in {"professional.course", "university.degree"}:
            return "tertiary"

        # Fallback
        return "unknown"

    df["education"] = df["education"].apply(normalize_education)

    # Sample
    n = max(1, min(args.n, len(df)))
    sample = df.sample(n=n, random_state=42)[
        ["age", "balance", "duration", "y", "education"]
    ].copy()

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
