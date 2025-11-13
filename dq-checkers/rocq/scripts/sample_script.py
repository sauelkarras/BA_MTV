#!/usr/bin/env python3
import argparse
import pandas as pd
from ucimlrepo import fetch_ucirepo

# ---------- helpers ----------

def _find_col(df, key_substr):
    key = key_substr.lower()
    for c in df.columns:
        if key in c.strip().lower():
            return c
    return None

def _get_attr(df, idx, fallback_substr=None):
    # exact AttributeN
    attr_name_variants = [f"Attribute{idx}", f"attribute{idx}", f"Attribute {idx}", f"attribute {idx}"]
    for name in attr_name_variants:
        if name in df.columns:
            return name
    # descriptive fallback
    if fallback_substr:
        cand = _find_col(df, fallback_substr)
        if cand is not None:
            return cand
    return None

def _coerce_numeric(s):
    try:
        return pd.to_numeric(s, errors="coerce")
    except Exception:
        return pd.NA

def _norm_code_series(s):
    return s.astype(str).str.strip().str.upper()

def _normalize_a9(series):
    """
    Normalize Attribute 9 (personal status & sex) to {A91..A95} when possible.
    If the dataset already uses A-codes, this is a no-op.
    If it uses text, map common patterns conservatively; unknowns stay as-is.
    """
    s = _norm_code_series(series)

    # If it already looks like Ax codes, keep.
    mask_ax = s.str.match(r"^A9[1-5]$")
    if mask_ax.all():
        return s

    # Try mapping common textual variants (very conservative).
    txt = s.str.replace("-", " ").str.replace("/", " ").str.replace("_", " ")
    txt = txt.str.replace("  ", " ", regex=False)

    def map_text_to_a9(v):
        v0 = v.lower()
        # male buckets
        if "male" in v0:
            if "single" in v0:
                return "A93"
            if "married" in v0 or "widowed" in v0:
                return "A94"
            if "divorced" in v0 or "separated" in v0:
                return "A91"
        # female buckets
        if "female" in v0:
            if "single" in v0:
                return "A95"
            if "divorced" in v0 or "separated" in v0 or "married" in v0:
                return "A92"
        # unknown → keep original; the checker treats unusable rows as missing
        return v

    mapped = txt.map(map_text_to_a9)
    return mapped

def _normalize_a20(series):
    """
    Normalize Attribute 20 (foreign worker) to {A201, A202}.
    Accepts existing A-codes, or yes/no variants.
    """
    s = _norm_code_series(series)

    # Already coded?
    mask_a201_2 = s.isin(["A201", "A202"])
    if mask_a201_2.all():
        return s

    # Map common boolean-ish encodings
    m = s.str.lower().map({
        "yes": "A201", "y": "A201", "true": "A201", "1": "A201",
        "no":  "A202", "n": "A202", "false": "A202", "0": "A202"
    })
    s = s.where(~mask_a201_2, s)          # keep A201/A202 if present
    s = m.fillna(s)                        # otherwise try mapping, else leave as-is
    return s

# ---------- main ----------

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--n", type=int, default=300, help="sample size")
    ap.add_argument("--out", default="german_credit_sample.csv",
                    help="output CSV written relative to CWD")
    # Optional overrides for column detection (useful across variants)
    ap.add_argument("--sex-col", default=None, help="header substring for Attribute 9")
    ap.add_argument("--foreign-col", default=None, help="header substring for Attribute 20")
    args = ap.parse_args()

    gc = fetch_ucirepo(id=144)  # German Credit
    X = gc.data.features.copy()
    Y = gc.data.targets.copy() if gc.data.targets is not None else pd.DataFrame()

    X.columns = X.columns.str.strip()
    if not Y.empty:
        Y.columns = Y.columns.str.strip()

    # Required features (Attributes)
    age_col = _get_attr(X, 13, "age")                    # age
    dur_col = _get_attr(X,  2, "duration")               # duration in month
    amt_col = _get_attr(X,  5, "credit amount")          # credit amount
    a1_col  = _get_attr(X,  1, "status of existing checking account")
    a3_col  = _get_attr(X,  3, "credit history")
    a6_col  = _get_attr(X,  6, "savings account")
    a10_col = _get_attr(X, 10, "other debtors")          # A101/A102/A103

    # NEW: Attribute 9 and 20 detection (robust fallbacks)
    if args.sex_col:
        a9_col = _find_col(X, args.sex_col)
    else:
        a9_col = (_get_attr(X, 9, "personal_status")
                  or _find_col(X, "personal status and sex")
                  or _find_col(X, "personal_status_and_sex")
                  or _find_col(X, "personal status"))
    if args.foreign_col:
        a20_col = _find_col(X, args.foreign_col)
    else:
        a20_col = (_get_attr(X, 20, "foreign")
                   or _find_col(X, "foreign worker")
                   or _find_col(X, "foreign_worker"))

    missing = [c for c in [age_col, dur_col, amt_col, a1_col, a3_col, a6_col, a10_col] if c is None]
    if missing:
        raise SystemExit(f"Missing required columns. Have features: {X.columns.tolist()}")

    # Label / class (Attribute21)
    y_col = None
    y_series = None
    if not Y.empty:
        for key in ["class", "risk", "creditability", "attribute21", "y"]:
            y_col = _find_col(Y, key)
            if y_col:
                y_series = Y[y_col]; break
    if y_series is None:
        for key in ["class", "risk", "creditability", "attribute21", "y"]:
            y_col = _find_col(X, key)
            if y_col:
                y_series = X[y_col]; break
    if y_series is None:
        raise SystemExit(f"Could not find label column in targets or features. "
                         f"Targets columns: {Y.columns.tolist()}  Features columns: {X.columns.tolist()}")

    # Build dataframe with our fixed schema (now includes a9 and a20)
    cols_to_take = {
        age_col: "age",
        amt_col: "balance",
        dur_col: "duration",
        a1_col:  "a1",
        a3_col:  "a3",
        a6_col:  "a6",
        a10_col: "a10",
    }
    base_df = X[list(cols_to_take.keys())].rename(columns=cols_to_take)

    # Attach a9/a20 if present; else create empty columns
    if a9_col:
        base_df["a9"] = X[a9_col]
    else:
        base_df["a9"] = ""
    if a20_col:
        base_df["a20"] = X[a20_col]
    else:
        base_df["a20"] = ""

    df = pd.concat([base_df, y_series.rename("y")], axis=1).dropna(subset=["age", "balance", "duration"])

    # Numerics
    for c in ["age", "balance", "duration"]:
        df[c] = pd.to_numeric(df[c], errors="coerce")
    df = df.dropna(subset=["age", "balance", "duration"])

    # Codes to uppercase (A11… A3x … A6x … A10x)
    for c in ["a1", "a3", "a6", "a10"]:
        df[c] = _norm_code_series(df[c])

    # Normalize a9 (A91..A95) and a20 (A201/A202) if available
    if "a9" in df.columns:
        df["a9"] = _normalize_a9(df["a9"])
    if "a20" in df.columns:
        df["a20"] = _normalize_a20(df["a20"])

    # Normalize y to "1"/"2"
    df["y"] = (
        df["y"].astype(str).str.strip().str.replace(".", "", regex=False).str.lower()
        .replace({"good": "1", "bad": "2", "GOOD": "1", "BAD": "2"})
    )
    df = df[df["y"].isin(["1", "2"])]

    # Sample & save (now with a9 and a20)
    n = max(1, min(args.n, len(df)))
    sample = df.sample(n=n, random_state=42)[
        ["age", "balance", "duration", "a1", "a3", "a6", "a9", "a10", "a20", "y"]
    ].copy()

    sample.to_csv(args.out, index=False)
    print(f"Wrote {len(sample)} rows to {args.out}")

if __name__ == "__main__":
    main()

# python3 scripts/sample_script.py --n 500 --out bank_sample_simple.csv

# cd ~/BA_MTV/dq-checkers/rocq
# make -j
# cd out
# ghc -O2 Main.hs Generated.hs -o run
# ./run ../bank_sample_simple.csv
