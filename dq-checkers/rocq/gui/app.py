#!/usr/bin/env python3
import os
import shlex
import subprocess

from flask import Flask, render_template, request

app = Flask(__name__)

# -------------------------------------------------------------------
# Paths
# -------------------------------------------------------------------

GUI_DIR = os.path.abspath(os.path.dirname(__file__))
ROCQ_ROOT = os.path.abspath(os.path.join(GUI_DIR, ".."))
OUT_DIR = os.path.join(ROCQ_ROOT, "out")
RUN_PATH = os.path.join(OUT_DIR, "run")


# -------------------------------------------------------------------
# Helpers to extract form rows
# -------------------------------------------------------------------

def _collect_rows(form, *names):
    """Collect parallel lists from form (…[] fields) and zip to row dicts."""
    cols = [form.getlist(n) for n in names]
    max_len = max((len(c) for c in cols), default=0)
    rows = []
    for i in range(max_len):
        row = {}
        for name, col in zip(names, cols):
            key = name[:-2] if name.endswith("[]") else name
            row[key] = col[i] if i < len(col) else ""
        rows.append(row)
    return rows


def normalize_attr_index(raw: str):
    """Allow '13' or 'attr13' and return either int or None on failure."""
    s = raw.strip()
    if not s:
        return None
    if s.startswith("attr"):
        s = s[4:]
    return int(s) if s.isdigit() else None


# -------------------------------------------------------------------
# Build CLI arguments from form, with field-level errors
# -------------------------------------------------------------------

def build_cli_from_form(form):
    """
    Returns (args, field_errors, range_rows, contr_rows, class_rows, dataset_mode)
    If field_errors is non-empty, args will be None and ./run must NOT be called.
    """
    field_errors = {}

    # Dataset / UCI selection
    dataset = (form.get("dataset") or "").strip()
    uci_line = (form.get("uci_line") or "").strip()
    dataset_mode = None   # "named" or "uci"

    if dataset and uci_line:
        field_errors["dataset"] = "Choose either a named dataset OR a UCI line, not both."
    elif dataset:
        dataset_mode = "named"
    elif uci_line:
        dataset_mode = "uci"
    else:
        field_errors["dataset"] = "Please choose a dataset or provide a UCI line."

    # Collect rows for re-rendering
    range_rows = _collect_rows(form, "range_attr[]", "range_lo[]", "range_hi[]")
    contr_rows = _collect_rows(
        form,
        "contr_prem_attr[]",
        "contr_prem_op[]",
        "contr_prem_val[]",
        "contr_tgt_attr[]",
        "contr_tgt_cond[]",
        "contr_tgt_label[]",
    )
    class_rows = _collect_rows(
        form,
        "class_attr[]",
        "class_label[]",
        "class_exp[]",
        "class_tol[]",
    )

    # Ensure at least one empty row for each section so the UI looks sane
    if not range_rows:
        range_rows = [{"range_attr": "", "range_lo": "", "range_hi": ""}]
    if not contr_rows:
        contr_rows = [{
            "contr_prem_attr": "",
            "contr_prem_op": ">",
            "contr_prem_val": "",
            "contr_tgt_attr": "",
            "contr_tgt_cond": "neq",
            "contr_tgt_label": "",
        }]
    if not class_rows:
        class_rows = [{
            "class_attr": "",
            "class_label": "",
            "class_exp": "",
            "class_tol": "",
        }]

    # If dataset selection is already invalid, we stop here (but keep rows)
    if field_errors:
        return None, field_errors, range_rows, contr_rows, class_rows, dataset_mode

    # Start building CLI args
    args = [RUN_PATH]
    if dataset_mode == "named":
        args += ["--dataset", dataset]
    else:
        # UCI line passed through as-is, Haskell will parse
        args += ["--uci-line", uci_line]

    # ---------------- Range checks ----------------
    for i, row in enumerate(range_rows):
        raw_attr = (row.get("range_attr") or "").strip()
        raw_lo = (row.get("range_lo") or "").strip()
        raw_hi = (row.get("range_hi") or "").strip()

        # Completely empty row → ignore
        if not (raw_attr or raw_lo or raw_hi):
            continue

        attr_idx = normalize_attr_index(raw_attr)
        if attr_idx is None:
            field_errors[f"range-{i}-attr"] = "Attribute index must be an integer (e.g. 13)."
            continue

        # validate bounds: integer or ±inf; empty means default -inf/inf
        def check_bound(raw, which):
            s = raw.strip()
            if s == "":
                return s  # will be mapped to default later
            if s in ("inf", "+inf", "-inf"):
                return s
            if s.lstrip("-").isdigit():
                return s
            field_errors[f"range-{i}-{which}"] = "Bound must be an integer or ±inf."
            return None

        lo_checked = check_bound(raw_lo, "lo")
        hi_checked = check_bound(raw_hi, "hi")
        if lo_checked is None or hi_checked is None:
            continue

        lo = lo_checked if lo_checked != "" else "-inf"
        hi = hi_checked if hi_checked != "" else "inf"
        args += ["--range", f"{attr_idx},{lo},{hi}"]

    # ---------------- Contradiction checks ----------------
    for i, row in enumerate(contr_rows):
        p_attr_raw = (row.get("contr_prem_attr") or "").strip()
        p_op = (row.get("contr_prem_op") or "").strip()
        p_val = (row.get("contr_prem_val") or "").strip()
        t_attr_raw = (row.get("contr_tgt_attr") or "").strip()
        t_cond = (row.get("contr_tgt_cond") or "").strip()
        t_label = (row.get("contr_tgt_label") or "").strip()

        # Treat row as "unused" unless user filled any of the *value* fields.
        # Operators have defaults and should not make a row active by themselves.
        if not (p_attr_raw or p_val or t_attr_raw or t_label):
            continue  # empty row → ignore

        p_attr = normalize_attr_index(p_attr_raw)
        t_attr = normalize_attr_index(t_attr_raw)

        if p_attr is None:
            field_errors[f"contr-{i}-prem-attr"] = "Premise attr index must be an integer."
        if t_attr is None:
            field_errors[f"contr-{i}-tgt-attr"] = "Target attr index must be an integer."

        if not p_val:
            field_errors[f"contr-{i}-prem-val"] = "Premise value/label is required."
        if not t_label:
            field_errors[f"contr-{i}-tgt-label"] = "Target label is required."

        if p_attr is None or t_attr is None or not p_val or not t_label:
            continue

        # Map UI operator to CLI syntax
        if p_op not in ["<", "<=", ">", ">=", "=", "!="]:
            field_errors[f"contr-{i}-prem-op"] = "Invalid operator."
            continue

        cond_str = "!=" if t_cond == "neq" else "="
        contr_str = f"attr{p_attr}{p_op}{p_val}=>attr{t_attr}{cond_str}{t_label}"
        args += ["--contr", contr_str]

    # ---------------- Class-balance checks ----------------
    for i, row in enumerate(class_rows):
        a_raw = (row.get("class_attr") or "").strip()
        lab = (row.get("class_label") or "").strip()
        exp_raw = (row.get("class_exp") or "").strip()
        tol_raw = (row.get("class_tol") or "").strip()

        if not (a_raw or lab or exp_raw or tol_raw):
            continue  # empty row

        a_idx = normalize_attr_index(a_raw)
        if a_idx is None:
            field_errors[f"class-{i}-attr"] = "Attribute index must be an integer."
        if not lab:
            field_errors[f"class-{i}-label"] = "Label is required."

        try:
            exp_val = float(exp_raw) if exp_raw else None
        except ValueError:
            field_errors[f"class-{i}-exp"] = "Expected share must be a number in [0,1]."
            exp_val = None

        try:
            tol_val = float(tol_raw) if tol_raw else None
        except ValueError:
            field_errors[f"class-{i}-tol"] = "Tolerance must be a non-negative number."
            tol_val = None

        if a_idx is None or not lab or exp_val is None or tol_val is None:
            continue

        args += ["--class", f"{a_idx},{lab},{exp_val},{tol_val}"]

    if field_errors:
        return None, field_errors, range_rows, contr_rows, class_rows, dataset_mode

    return args, {}, range_rows, contr_rows, class_rows, dataset_mode


# -------------------------------------------------------------------
# Flask route
# -------------------------------------------------------------------

@app.route("/", methods=["GET", "POST"])
def index():
    output_cli = ""
    error_msg = None
    command_str = "./run <csv-path>"

    # Default rows for GET
    range_rows = [{
        "range_attr": "",
        "range_lo": "",
        "range_hi": "",
    }]
    contr_rows = [{
        "contr_prem_attr": "",
        "contr_prem_op": ">",
        "contr_prem_val": "",
        "contr_tgt_attr": "",
        "contr_tgt_cond": "neq",
        "contr_tgt_label": "",
    }]
    class_rows = [{
        "class_attr": "",
        "class_label": "",
        "class_exp": "",
        "class_tol": "",
    }]
    field_errors = {}
    dataset_mode = None

    if request.method == "POST":
        args, field_errors, range_rows, contr_rows, class_rows, dataset_mode = build_cli_from_form(request.form)

        # If no field-level errors, actually run ./run
        if args is not None:
            command_str = " ".join(shlex.quote(a) for a in args)
            try:
                result = subprocess.run(
                    args,
                    cwd=OUT_DIR,
                    text=True,
                    capture_output=True,
                )
                output_cli = result.stdout
                if result.stderr:
                    output_cli += ("\n" + result.stderr)

                if result.returncode != 0:
                    error_msg = f"./run exited with code {result.returncode}."
            except FileNotFoundError as e:
                error_msg = f"Could not execute ./run: {e}"
            except Exception as e:
                error_msg = f"Unexpected error while running ./run: {e}"

    return render_template(
        "index.html",
        command=command_str,
        output=output_cli,
        error=error_msg,
        field_errors=field_errors,
        range_rows=range_rows,
        contr_rows=contr_rows,
        class_rows=class_rows,
    )


if __name__ == "__main__":
    app.run(debug=True)
