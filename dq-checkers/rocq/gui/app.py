from flask import Flask, render_template, request
import subprocess
import shlex
import os

app = Flask(__name__)

# Resolve paths relative to this file
BASE_DIR = os.path.dirname(os.path.abspath(__file__))        # .../rocq/gui
ROCQ_ROOT = os.path.dirname(BASE_DIR)                        # .../rocq
OUT_DIR = os.path.join(ROCQ_ROOT, "out")                     # .../rocq/out
RUN_PATH = os.path.join(OUT_DIR, "run")                      # .../rocq/out/run


@app.route("/", methods=["GET", "POST"])
def index():
    stdout = ""
    stderr = ""
    cli_preview = ""
    error_msg = None

    if request.method == "POST":
        # --- read form fields ---
        dataset = (request.form.get("dataset") or "").strip()
        uci_line = (request.form.get("uci_line") or "").strip()

        ranges_text = (request.form.get("ranges") or "").strip()
        contrs_text = (request.form.get("contrs") or "").strip()
        classes_text = (request.form.get("classes") or "").strip()

        # --- basic validation of dataset vs uci_line ---
        if dataset and uci_line:
            error_msg = "Please use either a named dataset OR a UCI line, not both."
        elif not dataset and not uci_line:
            error_msg = "Please specify a dataset (or UCI line) before running the checker."

        # --- build command if no error so far ---
        if error_msg is None:
            cmd = [RUN_PATH]

            if dataset:
                cmd += ["--dataset", dataset]
            else:
                # generic UCI mode
                cmd += ["--uci-line", uci_line]

            # range specs: one per line, already in "attr,lo,hi" form
            for line in ranges_text.splitlines():
                spec = line.strip()
                if spec:
                    cmd += ["--range", spec]

            # contr specs: one per line, already in "attrX<k=>attrY!=LAB" / "attrX=LAB1=>attrY=LAB2" form
            for line in contrs_text.splitlines():
                spec = line.strip()
                if spec:
                    cmd += ["--contr", spec]

            # class specs: one per line, already in "attr,label,expected_share,tolerance" form
            for line in classes_text.splitlines():
                spec = line.strip()
                if spec:
                    cmd += ["--class", spec]

            # for display, show what you would type in the out/ directory
            display_cmd = ["./run"]
            # we omit the absolute path in the preview
            display_cmd.extend(cmd[1:])
            cli_preview = " ".join(shlex.quote(p) for p in display_cmd)

            try:
                # run in rocq/out so Main.hs finds scripts and data as usual
                proc = subprocess.run(
                    cmd,
                    cwd=OUT_DIR,
                    capture_output=True,
                    text=True,
                )
                stdout = proc.stdout or ""
                stderr = proc.stderr or ""

                # if ./run itself returned non-zero, surface that as a web-level error hint
                if proc.returncode != 0 and not error_msg:
                    error_msg = f"./run exited with code {proc.returncode}."
            except FileNotFoundError:
                error_msg = (
                    f"Could not find executable at {RUN_PATH}. "
                    "Make sure you compiled the Haskell checker (ghc -O2 Main.hs Generated.hs -o run in rocq/out)."
                )
            except Exception as e:
                error_msg = f"Unexpected error while running ./run: {e}"

    return render_template(
        "index.html",
        stdout=stdout,
        stderr=stderr,
        cli_preview=cli_preview,
        error_msg=error_msg,
    )


if __name__ == "__main__":
    # listen only on localhost; debug on for development
    app.run(debug=True)
