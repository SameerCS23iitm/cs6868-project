#!/usr/bin/env python3
"""
Read `throughput.csv` (in the same folder) and generate four plots (one per object):
- stack.png
- queue.png
- bst.png
- sortedlist.png
Each plot contains two lines: "lock-free" and "wait-free" implementations.
"""
import os
import sys

try:
    import pandas as pd
    import matplotlib.pyplot as plt
    try:
        import seaborn as sns
        sns.set_style("whitegrid")
    except Exception:
        pass
except Exception as e:
    print("Missing dependency:", e, file=sys.stderr)
    print("Install with: pip install pandas matplotlib seaborn", file=sys.stderr)
    raise


def plot_for_object(df, obj, outdir):
    df_obj = df[df['object'] == obj].copy()
    if df_obj.empty:
        return
    df_obj['implementation'] = df_obj['implementation'].astype(str).str.strip()
    df_obj = df_obj.dropna(subset=['ops_per_sec'])
    df_obj = df_obj.sort_values('threads')
    pivot = df_obj.pivot(index='threads', columns='implementation', values='ops_per_sec')

    plt.figure(figsize=(6,4))
    for impl in ['lock-free', 'wait-free']:
        if impl in pivot.columns:
            plt.plot(pivot.index, pivot[impl], marker='o', label=impl)

    plt.xlabel('Threads')
    plt.ylabel('Throughput (ops/sec)')
    plt.title(f'{obj.capitalize()} throughput')
    plt.legend()
    plt.tight_layout()
    outpath = os.path.join(outdir, f'{obj}.png')
    plt.savefig(outpath, dpi=300)
    plt.close()


def main():
    here = os.path.dirname(__file__)
    csv_path = os.path.join(here, 'throughput.csv')
    if not os.path.exists(csv_path):
        print(f'Expected {csv_path} to exist', file=sys.stderr)
        sys.exit(2)

    df = pd.read_csv(csv_path)
    # normalize column names & trim whitespace
    df.columns = [c.strip() for c in df.columns]
    df['object'] = df['object'].astype(str).str.strip()
    df['implementation'] = df['implementation'].astype(str).str.strip()
    df['threads'] = pd.to_numeric(df['threads'], errors='coerce').astype(int)
    df['ops_per_sec'] = pd.to_numeric(df['ops_per_sec'], errors='coerce')

    outdir = here
    objects = sorted(df['object'].unique())
    for obj in objects:
        plot_for_object(df, obj, outdir)

    print('Plots written to', outdir)


if __name__ == '__main__':
    main()
