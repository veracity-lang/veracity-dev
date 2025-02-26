# python3 ./speedup_plot_compare.py out-dswp-arran out-dswp-nc out-dswp-noNB out-dswp-comparison-all

import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import sys
import os

def read_csv(file_path):
    try:
        return pd.read_csv(file_path, delim_whitespace=True)
    except pd.errors.ParserError:
        print(f"Error: The file {file_path} does not have the correct format.")
        sys.exit(1)

def create_comparison_plot(data_commute, data_no_commute, data_no_NB, benchmark, output_dir):
    N = data_commute.iloc[:, 0]
    log_N = np.log10(N)

    plt.figure(figsize=(6, 4))
    
    # plt.rcParams.update({'font.size': 13})
    
    plt.plot(log_N, data_commute[benchmark], label='Comm.', 
             marker='o', markersize=6, linewidth=2)
    plt.plot(log_N, data_no_commute[benchmark], label='False-Comm.', 
             marker='s', markersize=6, linewidth=2, color='red')
    plt.plot(log_N, data_no_NB[benchmark], label='No-NCB.', 
             marker='^', markersize=6, linewidth=2, color='green')

    plt.axhline(y=1, color='black', linestyle='--', 
                label='Speedup = 1', linewidth=1.6)

    # plt.xlabel('Log(Computation Size)', fontsize=12)
    # plt.ylabel('Par-to-Seq Speedup', fontsize=12)

    plt.xticks(fontsize=15)
    plt.yticks(fontsize=15)
    
    # benchmark_name = benchmark.replace('vote-run', 'Vote').title()
    # benchmark_name = benchmark_name.replace('2D-Array', 'PS-DSWP-Array')
    # plt.title(benchmark_name, fontsize=12, pad=10)
    
    plt.legend(loc='best', fontsize=14)
    plt.grid(True, linestyle=':', alpha=0.6)
    
    plt.tight_layout()

    output_file = os.path.join(output_dir, f'{benchmark}-comparison.png')
    plt.savefig(output_file, dpi=300, bbox_inches='tight', transparent=True)
    plt.close()
    print(f"Plot for {benchmark} saved at {output_file}")

def main():
    if len(sys.argv) != 5:
        print("Usage: python script.py <commute_csv_dir> <no_commute_csv_dir> <no_NB_csv_dir> <output_directory>")
        sys.exit(1)

    commute_csv = sys.argv[1]
    no_commute_csv = sys.argv[2]
    no_NB_csv = sys.argv[3]
    output_dir = sys.argv[4]

    if not os.path.exists(output_dir):
        os.makedirs(output_dir)

    data_commute = read_csv(os.path.join(commute_csv, 'ratio.csv'))
    data_no_commute = read_csv(os.path.join(no_commute_csv, 'ratio.csv'))
    data_no_NB = read_csv(os.path.join(no_NB_csv, 'ratio.csv'))

    benchmarks = data_commute.columns[1:]

    for benchmark in benchmarks:
        create_comparison_plot(data_commute, data_no_commute, data_no_NB, benchmark, output_dir)

if __name__ == "__main__":
    main()