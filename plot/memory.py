import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib.gridspec import GridSpec
import csv

results_directory = "./data/results"

experiment_types = ["normal", "lowarn"]
versions = [1147, 1163, 1223, 1234, 1271, 1326, 1363]
releases = [0.11, 0.12, 0.13, 0.14, 0.15, 0.16, 0.17]

averages = {experiment_type: [] for experiment_type in experiment_types}
standard_deviations = {experiment_type: [] for experiment_type in experiment_types}

experiment_type_labels = {
    "normal": "Without Lowarn DSU",
    "lowarn": "With Lowarn DSU",
}

colors = {
    "normal": "#3DAF73",
    "lowarn": "#FC7BBA",
}

mpl.rcParams.update({"text.usetex": True, "font.family": "serif", "font.size": 12})

for experiment_type in experiment_types:
    for version in versions:
        memories = []
        for i in range(10):
            with open(
                f"{results_directory}/{version}-{experiment_type}-{i + 1}-memory.txt",
                newline="",
            ) as file:
                reader = csv.reader(file)
                memories.append(np.mean([int(row[1]) for row in reader]))
        memories_array = np.array(memories)
        averages[experiment_type].append(np.mean(memories_array))
        standard_deviations[experiment_type].append(np.std(memories_array))

fig = plt.figure(layout="constrained", figsize=(6, 4))
gs = GridSpec(
    2,
    1,
    height_ratios=[10, 1],
    figure=fig,
)
ax1 = fig.add_subplot(gs[0, :])
ax2 = fig.add_subplot(gs[1, :], frameon=False)

ax2.tick_params(
    labeltop=False,
    top=False,
    labelbottom=False,
    bottom=False,
    labelleft=False,
    left=False,
    labelright=False,
    right=False,
)

x = np.arange(len(versions))
width = 0.35

for i, experiment_type in enumerate(experiment_types):
    ax1.bar(
        x + i * width,
        np.array(averages[experiment_type]) / 1024,
        width,
        yerr=np.array(standard_deviations[experiment_type]) * 2 / 1024,
        capsize=4,
        label=experiment_type_labels[experiment_type],
        color=colors[experiment_type],
        edgecolor="0",
    )

ax1.set_xlabel("Milestone version")
ax1.set_ylabel("Average PSS (MiB)")
ax1.set_xticks(x + width / 2, releases)

h, l = ax1.get_legend_handles_labels()
ax2.legend(
    h,
    l,
    loc="center",
    ncol=2,
    fancybox=False,
    framealpha=1,
    edgecolor="0",
)


plt.savefig("memory.pgf")
# plt.show()
