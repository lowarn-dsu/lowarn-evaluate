import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib.gridspec import GridSpec
import matplotlib.ticker as ticker

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

discontinuity_offset = 0.3
top_scale = 7

mpl.rcParams.update({"text.usetex": True, "font.family": "serif", "font.size": 12})

for experiment_type in experiment_types:
    for version in versions:
        times = []
        for i in range(10):
            with open(
                f"{results_directory}/{version}-{experiment_type}-{i + 1}-time.txt"
            ) as file:
                times.append(float(file.read().rstrip()))
        times_array = np.array(times)
        averages[experiment_type].append(np.mean(times_array))
        standard_deviations[experiment_type].append(np.std(times_array))

fig = plt.figure(layout="constrained", figsize=(6, 4))
gs = GridSpec(
    3,
    1,
    height_ratios=[top_scale + discontinuity_offset, 1 + discontinuity_offset, 1],
    figure=fig,
    hspace=0.005,
)
ax1 = fig.add_subplot(gs[0, :])
ax2 = fig.add_subplot(gs[1, :])
ax3 = fig.add_subplot(gs[2, :], frameon=False)
hidden_ax = fig.add_subplot(gs[0:2, :], frameon=False)

ax2.spines.top.set_visible(False)
ax1.spines.bottom.set_visible(False)
ax2.tick_params(labeltop=False, top=False)
ax1.tick_params(labelbottom=False, bottom=False)
ax3.tick_params(
    labeltop=False,
    top=False,
    labelbottom=False,
    bottom=False,
    labelleft=False,
    left=False,
    labelright=False,
    right=False,
)
hidden_ax.tick_params(
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

for ax in (ax1, ax2):
    for i, experiment_type in enumerate(experiment_types):
        ax.bar(
            x + i * width,
            averages[experiment_type],
            width,
            yerr=np.array(standard_deviations[experiment_type]) * 2,
            capsize=4,
            label=experiment_type_labels[experiment_type],
            color=colors[experiment_type],
            edgecolor="0",
        )

    ax.yaxis.set_major_locator(ticker.MultipleLocator(1))

hidden_ax.set_xlabel("Milestone version", labelpad=26)
hidden_ax.set_ylabel("Time taken to complete benchmark (s)", labelpad=26)
ax2.set_xticks(x + width / 2, releases)
ax1.set_ylim(30 - top_scale - discontinuity_offset, 30)
ax2.set_ylim(0, 1 + discontinuity_offset)

h, l = ax2.get_legend_handles_labels()
ax3.legend(
    h,
    l,
    loc="center",
    ncol=2,
    fancybox=False,
    framealpha=1,
    edgecolor="0",
)

d = 0.6
kwargs = dict(
    marker=[(-1, -d), (1, d)],
    markersize=12,
    linestyle="none",
    color="k",
    mec="k",
    mew=1,
    clip_on=False,
)
ax1.plot([0, 1], [0, 0], transform=ax1.transAxes, **kwargs)
ax2.plot([0, 1], [1, 1], transform=ax2.transAxes, **kwargs)


plt.savefig("time.pgf")
# plt.show()
