import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib.gridspec import GridSpec
import csv

data_directory = "./data"
results_directory = "./data/results"

experiment_types = ["normal", "lowarn"]

experiment_type_labels = {
    "normal": "Without Lowarn DSU",
    "lowarn": "With Lowarn DSU",
}

colors = {
    "normal": "#3DAF73",
    "lowarn": "#FC7BBA",
}

xmonad_versions = [1147, 1163, 1223, 1234, 1271, 1326, 1363]
xmonad_releases = [0.11, 0.12, 0.13, 0.14, 0.15, 0.16, 0.17]

smp_server_versions = [268, 289, 300, 308, 317, 327, 338, 356]
smp_server_releases = [3.3, 3.4, 4.0, 4.1, 4.2, 4.3, 4.4, 5.0]


def set_plot_options(useTex):
    mpl.rcParams.update(
        {"text.usetex": useTex, "font.family": "serif", "font.size": 12}
    )


def get_averages_and_standard_deviations(versions, suffix, f):
    averages_and_standard_deviations = {
        experiment_type: [] for experiment_type in experiment_types
    }

    for experiment_type in experiment_types:
        for version in versions:
            xs = []
            for i in range(10):
                with open(
                    f"{results_directory}/{version}-{experiment_type}-{i}-{suffix}.txt",
                    newline="",
                ) as file:
                    xs.append(f(file))
            xs_array = np.array(xs)
            averages_and_standard_deviations[experiment_type].append(
                np.array([np.mean(xs_array), np.std(xs_array)])
            )

    return {
        experiment_type: np.array(xs)
        for experiment_type, xs in averages_and_standard_deviations.items()
    }


def get_times(versions):
    return get_averages_and_standard_deviations(
        versions, "time", lambda file: float(file.read().rstrip())
    )


def get_memory_usages(versions):
    def readCsvAverage(file):
        reader = csv.reader(file)
        return np.mean([int(row[1]) for row in reader])

    return get_averages_and_standard_deviations(versions, "memory", readCsvAverage)


def get_legend_grid_spec(x, y, ratio):
    fig = plt.figure(layout="constrained", figsize=(x, y))
    gs = GridSpec(
        2,
        1,
        height_ratios=[ratio, 1],
        figure=fig,
    )

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

    return (fig, gs, ax2)


def add_legend(ax, legendAx):
    h, l = ax.get_legend_handles_labels()
    legendAx.legend(
        h,
        l,
        loc="center",
        ncol=2,
        fancybox=False,
        framealpha=1,
        edgecolor="0",
    )


def plot_bars(ax, averages_and_standard_deviations, width, x):
    for i, experiment_type in enumerate(experiment_types):
        ax.bar(
            x + i * width,
            averages_and_standard_deviations[experiment_type][:, 0],
            width,
            yerr=averages_and_standard_deviations[experiment_type][:, 1] * 2,
            capsize=4,
            label=experiment_type_labels[experiment_type],
            color=colors[experiment_type],
            edgecolor="0",
        )
