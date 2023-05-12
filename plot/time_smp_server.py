from matplotlib import ticker
import numpy as np
import matplotlib.pyplot as plt
from .plot_utils import *

set_plot_options(True)

memory_usages = get_times(smp_server_versions)

fig, gs, legend_ax = get_legend_grid_spec(3.075, 2.8, 8)
inner_gs = gs[0].subgridspec(
    1,
    1,
    wspace=0,
    hspace=0,
)

ax = fig.add_subplot(inner_gs[0])
hidden_ax = fig.add_subplot(inner_gs[:], frameon=False)

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

x = np.arange(len(smp_server_versions))
width = 0.39

plot_bars(ax, memory_usages, width, x, 2, 2)

ax.yaxis.set_major_locator(ticker.MultipleLocator(10))

hidden_ax.set_xlabel("Milestone version", labelpad=20)
hidden_ax.set_ylabel("Time taken to complete\nbenchmark (s)", labelpad=20)
ax.set_xticks(x + width / 2, smp_server_releases)
ax.set_ylim(0, 68)

add_legend(ax, legend_ax)

plt.margins(0)
plt.savefig("time-smp-server.pgf")
# plt.show()
