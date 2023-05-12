from matplotlib import ticker
import numpy as np
import matplotlib.pyplot as plt
from .plot_utils import *

set_plot_options(True)

memory_usages = get_times(xmonad_versions)

discontinuity_offset = 0.6
top_scale = 8
top = 32

fig, gs, legend_ax = get_legend_grid_spec(3.075, 2.8, 8)
inner_gs = gs[0].subgridspec(
    2,
    1,
    height_ratios=[top_scale + discontinuity_offset, 2 + discontinuity_offset],
    wspace=0,
    hspace=0,
)

ax1 = fig.add_subplot(inner_gs[0])
ax2 = fig.add_subplot(inner_gs[1])
hidden_ax = fig.add_subplot(inner_gs[:], frameon=False)

ax1.spines.bottom.set_visible(False)
ax2.spines.top.set_visible(False)
ax1.tick_params(labelbottom=False, bottom=False)
ax2.tick_params(labeltop=False, top=False)
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

x = np.arange(len(xmonad_versions))
width = 0.35

for ax, text_offset in zip((ax1, ax2), (0.3, None)):
    plot_bars(ax, memory_usages, width, x, text_offset, 2)
    ax.yaxis.set_major_locator(ticker.MultipleLocator(2))

hidden_ax.set_xlabel("Milestone version", labelpad=20)
hidden_ax.set_ylabel("Time taken to complete\nbenchmark (s)", labelpad=20)
ax2.set_xticks(x + width / 2, xmonad_releases)
ax1.set_ylim(top - top_scale - discontinuity_offset, top)
ax2.set_ylim(0, 2 + discontinuity_offset)

add_legend(ax2, legend_ax)

d = 0.3
kwargs = dict(
    marker=[(-1, -d), (1, d)],
    markersize=6,
    linestyle="none",
    color="k",
    mec="k",
    mew=1,
    clip_on=False,
)
ax1.plot([0, 1], [0, 0], transform=ax1.transAxes, **kwargs)
ax2.plot([0, 1], [1, 1], transform=ax2.transAxes, **kwargs)

plt.margins(0)
plt.savefig("time-xmonad.pgf")
# plt.show()
