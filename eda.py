import pandas as pd
import numpy as np
import datetime
import plotly.graph_objs as go
import plotly.express as px
from plotly.subplots import make_subplots

df = pd.read_csv("all_data_weekly.csv")

subfig = make_subplots(specs=[[{"secondary_y": True}]])

# create two independent figures with px.line each containing data from multiple columns
fig_b = px.line(df, x="week_start", y="building_percentage", color="state")
fig2_b = px.line(df, x="week_start", y="new_cases", color="state")

fig2_b.update_traces(yaxis="y2")

subfig.add_traces(fig_b.data + fig2_b.data)
subfig.layout.xaxis.title="Time"
subfig.layout.yaxis.title="Activity Percentage"
# subfig.layout.yaxis2.type="log"
subfig.layout.yaxis2.title="Daily New Cases"
# recoloring is necessary otherwise lines from fig und fig2 would share each color
# e.g. Linear-, Log- = blue; Linear+, Log+ = red... we don't want this
subfig.for_each_trace(lambda t: t.update(line=dict(color=t.marker.color)))
subfig.show()


subfig2 = make_subplots(specs=[[{"secondary_y": True}]])


# create two independent figures with px.line each containing data from multiple columns

# fig.data = []
# fig2.data = []

# // COMMENT OUT WHICH ONES YOU DONT WANT TO LOOK AT
fig = px.line(df, x="week_start", y="workplaces_percent_change_from_baseline", color="state")
fig3 = px.line(df, x="week_start", y="retail_and_recreation_percent_change_from_baseline", color="state")
fig4 = px.line(df, x="week_start", y="grocery_and_pharmacy_percent_change_from_baseline", color="state")
fig5 = px.line(df, x="week_start", y="parks_percent_change_from_baseline", color="state")
fig6 = px.line(df, x="week_start", y="transit_stations_percent_change_from_baseline", color="state")
fig7 = px.line(df, x="week_start", y="residential_percent_change_from_baseline", color="state")

fig2 = px.line(df, x="week_start", y="new_cases", color="state")
fig2.update_traces(yaxis="y2")

# fig.data = []
# fig3.data = []
# fig4.data = []
# fig5.data = []
# fig6.data = []
# fig7.data = []

subfig2.add_traces(fig.data + fig2.data + fig3.data + fig4.data + fig5.data + fig6.data + fig7.data)
# subfig.add_traces(fig.data + fig2.data)

subfig2.layout.xaxis.title="Time"
subfig2.layout.yaxis.title="Activity Percentage"
subfig2.layout.yaxis2.title="Daily New Cases"
# recoloring is necessary otherwise lines from fig und fig2 would share each color
# e.g. Linear-, Log- = blue; Linear+, Log+ = red... we don't want this
subfig2.for_each_trace(lambda t: t.update(line=dict(color=t.marker.color)))
subfig2.show()

##    INSTRUCTIONS
# - to limit what kind of mobility you look at just comment out the line (fig,fig3,fig4...fig7)
# - double click on a state to isolate

#