import pandas as pd
import numpy as np
import datetime
import plotly.graph_objs as go
import plotly.express as px
from plotly.subplots import make_subplots


# PLOT SETTINGS: which plot do you want to see?
# list = ['rona', 'buildings', 'workplace', 'retail', 'grocery', 'parks', 'transit', 'res']
list = ['rona', 'buildings']

# read in data
df = pd.read_csv("all_data_weekly.csv")

# make building percentages percent change from baseline
df['building_percentage'] = ((df['building_percentage']) - 1) * 100;

subfig2 = make_subplots(specs=[[{"secondary_y": True}]])

# make traces
buildings = px.line(df, x="week_start", y="building_percentage", color="state")
workplace = px.line(df, x="week_start", y="workplaces_percent_change_from_baseline", color="state")
rec = px.line(df, x="week_start", y="retail_and_recreation_percent_change_from_baseline", color="state")
groc = px.line(df, x="week_start", y="grocery_and_pharmacy_percent_change_from_baseline", color="state")
park = px.line(df, x="week_start", y="parks_percent_change_from_baseline", color="state")
transit = px.line(df, x="week_start", y="transit_stations_percent_change_from_baseline", color="state")
res = px.line(df, x="week_start", y="residential_percent_change_from_baseline", color="state")

rona = px.line(df, x="week_start", y="new_cases", color="state")
rona.update_traces(yaxis="y2")

# selects which traces you want
if 'buildings' not in list:
    buildings.data = []
if 'rona' not in list:
    rona.data = []
if 'workplace' not in list:
    workplace.data = []
if 'retail' not in list:
    rec.data = []
if 'grocery' not in list:
    groc.data = []
if 'parks' not in list:
    park.data = []
if 'transit' not in list:
    transit.data = []
if 'res' not in list:
    res.data = []

subfig2.add_traces(buildings.data + rona.data + workplace.data + rec.data + groc.data + park.data + transit.data + res.data)

subfig2.layout.xaxis.title="Week"
subfig2.layout.yaxis.title="Percent Change From Baseline"
subfig2.layout.yaxis2.title="New Cases of COVID-19"
subfig2.layout.title = "Mobility and COVID-19 Cases over Time"
# recoloring is necessary otherwise lines from fig und fig2 would share each color
subfig2.for_each_trace(lambda t: t.update(line=dict(color=t.marker.color)))
subfig2.show()