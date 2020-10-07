import pandas as pd
import datetime as dt

import dash
import dash_core_components as dcc
import dash_html_components as html
from dash.dependencies import Input, Output

import plotly
import plotly.express as px
from plotly.subplots import make_subplots

# ------------------------------------------------------------------------------
df = pd.read_csv("all_data_weekly.csv")
df['building_percentage'] = ((df['building_percentage']) - 1) * 100;
# ------------------------------------------------------------------------------

app = dash.Dash(__name__)

# ------------------------------------------------------------------------------
app.layout = html.Div([

    html.Div([
        html.Pre(children="Mobility and COVID-19",
                 style={"text-align": "center", "font-size": "100%", "color": "black"})
    ]),

    html.Div([
        dcc.Checklist(
            id='my_checklist',  # used to identify component in callback
            options=[
                # {'label': 'COVID-19 New Cases', 'value': 'rona'},
                {'label': 'Building Entry', 'value': 'buildings'},
                {'label': 'Workplace', 'value': 'workplace'},
                {'label': 'Retail and Recreation', 'value': 'retail'},
                {'label': 'Grocery', 'value': 'grocery'},
                {'label': 'Parks', 'value': 'parks'},
                {'label': 'Residential', 'value': 'res'},
                {'label': 'Transit', 'value': 'transit'}
            ],
            value=['buildings'],  # values chosen by default

            className='my_box_container',  # class of the container (div)
            # style={'display':'flex'},             # style of the container (div)

            inputClassName='my_box_input',  # class of the <input> checkbox element
            # inputStyle={'cursor':'pointer'},      # style of the <input> checkbox element

            labelClassName='my_box_label',  # class of the <label> that wraps the checkbox input and the option's label
            # labelStyle={'background':'#A5D6A7',   # style of the <label> that wraps the checkbox input and the option's label
            #             'padding':'0.5rem 1rem',
            #             'border-radius':'0.5rem'},

            # persistence='',                        # stores user's changes to dropdown in memory ( I go over this in detail in Dropdown video: https://youtu.be/UYH_dNSX1DM )
            # persistence_type='',                   # stores user's changes to dropdown in memory ( I go over this in detail in Dropdown video: https://youtu.be/UYH_dNSX1DM )
        ),
    ]),

    html.Div([
        dcc.Graph(id='the_graph')
    ]),

])


# ------------------------------------------------------------------------------
@app.callback(
    Output(component_id='the_graph', component_property='figure'),
    [Input(component_id='my_checklist', component_property='value')]
)
def update_graph(options_chosen):
    # chart = px.line(df, x="week_start", y="building_percentage", color="state")

    # fig = make_subplots(specs=[[{"secondary_y": True}]])

    # make traces
    fig = px.line(df, x="week_start", y="building_percentage", color="state")
    workplace = px.line(df, x="week_start", y="workplaces_percent_change_from_baseline", color="state")
    rec = px.line(df, x="week_start", y="retail_and_recreation_percent_change_from_baseline", color="state")
    groc = px.line(df, x="week_start", y="grocery_and_pharmacy_percent_change_from_baseline", color="state")
    park = px.line(df, x="week_start", y="parks_percent_change_from_baseline", color="state")
    transit = px.line(df, x="week_start", y="transit_stations_percent_change_from_baseline", color="state")
    res = px.line(df, x="week_start", y="residential_percent_change_from_baseline", color="state")

    # rona = px.line(df, x="week_start", y="new_cases", color="state")
    # rona.update_traces(yaxis="y2")

    # # selects which traces you want
    if 'buildings' not in options_chosen:
        fig.data = []
    if 'workplace' not in options_chosen:
        workplace.data = []
    if 'retail' not in options_chosen:
        rec.data = []
    if 'grocery' not in options_chosen:
        groc.data = []
    if 'parks' not in options_chosen:
        park.data = []
    if 'transit' not in options_chosen:
        transit.data = []
    if 'res' not in options_chosen:
        res.data = []

    fig.add_traces(
        workplace.data + rec.data + groc.data + park.data + transit.data + res.data)
    fig.for_each_trace(lambda t: t.update(line=dict(color=t.marker.color)))

    return (fig)

# //notes: how do i add corona if they won't let me use subfigs?
# i guess the question is how to use go to plot multiple traces splicable by state and then add secondary y axis?

# ------------------------------------------------------------------------------
if __name__ == '__main__':
    app.run_server(debug=True)
