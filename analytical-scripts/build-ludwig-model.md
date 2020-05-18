# readme

Install ludwig with `pip install ludwig`
Install comet with `pip install comet_ml`

Get a comet account at www.comet.ml ; copy the API

At the terminal, pass your API to Comet and Ludwig with:

$ export COMET_API_KEY="..."
$ export COMET_PROJECT_NAME="..."

See https://www.comet.ml/docs/python-sdk/ludwig/ for details.

Each time you make an experiment with Ludwig, do it from a new folder so that Comet correctly associates the run. Your user page in Comet will update with the data from the run, as well as the various plots. (You do not have to run with comet; simply remove the --comet flag from the commands below.) Your data file should have a column in it called 'split' with 0 = train, 1 = validation, 2 = test.

The data we created the model from was 'operational-data.csv', a reshaping of the square-matrix-results.csv, accomplished in the file `distMatrix-MDA.R`.

Run an experiment:

```
$ ludwig experiment --comet --data_csv data.csv  --model_name Model --model_definition_file model.yaml
```

Run a visualization:

```
ludwig visualize --visualization learning_curves --training_statistics training_statistics.json
```