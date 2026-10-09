---
description: "FireDAC client of MARS-Curiosity: TMARSFDResource and TMARSFDDataSetResource fetch datasets into TFDMemTables and post back only the changes (delta), applied on the server."
---

# FireDAC Client

When the server exposes [FireDAC datasets](/features/firedac), the client fetches them into live `TFDMemTable`s, lets the user edit them, and posts the changes back as a *delta*. This gives you a near-classic data-aware experience over REST. The components are `TMARSFDResource` and `TMARSFDDataSetResource` (`MARS.Client.FireDAC`, package `MARSClient.FireDAC`), on the *MARS-Curiosity Client* page of the palette (package `MARSClient.FireDACDesign`).

## Setup

Drop a `TMARSFDResource`, link it to an application, and pair each dataset of the server with a local `TFDMemTable` in `ResourceDataSets`:

```pascal
FDResource.Application := MARSApplication;
FDResource.Resource := 'customersdata';  // i.e. a TMARSFDDatasetResource on the server
// ResourceDataSets: Customers -> CustomersTable (SendDelta), Cities -> CitiesTable
```

Set the items at design time or in code. At design time, the *GET* verb of the component adds an item for each dataset of the response, and the *Create datasets* verb creates a `TFDMemTable` for each item that has none. Each item has:

| Property | Default | Meaning |
| --- | --- | --- |
| `DataSetName` | | The name of the dataset in the response. |
| `DataSet` | | The local `TFDMemTable` (its `CachedUpdates` is set when `SendDelta` is on). |
| `SendDelta` | `True` | `POST` sends the changes of this dataset. |
| `Synchronize` | `True` | Fill the dataset in the main thread. |

The component asks for `application/json-firedac`, the FireDAC binary format: data, field metadata and change tracking travel with the datasets.

## Fetching data

A `GET` fills the linked mem tables (an item is added for a dataset that has none, and removed for a dataset missing from the response):

```pascal
FDResource.GET;
// CustomersTable and CitiesTable are open, bound controls show the data
```

`GETAsync` does the same in a background thread; `Synchronize` updates the datasets in the main thread.

## Sending changes back

Edit the mem tables as usual: FireDAC tracks the changes. A `POST` sends only the **delta** of the items with `SendDelta`; the server applies it with `ApplyUpdates` and returns one result for each dataset:

```pascal
CustomersTable.Edit;
CustomersTable.FieldByName('credit').AsCurrency := 2000;
CustomersTable.Post;

FDResource.POST; // the changed record only
```

After the `POST`, `ApplyUpdatesResults` holds the results (`TMARSFDApplyUpdatesRes`: `dataset`, `result`, `errorCount`, `errors`) and `POSTResponse` the JSON of the response. When a dataset has errors, `OnApplyUpdatesError` is called; if it does not set `AHandled`, an exception is raised. Without errors the changes of the local datasets are merged (`ApplyUpdates` on the client), and the next delta starts from there.

```pascal
procedure TMainDataModule.FDResourceApplyUpdatesError(const ASender: TObject;
  const AItem: TMARSFDResourceDatasetsItem; const AErrorCount: Integer;
  const AErrors: TArray<string>; var AHandled: Boolean);
begin
  ShowMessage(AItem.DataSetName + ': ' + string.Join(sLineBreak, AErrors));
  AHandled := True;
end;
```

New records may leave an auto-incremental key empty (clear `Required` of its field): the database assigns it when the server applies the delta, the next `GET` brings it back.

## Single-dataset resource

`TMARSFDDataSetResource` binds a single `TFDMemTable` (`DataSet`) and sends `Filter` and `Sort` as the `filter` and `sort` query parameters of the `GET` (the server decides what to do with them); `SendDelta` and `Synchronize` work as above. `GET` to load, `POST` to save.

## End-to-end shape

```
[Client]  TMARSFDResource.GET  ──►  GET /rest/default/customersdata
                                     server: TMARSFDDatasetResource.Retrieve
          local TFDMemTables  ◄──    application/json-firedac (datasets)

  user edits rows (change tracking) ...

[Client]  TMARSFDResource.POST ──►  POST /rest/default/customersdata  (deltas)
                                     server: ApplyUpdates(datasets, deltas)
          ApplyUpdatesResults ◄──    [{ dataset, result, errorCount, errors }]
```

See the server side in [FireDAC & Datasets](/features/firedac) and the complete [FireDACDemo](/demos/#firedacdemo) (its FMX client loads, edits, adds and saves customers this way). The Devart libraries have equivalent components: see [Devart Client](/client/devart).
