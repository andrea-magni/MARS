# FireDAC Client

When the server exposes [FireDAC datasets](/features/firedac), the client can fetch them into live `TFDMemTable`s, let the user edit them, and post the changes back as a *delta*. This gives you a near-classic data-aware experience over REST. The component is `TMARSFDResource` (`MARS.Client.FireDAC.pas`), installed by the `MARSClient.FireDACDesign` package.

## Setup

Drop a `TMARSFDResource`, link it to an application, and tell it which local datasets correspond to the server's:

```pascal
FDResource.Application := App1;
FDResource.Resource := 'orders';              // a TMARSFDDatasetResource on the server
// ResourceDataSets maps server dataset names to local TFDMemTables
```

`TMARSFDResource` holds a collection (`ResourceDataSets`) pairing each server-side dataset name with a local `TFDMemTable`. Set these up at design time (the component editor lists the names) or in code.

## Fetching data

A `GET` populates the linked mem-tables:

```pascal
FDResource.GET(
  nil,
  procedure (AStream: TStream)
  begin
    // OrdersMemTable and ItemsMemTable are now filled and active
    Grid1.DataSource.DataSet := OrdersMemTable;
  end,
  nil);
```

Because the server can return the compact FireDAC wire format, transfers are efficient and field metadata (types, constraints) is preserved.

## Sending changes back

Edit the mem-tables as usual (FireDAC tracks the changes). A `POST` sends only the **delta** to the server, which applies it with `ApplyUpdates` and returns a per-dataset result:

```pascal
// user edited OrdersMemTable / ItemsMemTable ...
FDResource.POST(
  nil,
  procedure (AStream: TStream)
  begin
    if FDResource.ApplyUpdatesResults.AllOK then
      ShowMessage('Saved')
    else
      ShowMessage('Some rows were rejected — see error details');
  end,
  nil);
```

The server returns an array of `TMARSFDApplyUpdatesRes` (applied count and any per-row errors per dataset), which the client exposes so you can report or reconcile failures.

## Single-dataset resource

For the common one-dataset case, `TMARSFDDataSetResource` binds a single `TFDMemTable` and exposes convenience properties like `Filter` and `Sort` (sent as query parameters) and flags controlling whether to send deltas. The usage pattern is the same: `GET` to load, `POST` to save.

## End-to-end shape

```
[Client]  TMARSFDResource.GET  ──►  GET /rest/default/orders
                                     server: TMARSFDDatasetResource.Retrieve
          local TFDMemTables  ◄──    JSON / FireDAC binary (datasets)

  user edits rows (change tracking) ...

[Client]  TMARSFDResource.POST ──►  POST /rest/default/orders  (delta)
                                     server: ApplyUpdates(datasets, deltas)
          ApplyUpdatesResults ◄──    [{ dataset, result, errorCount, errors }]
```

See the server side in [FireDAC & Datasets](/features/firedac) and the working [ConnectionPoolingProject demo](/demos/#connectionpoolingproject).
