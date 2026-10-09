# Devart Client (UniDAC, MyDAC, IBDAC)

When the server exposes datasets with a Devart integration ([UniDAC](/features/unidac), [MyDAC](/features/mydac), [IBDAC](/features/ibdac)), a Delphi client fetches them into `TVirtualTable`s (Devart's in-memory dataset), shows and edits them with the usual data-aware controls, and can send them back. The components are the same for the three libraries, with their own names:

| Library | Unit | Multi-dataset | Single dataset | Package |
| --- | --- | --- | --- | --- |
| UniDAC | `MARS.Client.UniDAC` | `TMARSUniDACResource` | `TMARSUniDACDataSetResource` | `MARSClient.UniDAC` |
| MyDAC | `MARS.Client.MyDAC` | `TMARSMyDACResource` | `TMARSMyDACDataSetResource` | `MARSClient.MyDAC` |
| IBDAC | `MARS.Client.IBDAC` | `TMARSIBDACResource` | `TMARSIBDACDataSetResource` | `MARSClient.IBDAC` |

They exchange datasets in the native format of the library (`application/json-unidac`, `-mydac`, `-ibdac`) and need the define of the library (`MARS_UNIDAC`, `MARS_MYDAC`, `MARS_IBDAC`). The `MARSClient.<Library>Design` packages register them on the *MARS-Curiosity Client* page of the palette; they work as well when created in code, which needs no design package (the demos do so).

## Multi-dataset resource

`ResourceDataSets` pairs the name of each dataset of the response with a local `TVirtualTable`:

```pascal
SummaryResource := TMARSMyDACResource.Create(Self);
SummaryResource.Application := MARSApplication;
SummaryResource.Resource := 'customers/summary';
with SummaryResource.ResourceDataSets.Add do
begin
  DataSetName := 'Customers';
  DataSet := CustomersTable;   // TVirtualTable
end;
with SummaryResource.ResourceDataSets.Add do
begin
  DataSetName := 'Cities';
  DataSet := CitiesTable;
end;

SummaryResource.GET; // CustomersTable and CitiesTable are filled and open
```

Each item has:

| Property | Default | Meaning |
| --- | --- | --- |
| `DataSetName` | | The name of the dataset in the response (the member of the JSON object). |
| `DataSet` | | The `TVirtualTable` to fill: its fields come from the response (types and sizes as on the server). |
| `SendData` | `False` | `POST` sends this dataset. |
| `Synchronize` | `True` | Fill the dataset in the main thread (`GETAsync` from another thread updates bound controls safely). |

A `GET` adds an item for a dataset of the response that has none (without a `DataSet`, to fill later) and removes the items of the datasets missing from the response.

## Sending data back

The Devart libraries have no delta of the changes: `POST` sends the **whole** datasets of the items with `SendData = True`, in the native format, and the server inserts or updates their records (see *Receiving datasets* in the page of each library: the server decides what to do with them, i.e. an upsert by key). The response of the server, if JSON, is in `POSTResponse`:

```pascal
ImportResource := TMARSMyDACResource.Create(Self);
ImportResource.Application := MARSApplication;
ImportResource.Resource := 'customers/import';
ImportResource.SpecificAccept := 'application/json'; // the server answers with a JSON object
with ImportResource.ResourceDataSets.Add do
begin
  DataSetName := 'Customers';
  DataSet := CustomersTable;
  SendData := True;
end;

ImportResource.POST;
ShowMessage((ImportResource.POSTResponse as TJSONObject).ReadIntegerValue('imported').ToString);
```

Records added on the client usually have no key (it is assigned by the database): the server inserts them, the next `GET` brings back their keys. Records deleted on the client are not in the data sent, so a server that only inserts and updates does not delete them: give the client a `DELETE` endpoint for that.

## Single-dataset resource

`TMARS…DataSetResource` binds one `TVirtualTable` (`DataSet`) to a resource that returns one dataset: `GET` fills it with the first dataset of the response, `POST` sends it when `SendData` is `True`.

```pascal
CustomerResource := TMARSIBDACDataSetResource.Create(Self);
CustomerResource.Application := MARSApplication;
CustomerResource.Resource := 'customers/1';
CustomerResource.DataSet := CustomerTable;
CustomerResource.GET;
```

## Plain JSON

Any client can read the same resources as plain JSON (`Accept: application/json`, an array of records): use [`TMARSClientResourceJSON`](/client/resources) when you need JSON objects rather than datasets, or a resource of yours with `GETAsString`.

## See also

- [Data Access](/features/data-access): the model on the server.
- [UniDACDemo](/demos/#unidacdemo), [MyDACDemo](/demos/#mydacdemo), [IBDACDemo](/demos/#ibdacdemo): the FMX client of each demo loads, edits and saves the customers with these components.
- [FireDAC Client](/client/firedac): the same with FireDAC, which has deltas.
