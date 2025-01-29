unit MARS.Tests.Types;

interface

type
  TRequestData = record
    HostName: string;
    Port: Integer;
    Path: string;
    HttpMethod: string;
    QueryString: string;
    Content: string;
    Accept: string;
    Token: string;

    ExpectedResponse: string;
  end;


implementation

end.
