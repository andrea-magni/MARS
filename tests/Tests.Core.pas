unit Tests.Core;

interface

uses
  Classes, SysUtils
, DUnitX.TestFramework
, MARS.Core.URL
;

type
  [TestFixture('URL')]
  TMARSCoreTest = class(TObject)
  private
  public
    [Test]
    procedure ParseBase();

    [Test]
    procedure QueryParams();

    [Test]
    procedure URLMatching();

  end;

implementation

{ TMARSCoreTest }

procedure TMARSCoreTest.ParseBase;
var
  LURL: TMARSURL;
begin
  LURL := TMARSURL.Create('http://localhost:8080/rest/default/helloworld');
  try
    Assert.IsTrue(LURL.HasPathTokens);
    Assert.IsTrue(Length(LURL.PathTokens) = 3);
    Assert.AreEqual('rest', LURL.PathTokens[0]);
    Assert.AreEqual('default', LURL.PathTokens[1]);
    Assert.AreEqual('helloworld', LURL.PathTokens[2]);
    Assert.AreEqual('localhost', LURL.HostName);
    Assert.AreEqual('http', LURL.Protocol);
    Assert.AreEqual('/rest/default/helloworld', LURL.Path);
    Assert.AreEqual('helloworld', LURL.Document);
    Assert.IsEmpty(LURL.Query);
    Assert.IsTrue(LURL.QueryTokens.Count = 0);
    Assert.IsTrue(LURL.MatchPath('/'), 'MatchPath /');
    Assert.IsTrue(LURL.MatchPath('/rest'), 'MatchPath /rest');
    Assert.IsTrue(LURL.MatchPath('/rest/'), 'MatchPath /rest/');
    Assert.IsTrue(LURL.MatchPath('/rest/default'), 'MatchPath /rest/default');
    Assert.IsTrue(LURL.MatchPath('/rest/default/'), 'MatchPath /rest/default/');
    Assert.IsTrue(LURL.MatchPath('/rest/default/helloworld'), 'MatchPath /rest/default/helloworld');
//    Assert.IsTrue(LURL.MatchPath('/rest/default/helloworld/'), 'MatchPath /rest/default/helloworld/');
    Assert.IsFalse(LURL.MatchPath('/rest/default/helloworld/Alien'));
  finally
    LURL.Free;
  end;
end;

procedure TMARSCoreTest.QueryParams;
var
  LURL: TMARSURL;
begin
  LURL := TMARSURL.Create(
    'http://localhost:8080/rest/default/helloworld?first=One&second=Two&third=Three'
  );
  try
    Assert.IsNotEmpty(LURL.Query, 'Query is empty!');
    Assert.IsTrue(LURL.QueryTokens.Count = 3, 'QueryTokens.Count');
    Assert.IsTrue(LURL.QueryTokens.ContainsKey('first'), 'QueryTokens.ContainsKey first');
    Assert.IsTrue(LURL.QueryTokens.ContainsKey('second'), 'QueryTokens.ContainsKey second');
    Assert.IsTrue(LURL.QueryTokens.ContainsKey('third'), 'QueryTokens.ContainsKey third');
    Assert.AreEqual('One', LURL.QueryTokenByName('first', False, False), 'QueryTokenByName first');
    Assert.AreEqual('Two', LURL.QueryTokenByName('second', False, False), 'QueryTokenByName second');

    Assert.AreEqual('One', LURL.QueryTokenByName('FirSt', True, False), 'QueryTokenByName FirSt');
    Assert.AreEqual('Two', LURL.QueryTokenByName('seConD', True, False), 'QueryTokenByName seConD');
  finally
    LURL.Free;
  end;

  LURL := TMARSURL.Create(
    'http://localhost:8080/rest/default/helloworld?first=This%20is%20with%20spaces'
  );
  try
    Assert.AreEqual('This is with spaces', LURL.QueryTokenByName('first', False, False), 'QueryParam with spaces');
  finally
    LURL.Free;
  end;

  LURL := TMARSURL.Create(
    'http://localhost:8080/rest/default/helloworld?first=One%2FTwo%2FThree'
  );
  try
    Assert.AreEqual('One/Two/Three', LURL.QueryTokenByName('first', False, False), 'QueryParam with slashes');
  finally
    LURL.Free;
  end;

  LURL := TMARSURL.Create(
    'http://localhost:8080/rest/default/helloworld?first=One+Two+Three'
  );
  try
    Assert.AreEqual('One+Two+Three', LURL.QueryTokenByName('first', False, False), 'QueryParam with pluses');
  finally
    LURL.Free;
  end;

  LURL := TMARSURL.Create(
    'http://localhost:8080/rest/default/helloworld?first=me%40domain.com'
  );
  try
    Assert.AreEqual('me@domain.com', LURL.QueryTokenByName('first', False, False), 'QueryParam with @ simbol');
  finally
    LURL.Free;
  end;


  LURL := TMARSURL.Create(
    'http://localhost:8080/rest/default/helloworld?first=first,second,third'
  );
  try
    Assert.AreEqual('first,second,third', LURL.QueryTokenByName('first', False, False), 'QueryParam with commas');
  finally
    LURL.Free;
  end;

  LURL := TMARSURL.Create(
    'http://localhost:8080/rest/default/helloworld?first=citt%C3%A0'
  );
  try
    Assert.AreEqual('città', LURL.QueryTokenByName('first', False, False), 'QueryParam with à');
  finally
    LURL.Free;
  end;

  LURL := TMARSURL.Create(
    'http://localhost:8080/rest/default/helloworld?first=citta-prov-stato'
  );
  try
    Assert.AreEqual('citta-prov-stato', LURL.QueryTokenByName('first', False, False), 'QueryParam with dashes');
  finally
    LURL.Free;
  end;

  LURL := TMARSURL.Create(
    'http://localhost:8080/rest/default/helloworld?first=100%E2%82%AC'
  );
  try
    Assert.AreEqual('100€', LURL.QueryTokenByName('first', False, False), 'QueryParam with € sign');
  finally
    LURL.Free;
  end;

end;

procedure TMARSCoreTest.URLMatching;
var
  LURL: TMARSURL;
begin
  LURL := TMARSURL.Create('http://localhost:8080/rest/default/helloworld');
  try
    Assert.IsTrue(LURL.MatchPath('/rest/default'), 'MatchPath /rest/default');
    Assert.IsTrue(LURL.MatchPath('/rest/Default'), 'MatchPath /rest/Default');
    Assert.IsTrue(LURL.MatchPath('/Rest/default'), 'MatchPath /rest/Default');
    Assert.IsTrue(LURL.MatchPath('/Rest/Default'), 'MatchPath /rest/Default');
    Assert.IsTrue(LURL.MatchPath('/REST/DEFAULT'), 'MatchPath /rest/default');
  finally
    LURL.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMARSCoreTest);


end.
