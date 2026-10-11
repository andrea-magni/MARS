unit Tests.MARSgen.Naming;

interface

uses
  System.SysUtils,
  DUnitX.TestFramework,
  MARS.Gen.Naming;

type
  [TestFixture('Naming')]
  TGenNamingTest = class
  public
    [Test]
    procedure WordsAndPascalCase;
    [Test]
    procedure SanitizeAndEscape;
    [Test]
    procedure Singular;
    [Test]
    procedure UniqueNamesAreCaseInsensitive;
  end;

implementation

{ TGenNamingTest }

procedure TGenNamingTest.WordsAndPascalCase;
begin
  Assert.AreEqual('pet,Id', string.Join(',', TGenNaming.Words('petId')));
  Assert.AreEqual('HTTP,Server', string.Join(',', TGenNaming.Words('HTTPServer')));
  Assert.AreEqual('pet,id', string.Join(',', TGenNaming.Words('pet_id')));
  Assert.AreEqual('version2,Name', string.Join(',', TGenNaming.Words('version2Name')));

  Assert.AreEqual('PetId', TGenNaming.PascalCase('petId'));
  Assert.AreEqual('PetId', TGenNaming.PascalCase('pet_id'));
  Assert.AreEqual('PetId', TGenNaming.PascalCase('pet-id'));
  Assert.AreEqual('FirstName', TGenNaming.PascalCase('first name'));
  Assert.AreEqual('Id', TGenNaming.PascalCase('@id'));
  Assert.AreEqual('HTTPServer', TGenNaming.PascalCase('HTTPServer'));
  Assert.AreEqual('GetThing', TGenNaming.PascalCase('get-thing'));
  Assert.AreEqual('', TGenNaming.PascalCase('$$'));
end;

procedure TGenNamingTest.SanitizeAndEscape;
begin
  Assert.AreEqual('_2fa', TGenNaming.Sanitize('2fa'));
  Assert.AreEqual('_', TGenNaming.Sanitize(''));
  Assert.AreEqual('ab_c', TGenNaming.Sanitize('a-b_c'));

  Assert.AreEqual('&type', TGenNaming.Escape('type'));
  Assert.AreEqual('&End', TGenNaming.Escape('End'));
  // a visibility section cannot be a field name of a record
  Assert.AreEqual('&Private', TGenNaming.Escape('Private'));
  Assert.AreEqual('Name', TGenNaming.Escape('Name'));

  Assert.IsTrue(TGenNaming.IsValidIdentifier('available'));
  Assert.IsTrue(TGenNaming.IsValidIdentifier('_x1'));
  Assert.IsFalse(TGenNaming.IsValidIdentifier('not-sure'));
  Assert.IsFalse(TGenNaming.IsValidIdentifier('1st'));
  Assert.IsFalse(TGenNaming.IsValidIdentifier(''));
end;

procedure TGenNamingTest.Singular;
begin
  Assert.AreEqual('Pet', TGenNaming.Singular('Pets'));
  Assert.AreEqual('Category', TGenNaming.Singular('Categories'));
  Assert.AreEqual('Address', TGenNaming.Singular('Addresses'));
  Assert.AreEqual('Box', TGenNaming.Singular('Boxes'));
  Assert.AreEqual('Status', TGenNaming.Singular('Status'));
  Assert.AreEqual('Class', TGenNaming.Singular('Class'));
  Assert.AreEqual('Analysis', TGenNaming.Singular('Analysis'));
end;

procedure TGenNamingTest.UniqueNamesAreCaseInsensitive;
var
  LScope: TGenNameScope;
begin
  LScope := TGenNameScope.Create;
  try
    Assert.AreEqual('TPet', LScope.Unique('TPet'));
    Assert.AreEqual('tpet2', LScope.Unique('tpet'));
    Assert.AreEqual('TPet3', LScope.Unique('TPet'));
    LScope.Reserve('TOrder');
    Assert.AreEqual('TOrder2', LScope.Unique('TOrder'));
  finally
    LScope.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TGenNamingTest);

end.
