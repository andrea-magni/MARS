# Neslib.Yaml - A YAML library for Delphi

Neslib.Yaml is a library for parsing and emitting YAML and constructing YAML documents and streams.

Neslib.Yaml is build on top of the [LibYaml](https://github.com/yaml/libyaml) library and works on:

* Windows (32-bit and 64-bit)
* MacOS (32-bit and soon 64-bit)
* iOS (32-bit and 64-bit, *no* simulator)
* Android (32-bit and 64-bit later)

## Installation and Dependencies

To install:

```shell
> git clone --recursive https://github.com/neslib/Neslib.Yaml
```

This library only depends on the Neslib repository, which is included as submodule with this repository.

For all platforms except MacOS 32-bit, there are no run-time dependencies: the LibYaml library is linked directly into the executable. For MacOS 32-bit, you need to deploy the `libyaml_mac32.dylib` library to the remote path `Contents\MacOS\`\.

## YAML in a Nutshell

Here is a very brief introduction for YAML. For more detailed information take a look at the [official YAML site](https://yaml.org/) or one of the many on-line resources such as [this one](https://camel.readthedocs.io/en/latest/yamlref.html).

YAML (short for "YAML Ain't Markup Language") is a data serialization language. Unlike many other similar text-based languages (like JSON and XML) a primary goal of YAML is to be human-readable and also easy to create by humans. That's why its is commonly used for configuration files. However, it can be used for all kinds of data, such as this example from the YAML specification:

```yaml
invoice: 34843
date   : 2001-01-23
bill-to: &id001
    given  : Chris
    family : Dumars
    address:
        lines: |
            458 Walkman Dr.
            Suite #292
        city    : Royal Oak
        state   : MI
        postal  : 48046
ship-to: *id001
product:
    - sku         : BL394D
      quantity    : 4
      description : Basketball
      price       : 450.00
    - sku         : BL4438H
      quantity    : 1
      description : Super Hoop
      price       : 2392.00
tax  : 251.42
total: 4443.52
comments: >
    Late afternoon is best.
    Backup contact is Nancy
    Billsmer @ 338-4338.
```

A YAML document is a tree of values, called nodes (`TYamlNode` in this library). There are 4 kinds of nodes:

### Mappings

Mappings are similar to Delphi dictionaries. A mapping is a collection of key/value pairs. The root note of the sample document above is a mapping: it maps the key `invoice` to the value `34843` and contains 7 other key/value pairs (from `date` to `comments`). Both keys and values can be any YAML type, although you probably want to stick to strings for keys.

Mappings can be written in block notation (as in the example) or flow notation (using curly braces `{}`).

When using block notation, YAML uses indentation for scoping. Only spaces are allowed for indentation (*not* tabs). The number of spaces doesn't matter as long as all values at the same level use the same amount of spaces. In the example, the value of the `bill-to` key is another mapping. This mapping is indented to indicate that it belongs to the `bill-to` key.

### Sequences

Sequences are like Delphi arrays or lists. Small sequences can be written using flow notation (using square brackets `[]`). Larger or complex sequences are usually written in block notation as in the example: the value of the `product` key is a sequence of two products (a basketball and super hoop). Each item in the sequence starts with a dash and a space.

In this example, each product in the sequence is a mapping of 4 key/value pairs.

### Aliases

All nodes have at least two properties: a `Tag` and an `Anchor`. Tags are used to describe the semantic type of a node. Tags are not that common, so I will skip them in this introduction. Neslib.Yaml has full support for tags though.

An **anchor** can be used to mark a node in the document. You can then later refer back to this node using an **alias**.

**Anchors** are prefixed with an ampersand (`&`). In the example, the value of the `bill-to` key has an anchor called `id001` (the ampersand is not part of the name). Later in the document, the `ship-to` key refers back to this anchor using an **alias** (an asterisk followed by the anchor name, eg. `*id001`). This is a way of saying that the shipping address is the same as the billing address. Note that an alias does **not** copy the referenced value; it really just refers to another node.

Anchors *must* appear in the document before they can be referenced. Their names *don't* have to be unique within the document; if an new anchor is declared with the same name, it replaces the old anchor.

### Scalars

Scalars are the simplest types. Everything that is not a mapping, sequence or alias is a scalar. In practice, scalars are just strings. All the keys in the example above are string scalars, but a lot of the values are as well (such as `34843`, `2001-01-23` and `Chris`). 

The YAML 1.1 specification (which is what LibYaml uses) treats all these scalars as strings, even if they are numbers or dates as in this example. You can use tags to explicitly state that a specific scalar is of a specific type.

The `TYamlNode` record in this library provides methods like `ToInteger` and `ToDouble` to (try to) convert to Delphi types, regardless of any tags that may be attached to a node.

Scalars can be written in different "styles":

* The **plain** style is the most common style. It doesn't use any special symbols. Most scalars in the example are in plain style.
* The **double-quoted** style is useful if you need escape sequences in the text.
* The **single-quoted** style can be used if backslashes in text should *not* be un-escaped (eg. when using Windows file paths).
* The **literal** style can be used for a block of text spanning multiple lines. It starts with a pipe symbol (`|`). In the example above, the `bill-to.address.lines` value is a literal. Any new-lines in a literal are preserved.
* Finally, the **folded** style is similar to the literal style, but line breaks are folded (replaced with spaces). It is used with the `comments` key in the example.

There is much more to YAML, but this should cover many use cases.

## Loading or Parsing YAML

The main entry point to this library is the `IYamlDocument` or `IYamlStream` interface. 

A YAML file can contain multiple documents. If that is the case, you should use an `IYamlStream` to load it. A stream is just a collection of documents (of type `IYamlDocument`).

Most of the time though, a YAML file contains just a single document and it is easier to start with a `IYamlDocument`. Loading a document is easy:

```Delphi
var
  Doc: IYamlDocument;
begin
  Doc := TYamlDocument.Load('invoice.yaml');
end;
```

You can load from a file or stream, or you can parse YAML text using the `TYamlDocument.Parse` method.

You can now use the `IYamlDocument.Root` property to inspect the document. This property is of type `TYamlNode`, which is the building block for all documents.

> TYamlNode is implemented as a record to keep it light-weight. All nodes are "owner" by a document. This makes memory management fully automatic: once a document goes out of scope, all its nodes will be freed automatically. This does mean though that you should not "hang on" to nodes after a document has gone out of scope. Doing so results in undefined behavior or access violations.

For example, to access the `price` of the first product in the example above, you can use the following code:

```Delphi
Price := Doc.Root.Values['product'].Nodes[0].Values['price'].ToDouble;
```

You use the `Values` property to access values by key in *mapping*. Likewise the `Nodes` property is used to access values by index in a *sequence*, and one of the `ToXXX` methods can be used to convert a *scalar* value to a Delphi datatype.

To check the type of a node, you can use the `NodeType` property or one of the `IsXXX` properties (`IsMapping`, `IsScalar` etc.).

## Constructing and Emitting YAML

You can also create a YAML document from scratch and save it to a file or convert it to YAML. To create a YAML document, use one of the `TYamlDocument.CreateXXX` methods, depending on the type of root node you need. If you want to reconstruct the example document, you would start out with a mapping and call:

```Delphi
Doc := TYamlDocument.CreateMapping;
```

You can then start to add key/value pairs"

```Delphi
Doc.Root.AddOrSetValue('invoice', 34843);
Doc.Root.AddOrSetValue('date', '2001-01-23');
```

The `AddOrSetValue` method is used to add key/value pairs to a mapping. If the node is *not* a mapping, then an `EInvalidOperation` exception will be raised.

To add a non-scalar value, use one of the other `AddOrSetXXX` methods:

```Delphi
var
  Products: TYamlNode;
begin
  Products := Doc.Root.AddOrSetSequence('product');
end;
```

This adds a sequence to the mapping with the key `product`. You can then add values to the sequence using one of the `AddXXX` methods. Again, an `EInvalidOperation` exception will be raised if the node is not a sequence. In the example, we need to add another mapping to this sequence:

```Delphi
var
  Product: TYamlNode;
begin
  Product := Products.AddMapping;
  Product.AddOrSetValue('sku', 'BL394D');
  Product.AddOrSetValue('quantity', 4);
  // etc...
end;
```

Once you have constructed your document, you can save it to a file or stream using the `Save` method, or convert it to YAML using the `ToYaml` method:

```Delphi
var
  Yaml: String;
begin
  Yaml := Doc.ToYaml;
end;
```

You can pass an optional `TYamlOutputSettings` record to customize the YAML formatting.

## More Information

There is more to Neslib.Yaml than described above. For more details you can look at the well-document `Neslib.Yaml.pas` source file. Additional usage samples can be found in the unit tests, especially in the `Tests.Neslib.Yaml.Sample.pas` file.

## License

Neslib.Yaml is licensed under the Simplified BSD License. 

See License.txt for details.