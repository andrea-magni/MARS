"""Extracts an excerpt of an OpenAPI document: some paths and everything they reference.

  python extract.py <document> <output> <path> [<path> ...]

The excerpt keeps openapi, info, servers, security and components/securitySchemes, the given
path items whole, and the components their $ref reach, transitively. The output is YAML when its
name ends with .yaml, JSON otherwise. Used to build the excerpts of real APIs in this folder (see
README.md): run it again on a newer version of the document to refresh them.
"""
import json
import sys

import yaml


def load(name):
    with open(name, encoding='utf-8') as source:
        if name.endswith(('.yaml', '.yml')):
            return yaml.load(source, Loader=getattr(yaml, 'CSafeLoader', yaml.SafeLoader))
        return json.load(source)


def refs(value):
    if isinstance(value, dict):
        for key, item in value.items():
            if key == '$ref' and isinstance(item, str):
                yield item
            else:
                yield from refs(item)
    elif isinstance(value, list):
        for item in value:
            yield from refs(item)


def main():
    document, output, paths = load(sys.argv[1]), sys.argv[2], sys.argv[3:]
    excerpt = {key: document[key] for key in ('openapi', 'info', 'servers', 'security') if key in document}
    excerpt['paths'] = {}
    for path in paths:
        if path not in document['paths']:
            sys.exit('path not found: ' + path)
        excerpt['paths'][path] = document['paths'][path]

    components = {}
    if 'securitySchemes' in document.get('components', {}):
        components['securitySchemes'] = document['components']['securitySchemes']
    pending = list(refs(excerpt['paths']))
    seen = set()
    while pending:
        ref = pending.pop()
        if ref in seen or not ref.startswith('#/components/'):
            continue
        seen.add(ref)
        kind, name = ref[len('#/components/'):].split('/', 1)
        name = name.replace('~1', '/').replace('~0', '~')
        target = document['components'][kind][name]
        components.setdefault(kind, {})[name] = target
        pending.extend(refs(target))
    # the order of the original document
    excerpt['components'] = {
        kind: {name: components[kind][name] for name in document['components'][kind] if name in components[kind]}
        for kind in document['components'] if kind in components}

    with open(output, 'w', encoding='utf-8', newline='\n') as target:
        if output.endswith('.yaml'):
            yaml.safe_dump(excerpt, target, sort_keys=False, allow_unicode=True, width=100)
        else:
            json.dump(excerpt, target, indent=1, ensure_ascii=False)
            target.write('\n')
    print(output, len(excerpt['paths']), 'paths,',
          sum(len(items) for items in excerpt['components'].values()), 'components')


if __name__ == '__main__':
    main()
