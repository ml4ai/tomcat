"""Plugin to get metadata information into custom page templates"""

from pathlib import Path
from datasette import hookimpl, Response
import yaml
try:
    from yaml import CLoader as Loader, CDumper as Dumper
except ImportError:
    from yaml import Loader, Dumper


@hookimpl
def extra_template_vars():
    # We do the convoluted way to find the metadata.yml file below to work with
    # Docker Compose mounts.
    with open((Path(__file__).parents[1]/"metadata.yml").resolve()) as f:
        metadata = yaml.safe_load(f.read())

    return {"metadata": metadata}

def get_structured_metadata():
    # We do the convoluted way to find the metadata.yml file below to work with
    # Docker Compose mounts.
    with open((Path(__file__).parents[1]/"metadata.yml").resolve()) as f:
        metadata = yaml.safe_load(f.read())

    return Response.json(metadata["structured_metadata"])

@hookimpl
def register_routes():
    return [("/-/structured_metadata.json", get_structured_metadata)]
